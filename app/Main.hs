{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
  ( MVar,
    forkIO,
    isEmptyMVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    swapMVar,
    takeMVar,
  )
import Control.Exception (SomeException (..), catch)
import Control.Monad (forM, forM_, forever, unless, void)
import Control.Monad.Extra (ifM)
import qualified DBus
import qualified DBus.Client as DClient
import Data.Either (isRight)
import qualified Data.Map.Strict as M
import Data.Word (Word32, Word8)
import MidiEvent
  ( Action (..),
    EventMatchingResult (..),
    PedalStates,
    buildEvActionFunc,
    dummyPedalId,
  )
import OkularCtrl
  ( OkularObject (..),
    OkularObjectSelections (..),
    askOkularCurrentDocument,
    askOkularDocumentObjectPaths,
    askOkularServices,
  )
import SimpleCmdArgs
  ( argumentWith,
    auto,
    optionalLongWith,
    simpleCmdArgs,
  )
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Sequencer.Address as Address
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Subscribe as Subscribe
import System.Exit (die)
import Text.Read (readMaybe)

data SharedVariables = SharedVariables
  { prevPedalState :: MVar PedalStates,
    selectedOkularObject :: MVar (Maybe OkularObject),
    selections :: MVar OkularObjectSelections
  }

turnPage :: DClient.Client -> Action -> OkularObject -> IO (Either DBus.MethodError DBus.MethodReturn)
turnPage client action (OkularObject service path) =
  let methodName = case action of
        ActionNext -> "slotNextPage"
        ActionPrev -> "slotPreviousPage"
   in DClient.call
        client
        (DBus.methodCall path "org.kde.okular" methodName)
          { DBus.methodCallDestination = Just service
          }

printCandidates :: [(OkularObject, String)] -> IO ()
printCandidates c = forM_ (zip ([1 ..] :: [Int]) c) $ \(i, (_, path)) ->
  putStrLn ("[" ++ show i ++ "] " ++ path)

reconfigure ::
  DClient.Client ->
  MVar (Maybe OkularObject) ->
  MVar OkularObjectSelections ->
  IO ()
reconfigure client selected selections = do
  putStrLn "Detecting opened okular documents..."
  void $ takeMVar selected
  services <- askOkularServices client
  okularObjects <-
    concat
      <$> forM
        services
        ( \s ->
            map (OkularObject s) <$> askOkularDocumentObjectPaths client s
        )
  documentPaths <- mapM (askOkularCurrentDocument client) okularObjects
  let candidates = filter ((/= "") . snd) $ zip okularObjects documentPaths
  case candidates of
    [] ->
      putStrLn "No open okular documents found! You can press Enter to redetect."
    [(objectPath, documentPath)] -> do
      putStrLn $ "Controlling " ++ documentPath
      putStrLn "You can press Enter to reset the document you want to control"
      putMVar selected $ Just objectPath
    _ -> do
      putStrLn "Multiple Okular documents open:"
      printCandidates candidates
      putStrLn "Input the number of the document you want to control and press Enter"
      -- reconfiguration continues on main thread!
      -- remember to 'putMVar selectedOkularObject' there!
      putMVar selections (OkularObjectSelections candidates)

turnPageOnTarget :: DClient.Client -> MVar (Maybe OkularObject) -> Action -> IO Bool
turnPageOnTarget dbusClient selectedOkularObject action = do
  mbTarget <- takeMVar selectedOkularObject
  case mbTarget of
    Nothing -> putStrLn "No okular document to control. Event ignored" >> pure True
    Just target -> do
      dbusCallResult <- turnPage dbusClient action target
      putMVar selectedOkularObject mbTarget
      pure $ isRight dbusCallResult

eventListenerRoutine ::
  DClient.Client ->
  MVar Bool ->
  Client.T ->
  Port.T ->
  Word32 ->
  Word32 ->
  (PedalStates -> Event.T -> EventMatchingResult) ->
  SharedVariables ->
  IO ()
eventListenerRoutine
  dbusClient
  alsaConnectionSucceeded
  remoteClient
  remotePort
  nextPagePedalId
  prevPagePedalId
  evToAction
  (SharedVariables {..}) =
    SndSeq.withDefault SndSeq.Block $ \h -> do
      clientId <- Client.getId h
      Port.withSimple
        (h :: SndSeq.T SndSeq.InputMode)
        "primary"
        (Port.caps [Port.capWrite, Port.capSubsWrite])
        Port.typeMidiGeneric
        $ \p -> do
          let destAddr = Address.Cons clientId p
          let sourceAddr = Address.Cons remoteClient remotePort
          catch
            (Subscribe.subscribe h sourceAddr destAddr False Nothing)
            (\(SomeException _) -> putMVar alsaConnectionSucceeded False)
          putMVar alsaConnectionSucceeded True

          forever $ do
            ev <- Event.input h
            pps <- readMVar prevPedalState
            case evToAction pps ev of
              NoTrigger ActionNext state -> void $ swapMVar prevPedalState (M.insert nextPagePedalId state pps)
              NoTrigger ActionPrev state -> void $ swapMVar prevPedalState (M.insert prevPagePedalId state pps)
              OtherEvent -> pure ()
              DoAction action state -> do
                void $ swapMVar prevPedalState (M.insert nextPagePedalId state pps)
                succeeded <- turnPageOnTarget dbusClient selectedOkularObject action

                unless succeeded $ do
                  reconfigure dbusClient selectedOkularObject selections
                  -- retry, just ignore failure this time
                  void $ turnPageOnTarget dbusClient selectedOkularObject action

interaction :: DClient.Client -> SharedVariables -> IO ()
interaction dbusClient (SharedVariables {..}) = do
  line <- getLine
  ifM
    (isEmptyMVar selections)
    (reconfigure dbusClient selectedOkularObject selections)
    ( do
        OkularObjectSelections candidates <- takeMVar selections
        case (readMaybe :: String -> Maybe Int) line of
          Just choice | choice >= 1 && choice <= length candidates -> do
            putStrLn $ "Controlling " ++ snd (candidates !! (choice - 1))
            putStrLn "You can press Enter to reset the document you want to control"
            putMVar selectedOkularObject (Just $ fst $ candidates !! (choice - 1))
          _ -> putStrLn "Invalid input! Try again" >> putMVar selections (OkularObjectSelections candidates)
    )

mainRoutine :: Word8 -> Word8 -> Word32 -> Word32 -> IO ()
mainRoutine remoteClientId remotePortId nextPagePedalId prevPagePedalId = do
  prevPedalState <- newMVar M.empty
  selectedOkularObject <- newMVar Nothing
  selections <- newEmptyMVar
  let sharedVars = SharedVariables prevPedalState selectedOkularObject selections

  dbusClient <- DClient.connectSession

  let evToAction = buildEvActionFunc nextPagePedalId prevPagePedalId
  let remoteClient = Client.Cons remoteClientId
  let remotePort = Port.Cons remotePortId

  reconfigure dbusClient selectedOkularObject selections

  alsaConnectionSucceeded <- newEmptyMVar

  -- event listener thread
  void $
    forkIO $
      eventListenerRoutine
        dbusClient
        alsaConnectionSucceeded
        remoteClient
        remotePort
        nextPagePedalId
        prevPagePedalId
        evToAction
        sharedVars

  ok <- takeMVar alsaConnectionSucceeded
  if ok
    then forever $ interaction dbusClient sharedVars
    else die "Cannot connect to MIDI device"

main :: IO ()
main = do
  simpleCmdArgs Nothing "desc" "Description" $
    mainRoutine
      <$> argumentWith auto "CLIENT_ID"
      <*> argumentWith auto "PORT_ID"
      <*> optionalLongWith auto "next" "PEDAL_ID" "pedal to trigger next page" dummyPedalId
      <*> optionalLongWith auto "prev" "PEDAL_ID" "pedal to trigger previous page" dummyPedalId
