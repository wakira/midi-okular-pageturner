module MidiEvent where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Data.Word (Word32)
import Data.Int (Int32)
import qualified Sound.ALSA.Sequencer.Event as Event

-- | dummy pedal id when --next or --prev is not assigned; does not match any event
dummyPedalId :: Word32
dummyPedalId = maxBound

data Action = ActionNext | ActionPrev

data EventMatchingResult = DoAction Action Int32 | NoTrigger Action Int32 | OtherEvent

type PedalStates = M.Map Word32 Int32

pedalOnThreshold :: Int32
pedalOnThreshold = 64

buildEvActionFunc :: Word32 -> Word32 -> PedalStates -> Event.T -> EventMatchingResult
buildEvActionFunc nextPid prevPid ps ev =
  let pidPred pid =
        if pid == dummyPedalId
          then const False
          else (== pid)
      -- previous midi value of pedals
      nextPedalPreviousState = fromMaybe 0 $ M.lookup nextPid ps
      prevPedalPreviousState = fromMaybe 0 $ M.lookup prevPid ps
   in case Event.body ev of
        Event.CtrlEv
          Event.Controller
          (Event.Ctrl _ (Event.Parameter p) (Event.Value v))
            | pidPred nextPid p && v >= pedalOnThreshold && nextPedalPreviousState < pedalOnThreshold ->
                DoAction ActionNext v
        Event.CtrlEv
          Event.Controller
          (Event.Ctrl _ (Event.Parameter p) (Event.Value v))
            | pidPred prevPid p && v >= pedalOnThreshold && prevPedalPreviousState < pedalOnThreshold ->
                DoAction ActionPrev v
        Event.CtrlEv
          Event.Controller
          (Event.Ctrl _ (Event.Parameter p) (Event.Value v))
            | pidPred nextPid p && v < pedalOnThreshold ->
                NoTrigger ActionNext v
        Event.CtrlEv
          Event.Controller
          (Event.Ctrl _ (Event.Parameter p) (Event.Value v))
            | pidPred prevPid p && v < pedalOnThreshold ->
                NoTrigger ActionPrev v
        _ -> OtherEvent
