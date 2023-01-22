{-# LANGUAGE OverloadedStrings #-}

module OkularCtrl where

import qualified DBus
import qualified DBus.Client as DClient
import Data.List (isPrefixOf)
import qualified Text.XML.Light as X

data OkularObject = OkularObject
  { okularObjectDBusDestination :: DBus.BusName,
    okularObjectObjectPath :: DBus.ObjectPath
  }
  deriving (Show)

newtype OkularObjectSelections = OkularObjectSelections [(OkularObject, FilePath)]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

askOkularCurrentDocument :: DClient.Client -> OkularObject -> IO String
askOkularCurrentDocument client (OkularObject service path) = do
  reply <-
    DClient.call_
      client
      (DBus.methodCall path "org.kde.okular" "currentDocument")
        { DBus.methodCallDestination = Just service
        }
  case DBus.fromVariant (head $ DBus.methodReturnBody reply) of
    Nothing -> error "Cannot get current document"
    Just s -> pure s

askOkularServices :: DClient.Client -> IO [DBus.BusName]
askOkularServices client = do
  reply <-
    DClient.call_
      client
      (DBus.methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
        { DBus.methodCallDestination = Just "org.freedesktop.DBus"
        }
  let services = case DBus.fromVariant (head $ DBus.methodReturnBody reply) of
        Nothing -> error "Cannot list service names"
        Just s -> s :: [String]
  mapM DBus.parseBusName $ filter ("org.kde.okular" `isPrefixOf`) services

findOkularDocumentObjectPaths :: X.Element -> Maybe [DBus.ObjectPath]
findOkularDocumentObjectPaths x =
  let getElem (X.Elem e) = Just e
      getElem _ = Nothing

      isNode (X.Elem e) = (X.qName . X.elName) e == "node"
      isNode _ = False

      isOkularDocument s = s /= "okularshell" && "okular" `isPrefixOf` s

      topLevelNodes = do
        let nodes = filter isNode $ X.elContent x
        nodeElems <- mapM getElem nodes
        attribs <- mapM (safeHead . X.elAttribs) nodeElems
        pure $ map X.attrVal attribs :: Maybe [String]
   in (mapM (DBus.parseObjectPath . ('/' :)) . filter isOkularDocument) =<< topLevelNodes

askOkularDocumentObjectPaths :: DClient.Client -> DBus.BusName -> IO [DBus.ObjectPath]
askOkularDocumentObjectPaths client busName = do
  reply <-
    DClient.call_
      client
      (DBus.methodCall "/" "org.freedesktop.DBus.Introspectable" "Introspect")
        { DBus.methodCallDestination = Just busName
        }
  let result = do
        replyStr <- DBus.fromVariant =<< safeHead (DBus.methodReturnBody reply) :: Maybe String
        xmlDoc <- X.parseXMLDoc replyStr
        findOkularDocumentObjectPaths xmlDoc
  case result of
    Nothing -> error "Cannot get okular object paths from Introspect"
    Just p -> pure p
