{-# LANGUAGE DuplicateRecordFields #-}

module Lib (echoServer) where

import Prelude hiding (getLine, log)

import Control.Monad (forever)
import Data.Aeson
import Data.Aeson.Option
import Data.ByteString (getLine)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Maelstrom.IO

data MessageType = Init
  deriving (Show)

toMessageType :: Text -> Maybe MessageType
toMessageType = \case
  "init" -> Just Init
  _ -> Nothing

fromMessageType :: MessageType -> Value
fromMessageType = \case
  Init -> "init"

instance ToJSON MessageType where
  toJSON = fromMessageType

instance FromJSON MessageType where
  parseJSON = \case
    String s -> case toMessageType s of
      Just m -> pure m
      Nothing -> fail "Failed to parse string as message type"
    _ -> fail "Failed to parse as message type, expected string"

data MessageBody = MessageBody
  { _msgId :: Int
  , _nodeId :: Text
  , _nodeIds :: [Text]
  , _type :: MessageType
  }
  deriving (Show, Generic)

instance ToJSON MessageBody where
  toJSON = genericToJSONWithUnderscore

instance FromJSON MessageBody where
  parseJSON = genericParseJSONWithUnderscore

data Message = Message
  { _src :: Text
  , _dest :: Text
  , _body :: MessageBody
  }
  deriving (Show, Generic)

instance ToJSON Message where
  toJSON = genericToJSONWithUnderscore

instance FromJSON Message where
  parseJSON = genericParseJSONWithUnderscore

data ResponseType = InitOk
  deriving (Show)

toResponseType :: Text -> Maybe ResponseType
toResponseType = \case
  "init_ok" -> Just InitOk
  _ -> Nothing

fromResponseType :: ResponseType -> Value
fromResponseType = \case
  InitOk -> "init_ok"

instance ToJSON ResponseType where
  toJSON = fromResponseType

instance FromJSON ResponseType where
  parseJSON = \case
    String s -> case toResponseType s of
      Just rt -> pure rt
      Nothing -> fail "Unable to parse string as response type"
    _ -> fail "Unable to parse reponse type, expected string"

data ResponseBody = ResponseBody
  { _msgId :: Int
  , _inReplyTo :: Int
  , _type :: ResponseType
  }
  deriving (Show, Generic)

instance ToJSON ResponseBody where
  toJSON = genericToJSONWithUnderscore

instance FromJSON ResponseBody where
  parseJSON = genericParseJSONWithUnderscore

data Response = Response
  { _src :: Text
  , _dest :: Text
  , _body :: ResponseBody
  }
  deriving (Show, Generic)

instance ToJSON Response where
  toJSON = genericToJSONWithUnderscore

instance FromJSON Response where
  parseJSON = genericParseJSONWithUnderscore

echoServer :: IO ()
echoServer = do
  log "Starting server..."
  forever do
    line <- getLine
    log $ "Received " <> decodeUtf8 line
    let eMessage = eitherDecodeStrict @Message line
    case eMessage of
      Left err ->
        log $ "Decoding error " <> pack err
      Right message -> do
        let response =
              Response message._dest message._src $
                ResponseBody 1 message._body._msgId InitOk
        log $ "Sending " <> jsonToText response
        reply response
