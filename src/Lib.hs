{-# LANGUAGE DuplicateRecordFields #-}

module Lib (echoServer) where

import Prelude hiding (getLine)

import Control.Monad (forever)
import Data.Aeson
import Data.ByteString (getLine, toStrict)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

dropN :: Int -> Options
dropN n = defaultOptions {fieldLabelModifier = camelTo2 '_' . drop n}

enum :: Options
enum = defaultOptions {sumEncoding = ObjectWithSingleField}

printErr :: Show a => a -> IO ()
printErr = hPutStrLn stderr . show

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
  toJSON = genericToJSON (dropN 1)

instance FromJSON MessageBody where
  parseJSON = genericParseJSON (dropN 1)

data Message = Message
  { _id :: Maybe Int
  , _src :: Text
  , _dest :: Text
  , _body :: MessageBody
  }
  deriving (Show, Generic)

instance ToJSON Message where
  toJSON = genericToJSON (dropN 1)

instance FromJSON Message where
  parseJSON = genericParseJSON (dropN 1)

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
  toJSON = genericToJSON (dropN 1)

instance FromJSON ResponseBody where
  parseJSON = genericParseJSON (dropN 1)

data Response = Response
  { _src :: Text
  , _dest :: Text
  , _body :: ResponseBody
  }
  deriving (Show, Generic)

instance ToJSON Response where
  toJSON = genericToJSON (dropN 1)

instance FromJSON Response where
  parseJSON = genericParseJSON (dropN 1)

jsonToString :: ToJSON a => a -> String
jsonToString = unpack . decodeUtf8 . toStrict . encode

reply :: ToJSON a => a -> IO ()
reply r = do
  putStrLn $ jsonToString r
  hFlush stdout

echoServer :: IO ()
echoServer =
  printErr
    "Starting server..."
    forever
    $ do
      line <- getLine
      printErr $ "Received " <> line
      let eMessage = eitherDecodeStrict @Message line
      case eMessage of
        Left err ->
          printErr $ "Decoding error " <> err
        Right message -> do
          let response =
                Response message._dest message._src $
                  ResponseBody 1 message._body._msgId InitOk
          printErr $ "Sending " <> jsonToString response
          reply response
