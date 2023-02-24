module Maelstrom.IO where

import Data.Aeson
import Data.ByteString (toStrict)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

log :: Text -> IO ()
log = hPutStrLn stderr . unpack

jsonToText :: ToJSON a => a -> Text
jsonToText = decodeUtf8 . toStrict . encode

jsonToString :: ToJSON a => a -> String
jsonToString = unpack . jsonToText

reply :: ToJSON a => a -> IO ()
reply r = do
  putStrLn $ jsonToString r
  hFlush stdout
