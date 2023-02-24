{-# LANGUAGE FlexibleContexts #-}

module Data.Aeson.Option where

import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics (Generic, Rep)
import Prelude

dropLeadingUnderscore :: Options
dropLeadingUnderscore =
  defaultOptions {fieldLabelModifier = camelTo2 '_' . drop'}
  where
    drop' = \case
      ('_' : xs) -> xs
      xs -> xs

genericToJSONWithUnderscore :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Value
genericToJSONWithUnderscore = genericToJSON dropLeadingUnderscore

genericParseJSONWithUnderscore :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
genericParseJSONWithUnderscore = genericParseJSON dropLeadingUnderscore
