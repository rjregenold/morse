{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Morse.Web.Json where

import Data.Aeson
import Data.Text (Text)


data MorseRes = MorseRes
  { morseResInput  :: Text
  , morseResResult :: Text
  }

instance ToJSON MorseRes where
  toJSON MorseRes{..} = object
    [ "input"  .= morseResInput
    , "result" .= morseResResult
    ]

data MsgRes = MsgRes Text

instance ToJSON MsgRes where
  toJSON (MsgRes msg) = object
    [ "msg" .= msg
    ]