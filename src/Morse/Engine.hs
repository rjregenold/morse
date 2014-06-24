module Morse.Engine
  ( ProcessType(..)
  , toMorse
  , fromMorse
  , detectProcessType
  ) where

import Morse.Prelude (bool)

import Data.Char (toLower)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)


unmatchable :: Char
unmatchable = '?'

data ProcessType
  = PTEncode
  | PTDecode
  deriving (Eq)

morse :: [(Char, String)]
morse = 
  [ ('a', "._")
  , ('b', "_...")
  , ('c', "_._.")
  , ('d', "_..")
  , ('e', ".")
  , ('f', ".._.")
  , ('g', "__.")
  , ('h', "....")
  , ('i', "..")
  , ('j', ".___")
  , ('k', "_._")
  , ('l', "._..")
  , ('m', "__")
  , ('n', "_.")
  , ('o', "___")
  , ('p', ".__.")
  , ('q', "__._")
  , ('r', "._.")
  , ('s', "...")
  , ('t', "_")
  , ('u', ".._")
  , ('v', "..._")
  , ('w', ".__")
  , ('x', "_.._")
  , ('y', "_.__")
  , ('z', "__..")
  , ('1', ".____")
  , ('2', "..___")
  , ('3', "...__")
  , ('4', "...._")
  , ('5', ".....")
  , ('6', "_....")
  , ('7', "__...")
  , ('8', "___..")
  , ('9', "____.")
  , ('0', "_____")
  , (' ', "")
  ]

toMorse :: String -> String
toMorse = intercalate " " . map (fromMaybe [unmatchable] . flip lookup morse . toLower)

fromMorse :: String -> String
fromMorse = map (fromMaybe unmatchable . flip lookup inverseMorse) . (splitOn " ")
  where 
    inverseMorse = map swap morse

detectProcessType :: String -> ProcessType
detectProcessType = bool PTDecode PTEncode . all (flip elem [' ', '.', '_', unmatchable])