module Morse.Prelude
  ( bool
  ) where

bool :: a -> a -> Bool -> a
bool t _ True = t
bool _ f False = f