{-# LANGUAGE TemplateHaskell #-}
module Data.FuzzySet.Lens where

import Control.Lens
import Lib

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector         as Vector

makeLensesFor
  [ ("items"           , "_items")
  , ("matchDict"       , "_matchDict")
  , ("exactSet"        , "_exactSet")
  , ("useLevenshtein"  , "_useLevenshtein")
  , ("gramSizeLower"   , "_gramSizeLower")
  , ("gramSizeUpper"   , "_gramSizeUpper")
  ] ''FuzzySet

makeLensesFor
  [ ("vectorMagnitude" , "_vectorMagnitude")
  , ("normalizedEntry" , "_normalizedEntry")
  ] ''FuzzySetItem

