{-# LANGUAGE TemplateHaskell #-}
module Data.FuzzySet.Lens
  ( module Control.Lens
  , _items
  , _matchDict
  , _exactSet
  , _useLevenshtein
  , _gramSizeLower
  , _gramSizeUpper
  , _vectorMagnitude
  , _normalizedEntry
  ) where

import Control.Lens
import Data.FuzzySet.Types

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
