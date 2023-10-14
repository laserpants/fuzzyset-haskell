-- |
--
-- Module      : Data.FuzzySet.Simple
-- Copyright   : (c) 2017-present Heikki Johannes Hildén
-- License     : BSD3
-- Maintainer  : hildenjohannes@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Data.FuzzySet.Simple
  ( FuzzySet
  , FuzzyMatch
  , emptySet
  , defaultSet
  , findMin
  , findClosestMin
  , find
  , findClosest
  , addToSet
  , add
  , addManyToSet
  , addMany
  , fromList
  , values
  , size
  , isEmpty
  , (>+<)
  ) where

import Data.FuzzySet.Internal
  ( FuzzyMatch
  , FuzzySet
  , add
  , addMany
  , addManyToSet
  , addToSet
  , defaultSet
  , emptySet
  , fromList
  , isEmpty
  , find
  , findClosest
  , findMin
  , findClosestMin
  , size
  , values
  , (>+<)
  )
