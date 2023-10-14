-- |
--
-- Module      : Data.FuzzySet
-- Copyright   : (c) 2017-present Heikki Johannes Hildén
-- License     : BSD3
-- Maintainer  : hildenjohannes@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Data.FuzzySet
  ( add
  , findMin
  , values
  , addMany
  , find
  , closestMatchMin
  , closestMatch
  , size
  , isEmpty
  , FuzzySetT(..)
  , runFuzzySetT
  , runDefaultFuzzySetT
  , FuzzySetMonad
  ) where

import Data.FuzzySet.Monad
  (
    -- * A note about ??
    --
    -- | This module ...

    -- * How to use this library
    -- $howto

    -- * Types
    FuzzySetMonad
  , FuzzySetT(..)

    -- * Monad transformer
  , runDefaultFuzzySetT
  , runFuzzySetT

    -- * Insertion
  , add
  , addMany

    -- * Lookup
  , find
  , closestMatch
  , findMin
  , closestMatchMin

    -- * Inspection
  , isEmpty
  , size
  , values
  )

-- $howto
--
-- Make sure the @OverloadedStrings@ pragma is enabled and import the module:
--
-- > import Data.FuzzySet
--
--
