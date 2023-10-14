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
  , findClosestMin
  , findClosest
  , size
  , isEmpty
  , FuzzySetT (..)
  , runFuzzySetT
  , runDefaultFuzzySetT
  , FuzzySetMonad
  ) where

import Data.FuzzySet.Monad
  (
    -- * How to use this library
    -- $howto

    -- * Types
    FuzzySetMonad
  , FuzzySetT (..)

    -- * Monad transformer
  , runDefaultFuzzySetT
  , runFuzzySetT

    -- * Insertion
  , add
  , addMany

    -- * Lookup
  , find
  , findClosest
  , findMin
  , findClosestMin

    -- * Inspection
  , isEmpty
  , size
  , values
  )

-- $howto
--
-- @todo
--
