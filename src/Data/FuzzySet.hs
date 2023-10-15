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
  (
    -- * Alternative API
    --
    -- | This

    -- * How to use this library
    -- $howto

    FuzzySet
  , FuzzySetMonad
  , runFuzzySet
  , runDefaultFuzzySet

    -- * Monad transformer
  , FuzzySetT
  , runFuzzySetT
  , runDefaultFuzzySetT

    -- * Insertion
  , add
  , add_
  , addMany
  , addMany_

    -- * Lookup
  , find
  , findMin
  , findOne
  , findOneMin
  , closestMatchMin
  , closestMatch

    -- * Inspection
  , values
  , size
  , isEmpty
  ) where

import Data.FuzzySet.Monad
  ( FuzzySetMonad
  , FuzzySetT(..)
  , FuzzySet
  , runDefaultFuzzySetT
  , runFuzzySetT
  , runFuzzySet
  , runDefaultFuzzySet
  , add
  , add_
  , addMany
  , addMany_
  , find
  , findOne
  , findOneMin
  , closestMatch
  , findMin
  , closestMatchMin
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
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Data.Text (Text)
-- > import Data.FuzzySet
-- >
-- > findMovie :: Text -> FuzzySet (Maybe Text)
-- > findMovie title = do
-- >   add_ "Jurassic Park"
-- >   add_ "Terminator"
-- >   add_ "The Matrix"
-- >   closestMatch title
-- >
-- > main :: IO ()
-- > main = do
-- >   let result = runDefaultFuzzySet (findMovie "percolator")
-- >   print result
--
-- The output of this program is:
--
-- > Just "Terminator"
--
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Control.Monad.Trans.Class (lift)
-- > import Data.Text (Text)
-- > import Data.FuzzySet
-- >
-- > findMovie :: Text -> FuzzySetT IO (Maybe Text)
-- > findMovie = closestMatch
-- >
-- > prog :: FuzzySetT IO ()
-- > prog = do
-- >   add_ "Jurassic Park"
-- >   add_ "Terminator"
-- >   add_ "The Matrix"
-- >   result <- findMovie "percolator"
-- >   lift (print result)
-- >
-- > main :: IO ()
-- > main = runDefaultFuzzySetT prog
--
-- The output of this program is:
--
-- > Just "Terminator"
--
--
-- > findMovie :: Text -> FuzzySetT IO (Maybe Text)
-- > findMovie = closestMatchMin 0.8
--
-- The output is now:
--
-- > Nothing
--
