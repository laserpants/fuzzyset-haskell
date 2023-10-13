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
  ( FuzzySetMonad
  , FuzzySetT (..)
  , add
  , addMany
  , isEmpty
  , find
  , findClosest
  , findMin
  , findClosestMin
  , runDefaultFuzzySetT   
  , runFuzzySetT
  , size
  , values
  )
