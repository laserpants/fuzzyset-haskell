module Data.FuzzySet
  ( add
  , minMatch
  , values
  , addMany
  , match
  , minMatchClosest
  , matchClosest
  , size
  , isEmpty
  , FuzzySetT (..)
  , runFuzzySetT
  , runDefaultFuzzySetT
  , FuzzySetMonad
  )
where

import Data.FuzzySet.Monad
  ( FuzzySetMonad
  , FuzzySetT (..)
  , add                   -- add
  , addMany               -- addMany
  , isEmpty               -- isEmpty
  , match                 -- find
  , matchClosest          -- findClosest
  , minMatch              -- findMin
  , minMatchClosest       -- findClosestMin
  , runDefaultFuzzySetT   
  , runFuzzySetT
  , size                  -- size
  , values                -- values
  )
