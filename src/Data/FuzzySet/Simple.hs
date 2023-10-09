module Data.FuzzySet.Simple
  ( FuzzySet
  , FuzzyMatch
  , emptySet
  , defaultSet
  , minMatch
  , minMatchClosest
  , match
  , matchClosest
  , addToSet
  , add
  , addManyToSet
  , addMany
  , fromList
  , values
  , size
  , isEmpty
  , (>+<)
  )
where

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
  , match
  , matchClosest
  , minMatch
  , minMatchClosest
  , size
  , values
  , (>+<)
  )
