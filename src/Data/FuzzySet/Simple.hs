{-# LANGUAGE RecordWildCards #-}

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
  (
    -- * A note about the Simple API
    --
    -- | This module exposes a /pure/, simpler API for working with fuzzy sets.
    --   If you anticipate using the fuzzy search functionality in multiple
    --   places of your application, consider using the default Monadic
    --   interface in 'Data.FuzzySet'.

    -- * How to use this module
    -- $howto

    -- * Types
    FuzzySet
  , FuzzyMatch

    -- * Initialization
  , emptySet
  , defaultSet
  , fromList

    -- * Insertion
  , addToSet
  , add
  , addManyToSet
  , addMany
  , (>+<)

    -- * Lookup
  , findMin
  , closestMatchMin
  , find
  , closestMatch

    -- * Inspection
  , values
  , size
  , isEmpty
  ) where

import Data.FuzzySet.Internal
  ( FuzzySet(..)
  , FuzzyMatch
  , addMany_
  , add_
  , getMatches
  )

import Control.Monad.State (runState)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (elems, lookup)
import Data.FuzzySet.Utils (safeHead, (<$$>), (<$$$>))
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Foldable as Foldable
import Prelude hiding (lookup)

-- $howto
--
-- Make sure the @OverloadedStrings@ pragma is enabled and import the module:
--
-- > import Data.FuzzySet.Simple
--
-- After that, three steps are typically involved:
--
--   1. Create a set using one of 'defaultSet', 'emptySet', or 'fromList'.
--   2. To add entries, use 'add', 'addToSet', or 'addMany'.
--   3. Query the set with 'find', 'closestMatch', 'findMin', or 'closestMatchMin'.
--
-- >>> closestMatch "percolator" (defaultSet >+< "Jurassic Park" >+< "Terminator" >+< "The Matrix")
-- Just "Terminator"
--
-- >>> find "Shaggy Jones" (defaultSet >+< "Shaggy Rogers" >+< "Fred Jones" >+< "Daphne Blake" >+< "Velma Dinkley")
-- [(0.7692307692307693,"Shaggy Rogers"),(0.5,"Fred Jones")]
--
-- There are also a few functions to inspect a set: 'size', 'isEmpty', and 'values'.
--
-- == More examples
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Data.FuzzySet.Simple
-- >
-- > states = [ "Alabama"        , "Alaska"         , "American Samoa"            , "Arizona"       , "Arkansas"
-- >          , "California"     , "Colorado"       , "Connecticut"               , "Delaware"      , "District of Columbia"
-- >          , "Florida"        , "Georgia"        , "Guam"                      , "Hawaii"        , "Idaho"
-- >          , "Illinois"       , "Indiana"        , "Iowa"                      , "Kansas"        , "Kentucky"
-- >          , "Louisiana"      , "Maine"          , "Maryland"                  , "Massachusetts" , "Michigan"
-- >          , "Minnesota"      , "Mississippi"    , "Missouri"                  , "Montana"       , "Nebraska"
-- >          , "Nevada"         , "New Hampshire"  , "New Jersey"                , "New Mexico"    , "New York"
-- >          , "North Carolina" , "North Dakota"   , "Northern Marianas Islands" , "Ohio"          , "Oklahoma"
-- >          , "Oregon"         , "Pennsylvania"   , "Puerto Rico"               , "Rhode Island"  , "South Carolina"
-- >          , "South Dakota"   , "Tennessee"      , "Texas"                     , "Utah"          , "Vermont"
-- >          , "Virginia"       , "Virgin Islands" , "Washington"                , "West Virginia" , "Wisconsin"
-- >          , "Wyoming" ]
-- >
-- > statesSet = fromList states
-- >
-- > main = mapM_ print (find "Burger Islands" statesSet)
--
-- The output of this program is:
--
-- > (0.7142857142857143,"Virgin Islands")
-- > (0.5714285714285714,"Rhode Island")
-- > (0.44,"Northern Marianas Islands")
-- > (0.35714285714285715,"Maryland")
--
--  Using the definition of @statesSet@ from previous example:
--
-- > >>> find "Why-oh-me-ing" statesSet
-- > [(0.5384615384615384,"Wyoming")]
--
-- > >>> find "Connect a cat" statesSet
-- > [(0.6923076923076923,"Connecticut")]
--
-- > >>> find "Transylvania" statesSet
-- > [(0.75,"Pennsylvania"),(0.3333333333333333,"California"),(0.3333333333333333,"Arkansas"),(0.3333333333333333,"Kansas")]
--
-- > >>> find "CanOfSauce" statesSet
-- > [(0.4,"Kansas")]
--
-- > >>> find "Alaska" statesSet
-- > [(1.0,"Alaska")]
--
-- > >>> find "Alaskanbraskansas" statesSet
-- > [(0.47058823529411764,"Arkansas"),(0.35294117647058826,"Kansas"),(0.35294117647058826,"Alaska"),(0.35294117647058826,"Alabama"),(0.35294117647058826,"Nebraska")]

-- | Initialize an empty 'FuzzySet'.
emptySet
  :: Int
  -- ^ Lower bound on gram sizes to use (inclusive)
  -> Int
  -- ^ Upper bound on gram sizes to use (inclusive)
  -> Bool
  -- ^ Whether or not to use the [Levenshtein distance](https://people.cs.pitt.edu/~kirk/cs1501/Pruhs/Spring2006/assignments/editdistance/Levenshtein%20Distance.htm)
  -- to determine the score
  -> FuzzySet
  -- ^ An empty fuzzy string set
emptySet = FuzzySet mempty mempty mempty

-- | An empty 'FuzzySet' with the following defaults:
--
--   * Gram size lower: @2@
--   * Gram size upper: @3@
--   * Use Levenshtein distance: @True@
defaultSet :: FuzzySet
defaultSet = emptySet 2 3 True

-- | Try to match a string against the entries in the set, and return a list of
--   all results with a score greater than or equal to the specified minimum
--   score (i.e., the first argument). The results are ordered by similarity,
--   with the closest match first.
findMin
  :: Double
  -- ^ A minimum score
  -> Text
  -- ^ The string to search for
  -> FuzzySet
  -- ^ The fuzzy string set to compare the string against
  -> [FuzzyMatch]
  -- ^ A list of results (score and matched value)
findMin minScore str FuzzySet{..} =
  case key `lookup` exactSet of
    Just exactMatch ->
      [(1, exactMatch)]
    Nothing ->
      [gramSizeUpper, gramSizeUpper - 1 .. gramSizeLower]
        & fmap (getMatches FuzzySet{..} key minScore)
        & Foldable.find (not . null)
        & fromMaybe []
  where
    key = Text.toLower str

-- | Try to match the given string against the entries in the set using the
--   specified minimum score and return the closest match, if one is found.
closestMatchMin
  :: Double
  -- ^ A minimum score
  -> Text
  -- ^ The string to search for
  -> FuzzySet
  -- ^ The fuzzy string set to compare the string against
  -> Maybe Text
  -- ^ The closest match, if one is found
closestMatchMin = fmap snd . safeHead <$$$> findMin

-- | Try to match the given string against the entries in the set, using a
--   minimum score of 0.33. Return a list of results ordered by similarity
--   score, with the closest match first. Use 'findMin' if you need to specify
--   a custom threshold value.
find
  :: Text
  -- ^ The string to search for
  -> FuzzySet
  -- ^ The fuzzy string set to compare the string against
  -> [FuzzyMatch]
  -- ^ A list of results (score and matched value)
find = findMin 0.33

-- | Try to match the given string against the entries in the set, and return
--   the closest match, if one is found. A minimum score of 0.33 is used. To
--   specify a custom threshold value, instead use 'closestMatchMin'.
closestMatch
  :: Text
  -- ^ The string to search for
  -> FuzzySet
  -- ^ The fuzzy string set to compare the string against
  -> Maybe Text
  -- ^ The closest match, if one is found
closestMatch = closestMatchMin 0.33

-- | Add a string to the set, unless it is already present. A pair is returned
--   consisting of a boolean which denotes whether or not anything was inserted,
--   and the updated set.
addToSet
  :: Text
  -- ^ The new entry
  -> FuzzySet
  -- ^ Fuzzy string set to add the entry to
  -> (Bool, FuzzySet)
  -- ^ A flag to indicate if the value was added (i.e., did not already exist
  --   in the set), and the updated set.
addToSet = runState . add_

-- | Add a string to the set, or do nothing if a key that matches the string
--   already exists.
add
  :: Text
  -- ^ The new entry
  -> FuzzySet
  -- ^ Set to add the string to
  -> FuzzySet
  -- ^ An updated set
add = snd <$$> addToSet

-- | Infix operator to add entries to a 'FuzzySet', defined as @flip add@.
(>+<)
  :: FuzzySet
  -- ^ Set to add the string to
  -> Text
  -- ^ The new entry
  -> FuzzySet
  -- ^ An updated set
(>+<) = flip add

infixl 4 >+<

-- | Add a list of strings to the set, all at once.
--
-- Unless you need to know the subset of values that were actually inserted,
-- use 'addMany' instead.
addManyToSet
  :: [Text]
  -- ^ A list of strings to add to the set
  -> FuzzySet
  -- ^ The set to add the strings to
  -> ([Text], FuzzySet)
  -- ^ A pair where the first component is a list of values that were inserted,
  --   and the second is the updated set.
addManyToSet = runState . addMany_

-- | Add a list of strings to the set, all at once.
--
-- This function is identical to 'addManyToSet', except that it only returns
-- the set itself. If you need to know what values were inserted, then use the
-- latter instead.
addMany
  :: [Text]
  -- ^ A list of strings to add to the set
  -> FuzzySet
  -- ^ The set to add the strings to
  -> FuzzySet
  -- ^ The updated set
addMany = snd <$$> addManyToSet

-- | Create a new set from a list of entries, using the default settings.
fromList
  :: [Text]
  -- ^ A list of strings to insert into the new set
  -> FuzzySet
  -- ^ A new fuzzy string set
fromList = (`addMany` defaultSet)

-- | Return the elements of the set. No particular order is guaranteed.
--
-- >>> values (fromList ["bass", "craze", "space", "lace", "daze", "haze", "ace", "maze"])
-- ["space","daze","bass","maze","ace","craze","lace","haze"]
values :: FuzzySet -> [Text]
values = elems . exactSet

-- | Return the number of entries in the set.
--
-- >>> size (defaultSet >+< "map" >+< "cap")
-- 2
-- >>> size (defaultSet >+< "bork" >+< "bork" >+< "bork")
-- 1
size :: FuzzySet -> Int
size = length <$> values

-- | Return a boolean indicating whether the set is empty.
--
-- >>> isEmpty (fromList [])
-- True
-- >>> isEmpty $ fromList ["Aramis", "Porthos", "Athos"]
-- False
isEmpty :: FuzzySet -> Bool
isEmpty = null <$> values
