{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- |
--
-- Module      : Data.FuzzySet
-- Copyright   : (c) 2017 Johannes Hildén
-- License     : BSD3
-- Maintainer  : hildenjohannes@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- A fuzzy string set data structure for approximate string matching. This 
-- implementation is based on the Python and JavaScript libraries with the same
-- name: 
--
--   * [JavaScript version](http://glench.github.io/fuzzyset.js/)
--   * [Python version](https://github.com/axiak/fuzzyset)

module Data.FuzzySet
  ( 
  -- * How to use
  -- $howto

  -- * Types
    FuzzySet
  , Size

  -- * API

  -- ** Initializing
  , mkSet
  , defaultSet
  , fromList

  -- ** Adding
  , add
  , addToSet
  , addMany

  -- ** Retrieving
  , get
  , getWithMinScore
  , getOne

  -- ** Inspecting
  , size
  , isEmpty
  , values
  ) where

import Data.Foldable.Unicode
import Data.FuzzySet.Internal
import Data.FuzzySet.Lens
import Data.FuzzySet.Types
import Data.FuzzySet.Util
import Data.HashMap.Strict             ( HashMap, elems, insert, unionWith )
import Data.Maybe                      ( fromMaybe )
import Data.List                       ( find )
import Data.Text                       ( Text )
import Prelude.Unicode                 hiding ( (∈) )

import qualified Data.Text             as Text
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Vector           as Vector

-- $howto
--
-- Make sure the @OverloadedStrings@ pragma is enabled. Then there are just 
-- three steps:
--
--   1. Create a set using one of 'defaultSet', 'mkSet', or 'fromList'.
--   2. To add entries, use 'add', 'addToSet', or 'addMany'.
--   3. Then query the set with 'get', 'getOne', or 'getWithMinScore'.
--
-- >>> defaultSet `add` "Jurassic Park" `add` "Terminator" `add` "The Matrix" `getOne` "percolator"
-- Just "Terminator"
--
-- >>> defaultSet `add` "Shaggy Rogers" `add` "Fred Jones" `add` "Daphne Blake" `add` "Velma Dinkley" `get` "Shaggy Jones"
-- [(0.7692307692307693,"Shaggy Rogers"),(0.5,"Fred Jones")]
--
-- There are also a few functions to [inspect](#g:7) the set.
--
-- == More examples
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- > 
-- > import Data.FuzzySet
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
-- > main = mapM_ print (get statesSet "Burger Islands")
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
-- > >>> get statesSet "Why-oh-me-ing"
-- > [(0.5384615384615384,"Wyoming")]
--
-- > >>> get statesSet "Connect a cat"
-- > [(0.6923076923076923,"Connecticut")]
-- 
-- > >>> get statesSet "Transylvania"
-- > [(0.75,"Pennsylvania"),(0.3333333333333333,"California"),(0.3333333333333333,"Arkansas"),(0.3333333333333333,"Kansas")]
--
-- > >>> get statesSet "CanOfSauce"
-- > [(0.4,"Kansas")]
--
-- > >>> get statesSet "Alaska"
-- > [(1.0,"Alaska")]
--
-- > >>> get statesSet "Alaskanbraskansas"
-- > [(0.47058823529411764,"Arkansas"),(0.35294117647058826,"Kansas"),(0.35294117647058826,"Alaska"),(0.35294117647058826,"Alabama"),(0.35294117647058826,"Nebraska")]

-- | A 'FuzzySet' with the following field values:
--
-- > { gramSizeLower  = 2
-- > , gramSizeUpper  = 3
-- > , useLevenshtein = True
-- > , exactSet       = ε
-- > , matchDict      = ε
-- > , items          = ε }
defaultSet ∷ FuzzySet
defaultSet = FuzzySet 2 3 True ε ε ε

-- | Initialize a 'FuzzySet'.
mkSet ∷ Size 
      -- ^ The lower bound of gram sizes to use (inclusive)
      → Size 
      -- ^ The upper bound of gram sizes to use (inclusive)
      → Bool 
      -- ^ Whether to use [Levenshtein distance](https://people.cs.pitt.edu/~kirk/cs1501/Pruhs/Spring2006/assignments/editdistance/Levenshtein%20Distance.htm) 
      --   to determine the score
      → FuzzySet
      -- ^ An empty fuzzy string set 
mkSet lower upper levenshtein = FuzzySet lower upper levenshtein ε ε ε

-- | Try to match the given string against the entries in the set, and return
--   a list of all results with a score greater than or equal to the specified 
--   minimum score (i.e., the first argument). The results are ordered by
--   similarity score, with the closest match first.
getWithMinScore ∷ Double
                -- ^ A minimum score
                → FuzzySet
                -- ^ The fuzzy string set to compare the string against
                → Text
                -- ^ The lookup query
                → [(Double, Text)]
                -- ^ A list of results (score and matched value pairs)
getWithMinScore minScore FuzzySet{..} val =
    case HashMap.lookup key exactSet of
      Just v  → [(1, v)]
      Nothing → fromMaybe [] $ find (not ∘ null) (getMatch ctx <$> sizes)
  where
    ctx = GetContext key minScore FuzzySet{..}
    key = Text.toLower val
    sizes = reverse [gramSizeLower .. gramSizeUpper]

-- | Try to match the given string against the entries in the set, using a
--   minimum score of 0.33. Return a list of results ordered by similarity 
--   score, with the closest match first.
get ∷ FuzzySet
    -- ^ The fuzzy string set to compare the string against
    → Text
    -- ^ The lookup query
    → [(Double, Text)]
    -- ^ A list of results (score and matched value pairs)
get = getWithMinScore 0.33

-- | Try to match the given string against the entries in the set, and return
--   the closest match, if one is found.
getOne ∷ FuzzySet
       -- ^ The fuzzy string set to compare the string against
       → Text
       -- ^ The lookup query
       → Maybe Text
       -- ^ 'Just' the result, if one was found, otherwise 'Nothing'
getOne set val = 
    case get set val of
      [] → Nothing
      xs → Just (snd (head xs))

-- | Add an entry to the set, or do nothing if a key identical to the provided
--   value already exists in the set.
add ∷ FuzzySet
    -- ^ Fuzzy string set to add the entry to
    → Text
    -- ^ The new entry
    → FuzzySet
    -- ^ The updated set
add set = fst ∘ addToSet set

-- | Add an entry to the set and return a pair with the new set, and a boolean
--   to indicate if a new entry was inserted, or not.
addToSet ∷ FuzzySet
         -- ^ Fuzzy string set to add the entry to
         → Text
         -- ^ The new entry
         → (FuzzySet, Bool)
         -- ^ The updated set and a boolean, which will be 'True' if, and only 
         --   if, the value was not already in the set 
addToSet FuzzySet{..} val
    | key ∈ exactSet = (FuzzySet{..}, False)
    | otherwise =
      let sizes = [gramSizeLower .. gramSizeUpper]
       in (foldr ξ FuzzySet{..} sizes &_exactSet %~ insert key val, True)
  where
    key = Text.toLower val
    ξ size fs =
      let dict' = flip (:) [] ∘ GramInfo index <$> gramMap (normalized val) size
          item  = FuzzySetItem (gramMap key size & elems & norm) key
          index = fs ^._items ^? ix size ^._Just & Vector.length
       in over _matchDict (\dict → unionWith (⧺) dict dict')
        $ over (_items.at size) (Just ∘ (`Vector.snoc` item)
                                      ∘ fromMaybe Vector.empty) fs

-- | Add a list of entries to the set, in one go. 
--
-- > addMany = foldr (flip add)
addMany ∷ FuzzySet
        -- ^ Fuzzy string set to add the entries to
        → [Text]
        -- ^ A list of new entries
        → FuzzySet
        -- ^ A new fuzzy string set
addMany = foldr (flip add)

-- | Create a fuzzy string set with entries from the given list.
--
-- @fromList = addMany defaultSet@
fromList ∷ [Text] → FuzzySet
fromList = addMany defaultSet 

-- | Return the number of entries in the set.
--
-- >>> size (defaultSet `add` "map" `add` "cap")
-- 2
size ∷ FuzzySet → Int
size = HashMap.size ∘ exactSet

-- | Return a boolean indicating whether the provided set is empty.
--
-- >>> isEmpty (fromList [])
-- True
isEmpty ∷ FuzzySet → Bool
isEmpty = HashMap.null ∘ exactSet

-- | Return the elements of a set.
--
-- >>> values (fromList ["bass", "craze", "space", "lace", "daze", "haze", "ace", "maze"])
-- ["space","daze","bass","maze","ace","craze","lace","haze"]
values ∷ FuzzySet → [Text]
values = elems ∘ exactSet
