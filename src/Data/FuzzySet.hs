{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- | A fuzzy string set data structure for approximate string matching. This 
--   library is based on [fuzzyset.js](http://glench.github.io/fuzzyset.js/).

module Data.FuzzySet
  ( 
  -- * How to use
  -- $howto

  -- * Types
    FuzzySet(..)

  -- * Methods
  , defaultSet

  -- ** Adding
  , add
  , addToSet
  , addMany
  , fromList

  -- ** Retrieving
  , get
  , getWithMinScore

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
-- > module Main where
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
-- > set = fromList states
-- > 
-- > main = mapM_ print (get set "Burger Islands")
--
-- > (0.7142857142857143,"Virgin Islands")
-- > (0.5714285714285714,"Rhode Island")
-- > (0.44,"Northern Marianas Islands")
-- > (0.35714285714285715,"Maryland")
--
-- > >>> get set "Why-oh-me-ing"
-- > (0.5384615384615384,"Wyoming")

-- | A default 'FuzzySet' with the following fields:
--
-- > { gramSizeLower  = 2
-- > , gramSizeUpper  = 3
-- > , useLevenshtein = True
-- > , exactSet       = ε
-- > , matchDict      = ε
-- > , items          = ε }
defaultSet ∷ FuzzySet
defaultSet = FuzzySet 2 3 True ε ε ε

-- | Try to match the given string against the entries in the set, and return
--   results with score greater than the specified minimum score (first 
--   argument).
getWithMinScore ∷ Double
                -- ^ A minimum score
                → FuzzySet
                -- ^ The set to compare the string against
                → Text
                -- ^ The lookup query
                → [(Double, Text)]
                -- ^ A list of results (pairs of score and matched value)
getWithMinScore minScore FuzzySet{..} val =
    case HashMap.lookup key exactSet of
      Just v  → [(1, v)]
      Nothing → fromMaybe [] $ find (not ∘ null) (getMatch ctx <$> sizes)
  where
    ctx = GetContext key minScore FuzzySet{..}
    key = Text.toLower val
    sizes = reverse [gramSizeLower .. gramSizeUpper]

-- | Try to match the given string against the entries in the set, using the
--   default minimum score of 0.33.
get ∷ FuzzySet
    -- ^ The set to compare the string against
    → Text
    -- ^ The lookup query
    → [(Double, Text)]
    -- ^ A list of results (pairs of score and matched value)
get = getWithMinScore 0.33

-- | Add an entry to the set, or do nothing if a key identical to the provided
--   key already exists in the set.
add ∷ FuzzySet
    -- ^ A set to add the entry to
    → Text
    -- ^ The new entry
    → FuzzySet
    -- ^ A new fuzzy string set
add set = fst ∘ addToSet set

-- | Add an entry to the set and return a pair with the new set, and a boolean
--   value to indicate whether a new entry was added to the set.
addToSet ∷ FuzzySet
         -- ^ A set to add the entry to
         → Text
         -- ^ The new entry
         → (FuzzySet, Bool)
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

-- | Add a list of entries to the set, in one go. (@addMany = foldr (flip add)@)
addMany ∷ FuzzySet
        -- ^ A set to add the entries to
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
-- >>> size $ defaultSet `add` "map" `add` "cap"
-- 2
size ∷ FuzzySet → Int
size = HashMap.size ∘ exactSet

-- | Return a boolean to denote whether the provided set is empty.
--
-- >>> isEmpty (fromList [])
-- True
isEmpty ∷ FuzzySet → Bool
isEmpty = HashMap.null ∘ exactSet

-- | Return the elements of a set.
--
-- >>> values $ defaultSet `addMany` ["bass", "craze", "space", "lace", "daze", "haze", "ace", "maze"] 
-- ["space","daze","bass","maze","ace","craze","lace","haze"]
values ∷ FuzzySet → [Text]
values = elems ∘ exactSet
