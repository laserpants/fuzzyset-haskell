{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Data.FuzzySet
  ( addMany
  , addToSet
  , add
  , defaultSet
  , getWithMinScore
  , get
  , isEmpty
  , size
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

-- | A 'FuzzySet' with the following contents:
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
--   results with score greater than the first argument (a minimum score).
getWithMinScore ∷ Double
                → FuzzySet
                → Text
                → [(Double, Text)]
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
    → Text
    → [(Double, Text)]
get = getWithMinScore 0.33

-- | Add an entry to the set, or do nothing if a key identical to the provided
--   key already exists in the set.
add ∷ FuzzySet
    → Text
    → FuzzySet
add set = fst ∘ addToSet set

-- | Add an entry to the set and return a pair with the new set, and a boolean
--   value to indicate whether a new value was added.
addToSet ∷ FuzzySet
         → Text
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
        → [Text]
        → FuzzySet
addMany = foldr (flip add)

-- | Return the number of entries in the set.
size ∷ FuzzySet → Int
size = HashMap.size ∘ exactSet

-- | Return a boolean denoting whether the set is empty.
isEmpty ∷ FuzzySet → Bool
isEmpty = HashMap.null ∘ exactSet

-- | Return the elements of a set.
values ∷ FuzzySet → [Text]
values = elems ∘ exactSet
