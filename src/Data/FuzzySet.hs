{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Data.FuzzySet
  ( FuzzySet
  , Size
  , defaultSet
  , ε
  , get
  , add
  , addToSet
  , size
  , isEmpty
  , values
  , gramMap
  , grams
  ) where

import Control.Lens
import Data.Foldable.Unicode
import Data.FuzzySet.Lens
import Data.FuzzySet.Types
import Data.FuzzySet.Util
import Data.HashMap.Strict             ( HashMap, alter, empty, insert, member, elems, unionWith )
import Data.Maybe                      ( fromMaybe )
import Data.Text                       ( Text )
import Data.Vector                     ( Vector, singleton )
import Prelude.Unicode                 hiding ( (∈) )

import qualified Data.Text             as Text
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Vector           as Vector

-- | A 'FuzzySet' with the following contents
--
-- > { gramSizeLower  = 2
-- > , gramSizeLower  = 3
-- > , useLevenshtein = True
-- > , exactSet       = ε
-- > , matchDict      = ε
-- > , items          = ε }
defaultSet ∷ FuzzySet
defaultSet = FuzzySet 2 3 True ε ε ε

-- | Empty HashMap
ε ∷ HashMap k v
ε = empty
{-# INLINE ε #-}

-- Unicode minus sign symbol. Slightly longer than the hyphen-minus commonly
-- used, U+2212 aligns better with the horizontal bar of the + symbol.
(−) ∷ Num α ⇒ α → α → α
(−) = (-)
{-# INLINE (−) #-}

-- | Break apart the normalized input string into a list of /n/-grams. For
--   instance, the string "Destroido Corp." is first normalized into the
--   form "destroido corp", and then enclosed in hyphens, so that it becomes
--   "-destroido corp-". The /3/-grams generated from this normalized string are
--
--   > "-de", "des", "est", "str", "tro", "roi", "oid", "ido", "do ", "o c", " co", "cor", "orp", "rp-"
--
--   Given a normalized string of length /s/, we take all substrings of length
--   /n/, letting the offset range from \(0 \text{ to } s + 2 − n\). The number
--   of /n/-grams for a normalized string of length /s/ is thus
--   \(s + 2 − n + 1 = s − n + 3\), where \(0 < n < s − 2\).
grams ∷ Text   -- ^ An input string
      → Size   -- ^ The variable /n/, which must be at least /2/
      → [Text] -- ^ A /k/-length list of grams of size /n/,
               --   with \(k = s − n + 3\)
grams val size
    | size < 2  = error "gram size must be >= 2"
    | otherwise = ($ str) ∘ substr size <$> [0 .. Text.length str − size]
  where
    str = normalized val `enclosedIn` '-'

-- | Normalize the input string and translate the result to a 'HashMap' with
--   the /n/-grams as keys and 'Int' values corresponding to the number of
--   occurences of the key in the generated gram list.
--
-- >>> gramMap "xxxx" 2
-- fromList [("-x",1), ("xx",3), ("x-",1)]
--
-- >>> Data.HashMap.Strict.lookup "nts" (gramMap "intrent'srestaurantsomeoftrent'saunt'santswantsamtorentsomepants" 3)
-- Just 8
gramMap ∷ Text
        -- ^ An input string
        → Size
        -- ^ The gram size /n/, which must be at least /2/
        → HashMap Text Int
        -- ^ A mapping from /n/-gram keys to the number of occurrences of the
        --   key in the list returned by 'grams' (i.e., the list of all
        --   /n/-length substrings of the input enclosed in hyphens).
gramMap val size = foldr ζ ε (grams val size)
  where
    ζ = alter (pure ∘ succ ∘ fromMaybe 0)

-- | @TODO
get ∷ FuzzySet → Text → Int
get = undefined

_get = undefined

-- | Add an entry to the set. If a key identical to the provided key already
--   exists in the set; do nothing.
add ∷ FuzzySet → Text → FuzzySet
add set = fst ∘ addToSet set

-- | Add an entry to the set and return a pair with the new set, and a boolean
--   value to indicate whether a value was inserted.
addToSet ∷ FuzzySet → Text → (FuzzySet, Bool)
addToSet FuzzySet{..} val
    | key ∈ exactSet = (FuzzySet{..}, False)
    | otherwise =
      let sizes = [gramSizeLower .. gramSizeUpper]
       in (foldr ξ FuzzySet{..} sizes &_exactSet %~ insert key val, True)
  where
    ξ size fs =
      let dict' = flip (:) [] ∘ GramInfo index <$> gramMap (normalized val) size
          item  = FuzzySetItem (gramMap key size & elems & sqrtOfSquares) key
          index = fs ^._items ^? ix size ^._Just & Vector.length
       in over _matchDict (\dict → unionWith (⧺) dict dict')
        $ over (_items.at size) (Just ∘ (`Vector.snoc` item)
                                      ∘ fromMaybe Vector.empty) fs
    key = Text.toLower val

-- | Return the number of entries in the set.
size ∷ FuzzySet → Int
size = HashMap.size ∘ exactSet

-- | Return a boolean to indicate whether the set is empty.
isEmpty ∷ FuzzySet → Bool
isEmpty = HashMap.null ∘ exactSet

-- | Return the elements of a set.
values ∷ FuzzySet → [Text]
values = HashMap.elems ∘ exactSet
