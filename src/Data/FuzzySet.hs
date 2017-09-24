{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Data.FuzzySet
  ( FuzzySetItem(..)
  , FuzzySet(..)
  , GramInfo(..)
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

import Data.Foldable.Unicode
import Data.FuzzySet.Util
import Data.HashMap.Strict             ( HashMap, alter, empty, insert, member, unionWith )
import Data.Maybe                      ( fromMaybe )
import Data.Text                       ( Text, cons, snoc )
import Data.Vector                     ( Vector )
import Prelude.Unicode                 hiding ( (∈) )

import qualified Data.Text             as Text
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Vector           as Vector

data FuzzySetItem = FuzzySetItem
  { vectorMagnitude ∷ !Double
  , normalizedEntry ∷ !Text
  } deriving (Eq, Show)

data GramInfo = GramInfo
  { gramIndex ∷ !Int
  , gramCount ∷ !Int
  } deriving (Eq, Show)

type Size      = Int
type ExactSet  = HashMap Text Text
type MatchDict = HashMap Text [GramInfo]
type ItemMap   = HashMap Size (Vector FuzzySetItem)

data FuzzySet = FuzzySet
  { gramSizeLower  ∷ !Size
  , gramSizeUpper  ∷ !Size
  , useLevenshtein ∷ !Bool
  , exactSet       ∷ !ExactSet
  , matchDict      ∷ !MatchDict
  , items          ∷ !ItemMap
  } deriving (Eq, Show)

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
--   of /n/-grams for a normalized string of length /s/ is therefore
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

gramMap ∷ Text
        -- ^ An input string
        → Size
        -- ^ The gram size /n/, which must be at least /2/
        → HashMap Text Int
        -- ^ A mapping from /n/-gram keys to the number of occurrences of the
        --   key in the list returned by grams (i.e., the list of all /n/-length
        --   substrings of the input enclosed in hyphens).
gramMap val size = foldr (alter ζ) ε (grams val size)
  where
    ζ = pure ∘ succ ∘ fromMaybe 0

get ∷ FuzzySet → Text → Int
get = undefined

add ∷ FuzzySet → Text → FuzzySet
add set = fst ∘ addToSet set

addToSet ∷ FuzzySet → Text → (FuzzySet, Bool)
addToSet FuzzySet{..} val
    | key ∈ exactSet = (FuzzySet{..}, False)
    | otherwise      = (set' { exactSet = insert key val exactSet }, True)
  where

    set' ∷ FuzzySet
    set' = foldr addSize FuzzySet{..} [gramSizeLower .. gramSizeUpper]

    -- run once for each gram size in the list (e.g. 2..3)
    addSize ∷ Size → FuzzySet → FuzzySet
    addSize size set@FuzzySet{..} =
      set{ items     = alter f size items
         , matchDict = unionWith xxx matchDict (HashMap.map xx $ gramMap (normalized val) size) }
      where

        xxx ∷ [GramInfo] → [GramInfo] → [GramInfo]
        xxx = (⧺)

        xx ∷ Int → [GramInfo]
        xx count = [GramInfo ix count]
          where
            ix = Vector.length $ HashMap.lookupDefault Vector.empty size items

        f ∷ Maybe (Vector FuzzySetItem) → Maybe (Vector FuzzySetItem)
        f Nothing      = Just (Vector.singleton item)
        f (Just items) = Just (Vector.snoc items item)

        grams = gramMap key size
        item  = FuzzySetItem (sqrtOfSquares (HashMap.elems grams)) key

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
