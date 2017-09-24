{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Data.FuzzySet where

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
--   "-destroido corp-". The /3/-grams generated from this string are
--
--   > "-de", "des", "est", "str", "tro", "roi", "oid", "ido", "do ", "o c", " co", "cor", "orp", "rp-"
--
--   Given a normalized string of length /s/, we take all substrings of length
--   /n/, letting the offset range from @0@ to @s + 2 − n@. The number of
--   /n/-grams for a normalized string of length /s/ is therefore
--   @s + 2 − n + 1 = s − n + 3@, where @0 < n < s − 2@.
grams ∷ Text   -- ^ An input string
      → Size   -- ^ The variable /n/, which must be /> 1/
      → [Text] -- ^ A /k/-length list of grams of size /n/, with @k = s − n + 3@
grams val size
    | size < 2  = error "gram size must be >= 2"
    | otherwise = ($ str) ∘ substr size <$> [0 .. Text.length str − size]
  where
    str = normalized val `enclosedIn` '-'

gramMap ∷ Text
        -- ^ An input string
        → Size
        -- ^ The gram size /n/, which must be /> 1/
        → HashMap Text Int
        -- ^ A mapping from /n/-gram keys to the number of occurrences of the
        --   key in the list returned by grams (i.e., the list of all /n/-length
        --   substrings of the input enclosed in hyphens).
gramMap val size = foldr (alter ζ) ε (grams val size)
  where
    ζ = pure ∘ succ ∘ fromMaybe 0
