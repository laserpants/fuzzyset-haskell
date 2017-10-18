{-# LANGUAGE UnicodeSyntax   #-}
module Data.FuzzySet.Util
  ( normalized
  , substr
  , enclosedIn
  , norm
  , distance
  ) where

import Data.Char                       ( isAlphaNum, isSpace )
import Data.Text                       ( Text, cons, snoc )
import Data.Text.Metrics
import Prelude.Unicode
import qualified Data.Text             as Text

-- | Normalize the input by
--
--     * removing non-word characters, except for spaces and commas; and
--     * converting alphabetic characters to lowercase.
normalized ∷ Text → Text
normalized = Text.filter word ∘ Text.toLower
  where
    word ch
      | isAlphaNum ch = True
      | isSpace    ch = True
      | (≡) ','    ch = True
      | otherwise     = False

-- | Return /n/ characters starting from offset /m/ in the input string.
substr ∷ Int  -- ^ Length of the substring
       → Int  -- ^ A character offset /m/
       → Text -- ^ The input string
       → Text -- ^ A substring of length /n/
{-# INLINE substr #-}
substr n m = Text.take n ∘ Text.drop m

-- | Insert the character /ch/ at the beginning and end of the input string.
enclosedIn ∷ Text → Char → Text
{-# INLINE enclosedIn #-}
enclosedIn str ch = ch `cons` str `snoc` ch

-- | Returns the euclidian norm, or /magnitude/, of the input list interpreted
--   as a vector. That is, \( \sqrt{ \sum_{i=0}^n a_i^2 } \) for the input
--   \( \langle a_0, a_1, \dots, a_n \rangle \) where \( a_i \) is the element
--   at position /i/ in the input list.
norm ∷ (Integral a, Floating b) ⇒ [a] → b
norm = sqrt ∘ fromIntegral ∘ sum ∘ fmap (^2)

distance ∷ Text → Text → Double
distance s t = 1 - fromIntegral (levenshtein s t) / fromIntegral (max ls lt)
  where
    ls = Text.length s
    lt = Text.length t
