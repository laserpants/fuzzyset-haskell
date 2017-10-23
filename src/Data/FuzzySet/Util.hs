{-# LANGUAGE UnicodeSyntax #-}
module Data.FuzzySet.Util
  ( distance
  , enclosedIn
  , normalized
  , norm
  , substr
  , ε
  , (<$$>)
  , (−)
  , (×)
  ) where

import Data.Char                       ( isAlphaNum, isSpace )
import Data.HashMap.Strict             ( HashMap, empty )
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

-- | Returns the euclidian norm, a\/k\/a the /magnitude/, of the input list
--   interpreted as a vector. That is, \( \sqrt{ \sum_{i=0}^n a_i^2 } \) for the
--   input \( \langle a_0, a_1, \dots, a_n \rangle \) where \( a_i \) is the
--   element at position /i/ in the input list.
norm ∷ (Integral a, Floating b) ⇒ [a] → b
norm = sqrt ∘ fromIntegral ∘ sum ∘ fmap (^2)

--  | Return the normalized Levenshtein distance between the two strings.
distance ∷ Text → Text → Double
distance s t = fromRational (toRational d)
  where
    d = levenshteinNorm s t

(<$$>) ∷ (Functor f, Functor g) ⇒ (a → b) → g (f a) → g (f b)
(<$$>) = fmap ∘ fmap
{-# INLINE (<$$>) #-}

-- Empty HashMap
ε ∷ HashMap k v
ε = empty
{-# INLINE ε #-}

-- Unicode minus sign symbol. Slightly longer than the hyphen-minus commonly
-- used, U+2212 aligns naturally with the horizontal bar of the + symbol.
(−) ∷ Num α ⇒ α → α → α
(−) = (-)
{-# INLINE (−) #-}

-- Another unicode operator. This one for multiplication.
(×) ∷ Num α ⇒ α → α → α
(×) = (*)
{-# INLINE (×) #-}
