module Data.FuzzySet.Util
    ( normalized
    , substr
    , enclosedIn
    , norm
    , distance
    ) where

import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text, cons, snoc)
import Data.Text.Metrics (levenshteinNorm)
import qualified Data.Text as Text


-- | Normalize the input by
--
--   * removing non-word characters, except for spaces and commas; and
--   * converting alphabetic characters to lowercase.
--
normalized :: Text -> Text
{-# INLINE normalized #-}
normalized =
    Text.filter word . Text.toLower
  where
    word char
        | isAlphaNum char = True
        | isSpace char = True
        | char == ',' = True
        | otherwise = False


-- | Return /n/ characters starting from offset /m/ in the input string.
--
substr
    :: Int
    -- ^ Length of the substring
    -> Int
    -- ^ The character offset /m/
    -> Text
    -- ^ The input string
    -> Text
    -- ^ A substring of length /n/
{-# INLINE substr #-}
substr n m =
    Text.take n . Text.drop m


-- | Insert a character at the beginning and end of the given string.
--
enclosedIn :: Text -> Char -> Text
{-# INLINE enclosedIn #-}
enclosedIn str char =
    char `cons` str `snoc` char


-- | Returns the euclidean norm, or /magnitude/, of the input list interpreted
-- as a vector.
--
-- That is,
--
-- \( \quad \sqrt{ \sum_{i=0}^n a_i^2 } \)
--
-- for the input
--
-- \( \quad \langle a_0, a_1, \dots, a_n \rangle \)
--
-- where \( a_i \) is the element at position /i/ in the input list.
--
norm :: (Integral a, Floating b) => [a] -> b
norm =
    sqrt . fromIntegral . sum . fmap (^2)


-- | Return the normalized Levenshtein distance between the two strings.
-- See <https://en.wikipedia.org/wiki/Levenshtein_distance>.
--
distance :: Text -> Text -> Double
distance s t =
    fromRational (toRational dist)
  where
    dist = levenshteinNorm s t
