{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax   #-}
module Data.FuzzySet.Internal where

import Data.Function                   ( on )
import Data.FuzzySet.Lens
import Data.FuzzySet.Types
import Data.FuzzySet.Util
import Data.HashMap.Strict             ( HashMap, alter, empty, elems, foldrWithKey )
import Data.Maybe                      ( fromMaybe )
import Data.List                       ( sortBy )
import Data.Text                       ( Text )
import Prelude.Unicode

import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Text             as Text
import qualified Data.Vector           as Vector

getMatch ∷ GetContext → Size → [(Double, Text)]
getMatch GetContext{..} size = match <$$> filtered
  where
    match α = set ^._exactSet.ix α
    filtered = filter ((<) minScore ∘ fst) sorted
    μ p = p & _1.~ distance (p ^._2) key
    sortByFst = sortBy (flip compare `on` fst)
    sorted = sortByFst $
        let rs = results GetContext{..} size
         in if set ^._useLevenshtein
                then (fmap μ ∘ take 50 ∘ sortByFst) rs
                else rs

results ∷ GetContext → Size → [(Double, Text)]
results GetContext{..} size = ζ <$> HashMap.toList (matches set grams)
  where
    grams  = gramMap key size
    normal = norm (elems grams)
    ζ (index, score) =
      let FuzzySetItem{..} = Vector.unsafeIndex (set ^._items.ix size) index
       in (fromIntegral score / (normal × vectorMagnitude), normalizedEntry)

matches ∷ FuzzySet → HashMap Text Int → HashMap Int Int
matches set = foldrWithKey ζ empty
  where
    ζ gram occ m = foldr (\GramInfo{..} →
        alter (pure ∘ (+) (occ × gramCount) ∘ fromMaybe 0) itemIndex)
          m (set ^._matchDict.ix gram)

-- | Normalize the input string, call 'grams' on the normalized input, and then
--   translate the result to a 'HashMap' with the /n/-grams as keys and 'Int'
--   values corresponding to the number of occurences of the key in the
--   generated gram list.
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
