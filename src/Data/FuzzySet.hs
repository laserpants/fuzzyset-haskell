{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Data.FuzzySet where
--  ( FuzzySet
--  , Size
--  , defaultSet
--  , ε
--  , get
--  , add
--  , addToSet
--  , size
--  , isEmpty
--  , values
--  , gramMap
--  , grams
--
--  , matches
--
--  ) where

import Control.Lens
import Control.Monad
import Data.Foldable.Unicode
import Data.Function                   ( on )
import Data.FuzzySet.Lens
import Data.FuzzySet.Types
import Data.FuzzySet.Util
import Data.HashMap.Strict             ( HashMap, alter, empty, foldrWithKey, insert, member, elems, unionWith )
import Data.List                       ( sortBy, find )
import Data.Maybe                      ( fromMaybe )
import Data.Text                       ( Text )
import Data.Text.Metrics
import Data.Vector                     ( Vector, singleton )
import Prelude.Unicode                 hiding ( (∈) )

import qualified Data.Text             as Text
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Vector           as Vector

import Debug.Trace

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

-- Another unicode operator. This time for multiplication.
(×) ∷ Num α ⇒ α → α → α
(×) = (*)
{-# INLINE (×) #-}

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

get ∷ FuzzySet → Text → [(Double, Text)]
get = getMin 0.33

data LookupCtx = LookupCtx
  { key      ∷ !Text
  , minScore ∷ !Double
  , set      ∷ !FuzzySet
  } deriving (Show)

--type LookupR = Reader LookupCtx

getMin ∷ Double → FuzzySet → Text → [(Double, Text)]
getMin minScore FuzzySet{..} val =
    case exactMatch of
      Just v  → [(1, v)]
      Nothing → fromMaybe [] $ find (not ∘ null) (get_ minScore key FuzzySet{..} <$> sizes)
  where
    key   = Text.toLower val
    sizes = reverse [gramSizeLower .. gramSizeUpper]
    exactMatch = HashMap.lookup key exactSet

get_ ∷ Double → Text → FuzzySet → Size → [(Double, Text)]  -- use Reader monad
get_ minScore key set size = match <$$> filtered
  where
    match α = set ^._exactSet.ix α
    filtered = filter ((<) minScore ∘ fst) sorted
    μ p = p & _1.~ distance (p ^._2) key
    sorted = sortBy (flip compare `on` fst) $
        let rs = results key set size
         in if set ^._useLevenshtein
                then take 50 (μ <$> rs)
                else rs

results ∷ Text → FuzzySet → Size → [(Double, Text)]
results key set size = ζ <$> HashMap.toList (matches set grams)
  where
    grams  = gramMap key size
    normal = norm (elems grams)
    ζ (index, score) =
      let FuzzySetItem{..} = Vector.unsafeIndex (set ^._items.ix size) index
       in (fromIntegral score / (normal × vectorMagnitude), normalizedEntry)

---- | @TODO
--getMin ∷ Double → FuzzySet → Text → [(Double, Text)]
----getMin minScore set@FuzzySet{..} val
--getMin minScore set@FuzzySet{..} val =
--    let key   = Text.toLower val
--        sizes = reverse [ gramSizeLower .. gramSizeUpper ]
--        fff   = get_ minScore key set
--     in case HashMap.lookup key exactSet of
--      Just v  → [(1, v)]
--      Nothing → fromMaybe [] (msum $ fff <$> sizes)
--
--get_ ∷ Double → Text → FuzzySet → Size → Maybe [(Double, Text)]
--get_ minScore key set size
--    -- | null results = Nothing
--    | null xs = Nothing
--    | otherwise = trace (show $ results) $ Just $ fmap (fmap ((HashMap.!) (set ^._exactSet))) xs
--  where
--    f (_,α) = (distance key α, α)
--    xs      = filter ((<) minScore ∘ fst) ys  -- leeeens !?
--    ys      = if set ^._useLevenshtein then take 50 $ map f sorted else sorted
--    sorted  = sortBy (flip compare `on` fst) results
--    results = ζ <$> HashMap.toList (matches set grams)
--    grams   = gramMap key size
--    items   = set ^._items.ix size
--    norm'   = norm (elems grams)
--    ζ (index, score) =
--      let FuzzySetItem{..} = Vector.unsafeIndex items index
--       in (fromIntegral score / (norm' × vectorMagnitude), normalizedEntry)

matches ∷ FuzzySet → HashMap Text Int → Matches
matches set = foldrWithKey ζ empty
  where
    ζ gram occ m = foldr (\GramInfo{..} →
        alter (pure ∘ (+) (occ × gramCount) ∘ fromMaybe 0) itemIndex)
          m (set ^._matchDict.ix gram)

-- | Add an entry to the set, or do nothing if a key identical to the provided
--   key already exists.
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
          item  = FuzzySetItem (gramMap key size & elems & norm) key
          index = fs ^._items ^? ix size ^._Just & Vector.length
       in over _matchDict (\dict → unionWith (⧺) dict dict')
        $ over (_items.at size) (Just ∘ (`Vector.snoc` item)
                                      ∘ fromMaybe Vector.empty) fs
    key = Text.toLower val

addMany ∷ FuzzySet → [Text] → FuzzySet
addMany = foldr (flip add)

-- | Return the number of entries in the set.
size ∷ FuzzySet → Int
size = HashMap.size ∘ exactSet

-- | Return a boolean to indicate whether the set is empty.
isEmpty ∷ FuzzySet → Bool
isEmpty = HashMap.null ∘ exactSet

-- | Return the elements of a set.
values ∷ FuzzySet → [Text]
values = elems ∘ exactSet
