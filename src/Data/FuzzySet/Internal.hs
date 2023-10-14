{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Data.FuzzySet.Internal
  ( FuzzySet(..)
  , FuzzySetItem(..)
  , GramInfo(..)
  , FuzzyMatch
  , grams
  , gramVector
  , matches
  , getMatches
  , add_
  , addMany_
  , normalized
  , norm
  , distance
  ) where

import Control.Monad.State (MonadState, get, modify)
import Data.Bifunctor (second)
import Data.Char (isAlphaNum, isSpace)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.FuzzySet.Utils (enclosedIn, substr, (<$$>))
import Data.HashMap.Strict (HashMap, elems, foldrWithKey, insert, insertWith, lookup, lookupDefault)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..), comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Metrics (levenshteinNorm)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector
import Prelude hiding (lookup)

data FuzzySetItem = FuzzySetItem
  { vectorMagnitude :: !Double
  , normalizedEntry :: !Text
  } deriving (Eq, Show)

data GramInfo = GramInfo
  { itemIndex :: !Int
  , gramCount :: !Int
  } deriving (Eq, Show)

-- | Main fuzzy string set data type.
data FuzzySet = FuzzySet
  { exactSet       :: !(HashMap Text Text)
  , matchDict      :: !(HashMap Text [GramInfo])
  , items          :: !(HashMap Int (Vector FuzzySetItem))
  , gramSizeLower  :: !Int
  , gramSizeUpper  :: !Int
  , useLevenshtein :: !Bool
  } deriving (Eq, Show)

-- | An individual result when looking up a string in the set, consisting of
--
--     * a similarity score in the range \([0, 1]\), and
--     * the matching string.
type FuzzyMatch = (Double, Text)

matches :: FuzzySet -> HashMap Text Int -> HashMap Int Int
matches FuzzySet{..} = foldrWithKey go mempty
  where
    go gram count hashMap =
      HashMap.lookup gram matchDict
        & maybe hashMap (foldr (insert_ count) hashMap)
    insert_ count GramInfo{..} =
      insertWith (+) itemIndex (gramCount * count)

getMatches :: FuzzySet -> Text -> Double -> Int -> [FuzzyMatch]
getMatches FuzzySet{..} str minScore gramSize =
  results
    & filter ((>= minScore) . fst)
    & fmap (second (flip (lookupDefault mempty) exactSet))
  where
    results =
      let sorted =
            matches FuzzySet{..} queryVector
              & foldrWithKey go []
              & sortBy (comparing (Down . fst))
       in if useLevenshtein
            then
              sorted
                & take 50
                & fmap (\(_, entry) -> (distance str entry, entry))
                & sortBy (comparing (Down . fst))
            else sorted

    queryMagnitude = norm (elems queryVector)
    queryVector = gramVector str gramSize
    itemsVector = fromMaybe mempty (gramSize `lookup` items)

    go index score list =
      case itemsVector !? index of
        Nothing ->
          list
        Just FuzzySetItem{..} ->
          ( fromIntegral score / (queryMagnitude * vectorMagnitude)
          , normalizedEntry
          ) : list

add_ :: (MonadState FuzzySet m) => Text -> m Bool
add_ str = do
  FuzzySet{..} <- get
  if key `elem` exactSet
    then -- An entry already exists
      pure False
    else do
      traverse_ (modify . updateDict) [gramSizeLower .. gramSizeUpper]
      modify (updateExactSet key str)
      pure True
  where
    key = Text.toLower str
    updateDict size_ FuzzySet{..} =
      let
        itemVector =
          items
            & HashMap.lookup size_
            & fromMaybe Vector.empty
        grams_ =
          gramVector key size_
        insertInfo gram count =
          let info = GramInfo (Vector.length itemVector) count
           in HashMap.insertWith (<>) gram [info]
        item =
          FuzzySetItem (elems grams_ & norm) key
       in
        FuzzySet
          { items = insert size_ (itemVector `Vector.snoc` item) items
          , matchDict = foldrWithKey insertInfo matchDict grams_
          , ..
          }

addMany_ :: (MonadState FuzzySet m) => [Text] -> m [Text]
addMany_ = concat <$$> traverse addOne
  where
    addOne str = do
      p <- add_ str
      pure [str | p]

-- | Generate a list of /n/-grams (character substrings) from the normalized
--   input and then translate this into a dictionary with the /n/-grams as keys
--   mapping to the number of occurences of the substring in the list.
--
-- >>> gramVector "xxxx" 2
-- fromList [("-x",1), ("xx",3), ("x-",1)]
--
-- The substring @"xx"@ appears three times in the normalized string:
--
-- >>> grams "xxxx" 2
-- ["-x","xx","xx","xx","x-"]
--
-- >>> Data.HashMap.Strict.lookup "nts" (gramVector "intrent'srestaurantsomeoftrent'saunt'santswantsamtorentsomepants" 3)
-- Just 8
gramVector :: Text -> Int -> HashMap Text Int
gramVector = foldr insert_ HashMap.empty <$$> grams
  where
    insert_ key = HashMap.insertWith (+) key 1

-- | Break apart the input string into a list of /n/-grams. The string is first
--   'Data.FuzzySet.Util.normalized' and enclosed in hyphens. We then take all
--   substrings of length /n/, letting the offset range from \(0 \text{ to } s + 2 − n\),
--   where /s/ is the length of the normalized input.
--
-- /Example:/
-- The string @"Destroido Corp."@ is first normalized to @"destroido corp"@,
-- and then enclosed in hyphens, so that it becomes @"-destroido corp-"@. The
-- trigrams generated from this normalized string are:
--
-- > [ "-de"
-- > , "des"
-- > , "est"
-- > , "str"
-- > , "tro"
-- > , "roi"
-- > , "oid"
-- > , "ido"
-- > , "do "
-- > , "o c"
-- > , " co"
-- > , "cor"
-- > , "orp"
-- > , "rp-"
-- > ]
grams :: Text -> Int -> [Text]
grams input size_
  | size_ < 2 = error "gram size_ must be at least 2"
  | otherwise = flip (substr size_) normalizedInput <$> offsets
  where
    normalizedInput = normalized input `enclosedIn` '-'
    offsets = [0 .. Text.length normalizedInput - size_]

-- | Normalize the input by
--
--   * removing non-word characters, except for spaces and commas; and
--   * converting alphabetic characters to lowercase.
--
normalized :: Text -> Text
normalized = Text.filter word . Text.toLower
  where
    word char
      | isAlphaNum char = True
      | isSpace char    = True
      | char == ','     = True
      | otherwise       = False

-- | Return the euclidean norm, or /magnitude/, of the input list interpreted
--   as a vector.
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
norm :: [Int] -> Double
norm = sqrt . fromIntegral . sum . fmap (^ (2 :: Int))

-- | Return the normalized Levenshtein distance between the two strings.
--
-- See <https://en.wikipedia.org/wiki/Levenshtein_distance>.
distance :: Text -> Text -> Double
distance = realToFrac <$$> levenshteinNorm

updateExactSet :: Text -> Text -> FuzzySet -> FuzzySet
updateExactSet key str FuzzySet{..} =
  FuzzySet
    { exactSet = insert key str exactSet
    , ..
    }
