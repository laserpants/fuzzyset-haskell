{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Data.FuzzySet.Internal
  ( FuzzySet(..)
  , FuzzySetItem(..)
  , GramInfo(..)
  , FuzzyMatch
  , emptySet
  , defaultSet
  , findMin
  , findClosestMin
  , find
  , findClosest
  , grams
  , gramVector
  , matches
  , getMatches
  , addToSet
  , add
  , addManyToSet
  , addMany
  , fromList
  , add_
  , addMany_
  , values
  , size
  , isEmpty
  , normalized
  , norm
  , distance
  , (>+<)
  ) where

import Control.Monad.State (MonadState, get, modify, runState)
import Data.Bifunctor (second)
import Data.Char (isAlphaNum, isSpace)
import Data.Foldable (traverse_)
import qualified Data.Foldable as Foldable
import Data.Function ((&))
import Data.FuzzySet.Utils (enclosedIn, safeHead, substr, (<$$$>), (<$$>))
import Data.HashMap.Strict (HashMap, elems, foldrWithKey, insert, insertWith, lookup, lookupDefault)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
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

-- | Main fuzzy string set data type. Use 'Data.FuzzySet.emptySet',
--   'Data.FuzzySet.defaultSet', or 'Data.FuzzySet.fromList' to create new sets.
data FuzzySet = FuzzySet
  { exactSet       :: !(HashMap Text Text)
  , matchDict      :: !(HashMap Text [GramInfo])
  , items          :: !(HashMap Int (Vector FuzzySetItem))
  , gramSizeLower  :: !Int
  , gramSizeUpper  :: !Int
  , useLevenshtein :: !Bool
  } deriving (Eq, Show)

-- | An individual result when looking up a string in the set, consisting of
--   a similarity score in the range 0 to 1, and the matching string.
type FuzzyMatch = (Double, Text)

-- | Initialize an empty 'FuzzySet'.
emptySet
  :: Int
  -- ^ Lower bound on gram sizes to use (inclusive)
  -> Int
  -- ^ Upper bound on gram sizes to use (inclusive)
  -> Bool
  -- ^ Whether or not to use the [Levenshtein distance](https://people.cs.pitt.edu/~kirk/cs1501/Pruhs/Spring2006/assignments/editdistance/Levenshtein%20Distance.htm)
  -- to determine the score
  -> FuzzySet
  -- ^ An empty fuzzy string set
emptySet = FuzzySet mempty mempty mempty

-- | An empty 'FuzzySet' with the following defaults:
--
--   * Gram size lower: @2@
--   * Gram size upper: @3@
--   * Use Levenshtein distance: @True@
defaultSet :: FuzzySet
defaultSet = emptySet 2 3 True

-- | Try to match a string against the entries in the set, and return a list of
--   all results with a score greater than or equal to the specified minimum
--   score (i.e., the first argument). The results are ordered by similarity,
--   with the closest match first.
findMin
  :: Double
  -- ^ A minimum score
  -> Text
  -- ^ The string to search for
  -> FuzzySet
  -- ^ The fuzzy string set to compare the string against
  -> [FuzzyMatch]
  -- ^ A list of results (score and matched value)
findMin minScore str FuzzySet{..} =
  case key `lookup` exactSet of
    Just exactMatch ->
      [(1, exactMatch)]
    Nothing ->
      [gramSizeUpper, gramSizeUpper - 1 .. gramSizeLower]
        & fmap (getMatches FuzzySet{..} key minScore)
        & Foldable.find (not . null)
        & fromMaybe []
  where
    key = Text.toLower str

-- | Try to match the given string against the entries in the set using the
--   specified minimum score and return the closest match, if one is found.
findClosestMin
  :: Double
  -- ^ A minimum score
  -> Text
  -- ^ The string to search for
  -> FuzzySet
  -- ^ The fuzzy string set to compare the string against
  -> Maybe Text
  -- ^ The closest match, if one is found
findClosestMin = fmap snd . safeHead <$$$> findMin

-- | Try to match the given string against the entries in the set, using a
--   minimum score of 0.33. Return a list of results ordered by similarity
--   score, with the closest match first. Use 'findMin' if you need to specify
--   a custom threshold value.
find
  :: Text
  -- ^ The string to search for
  -> FuzzySet
  -- ^ The fuzzy string set to compare the string against
  -> [FuzzyMatch]
  -- ^ A list of results (score and matched value)
find = findMin 0.33

-- | Try to match the given string against the entries in the set, and return
--   the closest match, if one is found. A minimum score of 0.33 is used. To
--   specify a custom threshold value, instead use 'findClosestMin'.
--
findClosest
  :: Text
  -- ^ The string to search for
  -> FuzzySet
  -- ^ The fuzzy string set to compare the string against
  -> Maybe Text
  -- ^ The closest match, if one is found
findClosest = findClosestMin 0.33

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

-- | Add a string to the set, unless it is already present. A pair is returned
--   consisting of a boolean which denotes whether or not anything was inserted,
--   and the updated set.
addToSet
  :: Text
  -- ^ The new entry
  -> FuzzySet
  -- ^ Fuzzy string set to add the entry to
  -> (Bool, FuzzySet)
  -- ^ A flag to indicate if the value was added (i.e., did not already exist
  --   in the set), and the updated set.
addToSet = runState . add_

-- | Add a string to the set, or do nothing if a key that matches the string
--   already exists.
add
  :: Text
  -- ^ The new entry
  -> FuzzySet
  -- ^ Set to add the string to
  -> FuzzySet
  -- ^ An updated set
add = snd <$$> addToSet

-- | Infix operator to add entries to a 'FuzzySet', defined as @flip add@.
(>+<)
  :: FuzzySet
  -- ^ Set to add the string to
  -> Text
  -- ^ The new entry
  -> FuzzySet
  -- ^ An updated set
(>+<) = flip add

infixl 4 >+<

-- | Add a list of strings to the set, all at once.
--
-- Unless you need to know the subset of values that were actually inserted,
-- use 'addMany' instead.
addManyToSet
  :: [Text]
  -- ^ A list of strings to add to the set
  -> FuzzySet
  -- ^ The set to add the strings to
  -> ([Text], FuzzySet)
  -- ^ A pair where the first component is a list of values that were inserted,
  --   and the second is the updated set.
addManyToSet = runState . addMany_

-- | Add a list of strings to the set, all at once.
--
-- This function is identical to 'addManyToSet', except that it only returns
-- the set itself. If you need to know what values were inserted, then use the
-- latter instead.
addMany
  :: [Text]
  -- ^ A list of strings to add to the set
  -> FuzzySet
  -- ^ The set to add the strings to
  -> FuzzySet
  -- ^ The updated set
addMany = snd <$$> addManyToSet

-- | Create a new set from a list of entries, using the default settings.
fromList
  :: [Text]
  -- ^ A list of strings to insert into the new set
  -> FuzzySet
  -- ^ A new fuzzy string set
fromList = (`addMany` defaultSet)

-- | Return the elements of the set. No particular order is guaranteed.
--
-- >>> values (fromList ["bass", "craze", "space", "lace", "daze", "haze", "ace", "maze"])
-- ["space","daze","bass","maze","ace","craze","lace","haze"]
values :: FuzzySet -> [Text]
values = elems . exactSet

-- | Return the number of entries in the set.
--
-- >>> size (defaultSet >+< "map" >+< "cap")
-- 2
-- >>> size (defaultSet >+< "bork" >+< "bork" >+< "bork")
-- 1
size :: FuzzySet -> Int
size = length <$> values

-- | Return a boolean indicating whether the set is empty.
--
-- >>> isEmpty (fromList [])
-- True
-- >>> isEmpty $ fromList ["Aramis", "Porthos", "Athos"]
-- False
isEmpty :: FuzzySet -> Bool
isEmpty = null <$> values

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
