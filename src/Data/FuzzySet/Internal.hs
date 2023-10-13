{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Data.FuzzySet.Internal
  ( FuzzySet
  , FuzzyMatch
  , emptySet
  , defaultSet
  , findMin
  , findClosestMin
  , find
  , findClosest
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

data FuzzySet = FuzzySet
  { exactSet       :: !(HashMap Text Text)
  , matchDict      :: !(HashMap Text [GramInfo])
  , items          :: !(HashMap Int (Vector FuzzySetItem))
  , gramSizeLower  :: !Int
  , gramSizeUpper  :: !Int
  , useLevenshtein :: !Bool
  } deriving (Eq, Show)

type FuzzyMatch = (Double, Text)

emptySet :: Int -> Int -> Bool -> FuzzySet
emptySet = FuzzySet mempty mempty mempty

defaultSet :: FuzzySet
defaultSet = emptySet 2 3 True

findMin :: Double -> Text -> FuzzySet -> [FuzzyMatch]
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

findClosestMin :: Double -> Text -> FuzzySet -> Maybe FuzzyMatch
findClosestMin = safeHead <$$$> findMin

find :: Text -> FuzzySet -> [FuzzyMatch]
find = findMin 0.33

findClosest :: Text -> FuzzySet -> Maybe FuzzyMatch
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

addToSet :: Text -> FuzzySet -> (Bool, FuzzySet)
addToSet = runState . add_

add :: Text -> FuzzySet -> FuzzySet
add = snd <$$> addToSet

(>+<) :: FuzzySet -> Text -> FuzzySet
(>+<) = flip add

infixl 4 >+<

addManyToSet :: [Text] -> FuzzySet -> ([Text], FuzzySet)
addManyToSet = runState . addMany_

addMany :: [Text] -> FuzzySet -> FuzzySet
addMany = snd <$$> addManyToSet

fromList :: [Text] -> FuzzySet
fromList = (`addMany` defaultSet)

values :: FuzzySet -> [Text]
values = elems . exactSet

size :: FuzzySet -> Int
size = length <$> values

isEmpty :: FuzzySet -> Bool
isEmpty = null <$> values

gramVector :: Text -> Int -> HashMap Text Int
gramVector = foldr insert_ HashMap.empty <$$> grams
  where
    insert_ key = HashMap.insertWith (+) key 1

grams :: Text -> Int -> [Text]
grams input size_
  | size_ < 2 = error "gram size_ must be at least 2"
  | otherwise = flip (substr size_) normalizedInput <$> offsets
  where
    normalizedInput = normalized input `enclosedIn` '-'
    offsets = [0 .. Text.length normalizedInput - size_]

normalized :: Text -> Text
normalized = Text.filter word . Text.toLower
  where
    word char
      | isAlphaNum char = True
      | isSpace char = True
      | char == ',' = True
      | otherwise = False

norm :: [Int] -> Double
norm = sqrt . fromIntegral . sum . fmap (^ (2 :: Int))

distance :: Text -> Text -> Double
distance = realToFrac <$$> levenshteinNorm

updateExactSet :: Text -> Text -> FuzzySet -> FuzzySet
updateExactSet key str FuzzySet{..} =
  FuzzySet
    { exactSet = insert key str exactSet
    , ..
    }
