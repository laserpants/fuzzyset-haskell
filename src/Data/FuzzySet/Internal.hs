{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- The code in this module is responsible for querying a set for possible
-- matches and determining how similar the string is to each candidate match.
--

module Data.FuzzySet.Internal
    ( (|>)
    , matches
    , getMatches
    , gramVector
    , grams
    ) where

import Data.Function ((&))
import Data.FuzzySet.Types
import Data.FuzzySet.Util (distance)
import Data.FuzzySet.Util (norm)
import Data.FuzzySet.Util (normalized, substr, enclosedIn)
import Data.HashMap.Strict (HashMap, elems, foldrWithKey, lookup, lookupDefault, alter)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..), comparing)
import Data.Text (Text)
import Data.Vector ((!?))
import qualified Data.FuzzySet.Util as Util
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text


-- | Alternative syntax for the reverse function application operator @(&)@,
-- known also as the /pipe/ operator.
--
(|>) :: a -> (a -> b) -> b
(|>) = (&)
infixl 1 |>


-- | Dot products used to compute the cosine similarity, which is the similarity
-- score assigned to entries that match the search string in the fuzzy set.
--
matches
    :: FuzzySet
    -- ^ The string set
    -> HashMap Text Int
    -- ^ A sparse vector representation of the search string (generated by 'gramVector')
    -> HashMap Int Int
    -- ^ A mapping from item index to the dot product between the corresponding
    -- entry of the set and the search string
matches set@FuzzySet{..} =
    foldrWithKey fun mempty
  where
    fun gram count map =
        let
            insScore otherCount entry =
                Just (fromMaybe 0 entry + otherCount * count)
        in
        gram `HashMap.lookup` matchDict
            |> maybe map (foldr (\GramInfo{..} -> alter (insScore gramCount) itemIndex) map)


-- | This function performs the actual task of querying a set for matches,
-- supported by the other functions in this module.
-- See [Implementation](Data-FuzzySet.html#g:8) for an explanation.
--
getMatches
    :: FuzzySet
    -- ^ The string set
    -> Text
    -- ^ A string to search for
    -> Double
    -- ^ Minimum score
    -> Int
    -- ^ The gram size /n/, which must be at least /2/
    -> [( Double, Text )]
    -- ^ A list of results (score and matched value)
getMatches set@FuzzySet{..} query minScore gramSize =
    results
        |> filter (\pair -> fst pair >= minScore)
        |> fmap (\( score, entry ) -> ( score, exactSet |> lookupDefault "" entry ))
  where
    results =
        let sorted =
                matches set queryVector
                    |> HashMap.foldrWithKey fun []
                    |> sortBy (comparing (Down . fst))
        in
        if useLevenshtein then
            sorted
                |> take 50
                |> fmap (\( _, entry ) -> ( distance query entry, entry ))
                |> sortBy (comparing (Down . fst))
        else
            sorted

    queryMagnitude = norm (elems queryVector)
    queryVector = gramVector query gramSize
    itemsVector = fromMaybe mempty (gramSize `HashMap.lookup` items)

    fun index score list =
        case itemsVector !? index of
            Nothing ->
                list

            Just FuzzySetItem{..} ->
                ( fromIntegral score / (queryMagnitude * vectorMagnitude)
                , normalizedEntry
                ) : list


-- | Generate a list of /n/-grams (character substrings) from the normalized
-- input and then translate this into a dictionary with the /n/-grams as keys
-- mapping to the number of occurences of the substring in the list.
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
--
gramVector
    :: Text
    -- ^ An input string
    -> Int
    -- ^ The gram size /n/, which must be at least /2/
    -> HashMap Text Int
    -- ^ A sparse vector with the number of times a substring occurs in the
    -- normalized input string
gramVector value size =
    foldr fun HashMap.empty (grams value size)
  where
    fun = HashMap.alter (pure . succ . fromMaybe 0)


-- | Break apart the input string into a list of /n/-grams. The string is
-- first 'Data.FuzzySet.Util.normalized' and enclosed in hyphens.  We then take
-- all substrings of length /n/, letting the offset range from
-- \(0 \text{ to } s + 2 − n\), where /s/ is the length of the normalized input.
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
--
grams
    :: Text
    -- ^ An input string
    -> Int
    -- ^ The gram size /n/, which must be at least /2/
    -> [Text]
    -- ^ A list of /n/-grams
grams value size
    | size < 2 = error "gram size must be at least 2"
    | otherwise =
        (\offs -> substr size offs str) <$> offsets
  where
    str = normalized value `enclosedIn` '-'
    offsets = [0 .. Text.length str - size]
