{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The code in this module implements the algorithm responsible for querying
-- a set for potential matches and determining how similar the search string is
-- to the entries of a set. The key idea is to translate strings to vectors and
-- then calculate the /cosine similarity/ between these vectors. An excellent
-- explanation with interactive examples can be found on the website for the
-- JavaScript version of this library. I will only give a brief overview here:
--
-- The cosine similarity of two vectors \(A\) and \(B\) is given by the formula
--
-- \[ \frac{A \cdot B}{||A||\ ||B||} \]
--
-- Since it is a measure of the (cosine of the) angle between the two vectors,
-- this value is always in the range \([0, 1]\).
--
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
-- known as the /pipe/ operator in some languages.
--
(|>) :: a -> (a -> b) -> b
(|>) = (&)
infixl 1 |>


-- | The dictionary returned by this function is used when computing the cosine
-- similarity, which is the similarity score assigned to entries that match
-- the search string in the fuzzy set.
--
-- \( \frac{A \cdot B}{||A||\ ||B||} \)
--
--
--
matches
    :: FuzzySet
    -> HashMap Text Int
    -> HashMap Int Int
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
-- supported by the other functions in this module. It works as follows:
--
-- A list of /n/-grams is generated from the query (see 'grams`) for the
-- specified gram size. Subsequently, 'gramVector' translates this list into a
-- dictionary which maps each /n/-gram key to to the number of times it occurs
-- in the list.
--
--
--
getMatches
    :: FuzzySet
    -- ^ The set
    -> Text
    -- ^ The lookup query
    -> Double
    -- ^ A minimum score
    -> Int
    -- ^ The gram size /n/, which must be at least /2/
    -> [( Double, Text )]
    -- ^ TODO
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


-- | Generate a list of /n/-grams (character substrings) from the normalized input
-- and then create a dictionary with the /n/-grams as keys mapping to the number
-- of occurences of the key in the list.
--
-- >>> gramVector "xxxx" 2
-- fromList [("-x",1), ("xx",3), ("x-",1)]
--
-- >>> gramVector "bananas" 3
-- fromList [("as-",1),("-ba",1),("ana",2),("nas",1),("ban",1),("nan",1)]
--
-- In the above example, the substring @"ana"@ occurs twice in the string (at
-- offsets 1 and 3), and therefore has a value of 2 in the dictionary.
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
    -- ^ A mapping from /n/-gram keys to the number of times the substring
    -- occurs in the list returned by 'grams'.
gramVector value size =
    foldr fun HashMap.empty (grams value size)
  where
    fun = HashMap.alter (pure . succ . fromMaybe 0)


-- | Break apart the input string into a list of /n/-grams.
--
-- The string is first 'Data.FuzzySet.Util.normalized' and enclosed in hyphens.
-- We then take all substrings of length /n/, letting the offset range from
-- \(0 \text{ to } s + 2 − n\), where /s/ is the length of the normalized input.
-- The number of /n/-grams is thus \(s + 2 − n + 1 = s − n + 3\).
--
-- /Example:/
-- The string @"Destroido Corp."@ is first normalized to @"destroido corp"@,
-- and then enclosed in hyphens, so that it becomes @"-destroido corp-"@. The
-- /3/-grams generated from this normalized string are:
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
    -- ^ The resulting list of /n/-grams
grams value size
    | size < 2 = error "gram size must be at least 2"
    | otherwise =
        (\offs -> substr size offs str) <$> offsets
  where
    str = normalized value `enclosedIn` '-'
    offsets = [0 .. Text.length str - size]
