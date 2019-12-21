{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | = Implementation
--
-- The code in this module is responsible for querying a set for possible 
-- matches and determining how similar a string is to each candidate. The key 
-- idea is to translate strings to vectors and then calculate the /cosine similarity/ 
-- between these vectors. During lookup, this allows us to assign a score to the 
-- set entries that exhibit some similarity with the search string. An excellent 
-- explanation with interactive examples can be found on the website for the 
-- [JavaScript version](http://glench.github.io/fuzzyset.js/ui/) of this library. 
-- A brief overview follows here:
--
-- === Cosine similarity
--
-- The cosine similarity of two vectors \(A\) and \(B\) is given by the formula
--
-- \[ \frac{A \cdot B}{||A||\ ||B||} \]
--
-- where \(A \cdot B\) is the dot product of the two vectors, and \(||A||\)
-- denotes the [euclidean norm](Data-FuzzySet-Util.html#v:norm), or /magnitude/, of \(A\).
-- Since the cosine similarity is a measure of the (cosine of the) angle between
-- the two vectors, it is always in the range \([0, 1]\).
--
-- === Gram vectors
--
-- The vector we are interested in has as its components the number of times
-- a gram (substring) occurs in the (normalized version of the) string under 
-- consideration. The function 'gramVector' takes an arbitrary string as input 
-- and returns this vector, in dictionary form:
--
-- >>> gramVector "Mississippi" 3
-- fromList [("pi-",1),("ssi",2),("sis",1),("iss",2),("-mi",1),("mis",1),("sip",1),("ppi",1),("ipp",1)]
--
-- The below table makes this more clear. 
--
-- +---------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
-- | /Gram/  | @-mi@ | @mis@ | @iss@ | @ssi@ | @sis@ | @sip@ | @ipp@ | @ppi@ | @pi-@ |
-- +---------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
-- | /Count/ |   1   |   1   |   2   |   2   |   1   |   1   |   1   |   1   |   1   |
-- +---------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
--
-- The 'FuzzySet' data structure maintains a lookup table for all grams that 
-- occur in the entries of the set for different sizes.
--
-- diagram
--
-- The function 'matches' 
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
-- the search string in the fuzzy set. It contains 
--
-- For example, let's say we have a set where the string @"coffee"@ appears
-- in the set, and try to search for the string @covfefe@:
--
-- These strings correspond to the following bigram vectors:
--
-- +------------------------------------------------+------------------------------------------------+
-- |                 /coffee/                       |            /covfefe/                           |
-- +------+------+------+------+------+------+------+------+------+------+------+------+------+------+
-- | @-c@ | @co@ | @of@ | @ff@ | @fe@ | @ee@ | @e-@ | @-c@ | @co@ | @ov@ | @vf@ | @fe@ | @e-@ | @e-@ | 
-- +------+------+------+------+------+------+------+------+------+------+------+------+------+------+
-- |  1   |  1   |  1   |  1   |  1   |  1   |  1   |  1   |  1   |  1   |  1   |  2   |  1   |  1   |
-- +------+------+------+------+------+------+------+------+------+------+------+------+------+------+
--
-- The non-zero entries common to these two vectors are then:
--
-- +---------+------+------+------+------+
-- |         | @-c@ | @co@ | @fe@ | @e-@ | 
-- +---------+------+------+------+------+
-- | \(a_i\) |  1   |  1   |  1   |  1   |
-- +---------+------+------+------+------+
-- | \(b_i\) |  1   |  1   |  2   |  1   |
-- +---------+------+------+------+------+
--
-- Dotting these we get \(1 \times 1 + 1 \times 1 + 1 \times 2 + 1 \times 1 = 5 \)
--
-- If the entry appears at item index, say, 3 in the set data structure, this
-- would yield an entry
--  
-- \[
--  3 \implies 5
-- \]
--
-- since @coffee@ has index 3 in the set.
--
-- >>> matches (defaultSet `add` "tea" `add` "biscuits" `add` "cake" `add` "coffee") (gramVector "covfefe" 2)
-- fromList [(2,2),(3,5)]
--
matches
    :: FuzzySet
    -- ^ The string set
    -> HashMap Text Int
    -- ^ A sparse vector representation of the search string, generated by 'gramVector'
    -> HashMap Int Int
    -- ^ A mapping from item index to the dot product of the entry and search string vectors
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
-- A list of /n/-grams is generated from the query (see 'grams`) for a given 
-- gram size. Subsequently, 'gramVector' translates this list into a dictionary 
-- which maps each /n/-gram key to to the number of times it occurs in the list.
--
--
--
getMatches
    :: FuzzySet
    -- ^ The string set
    -> Text
    -- ^ A string to search for
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
    -- ^ The resulting list of /n/-grams
grams value size
    | size < 2 = error "gram size must be at least 2"
    | otherwise =
        (\offs -> substr size offs str) <$> offsets
  where
    str = normalized value `enclosedIn` '-'
    offsets = [0 .. Text.length str - size]
