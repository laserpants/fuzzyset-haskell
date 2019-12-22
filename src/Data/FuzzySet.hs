{-# LANGUAGE RecordWildCards #-}

-- |
--
-- Module      : Data.FuzzySet
-- Copyright   : (c) 2017-2019 Johannes Hildén
-- License     : BSD3
-- Maintainer  : hildenjohannes@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- A fuzzy string set data structure for approximate string matching. This
-- implementation is based on the Python and JavaScript libraries with similar
-- names; [fuzzyset.js](http://glench.github.io/fuzzyset.js/), and the original
-- [fuzzyset](https://github.com/axiak/fuzzyset) Python library.

module Data.FuzzySet
    (
    -- * How to use this library
    -- $howto

    -- * Types
      FuzzySet

    -- * API

    -- ** Initializing
    , emptySet
    , defaultSet
    , fromList

    -- ** Adding
    , add
    , addToSet
    , addMany

    -- ** Retrieving
    , get
    , getWithMinScore
    , getOne
    , getOneWithMinScore

    -- ** Inspecting
    , size
    , isEmpty
    , values

    -- * Implementation
    -- $implementation

    ) where

import Data.Default (Default, def)
import Data.FuzzySet.Internal
import Data.FuzzySet.Types
import Data.FuzzySet.Util
import Data.HashMap.Strict (HashMap, elems, insert)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (snoc)
import qualified Data.FuzzySet.Util as Util
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- $howto
--
-- Make sure the @OverloadedStrings@ pragma is enabled. After that, three steps
-- are typically involved:
--
--   1. Create a set using one of 'defaultSet', 'emptySet', or 'fromList'.
--   2. To add entries, use 'add', 'addToSet', or 'addMany'.
--   3. Query the set with 'get', 'getOne', 'getWithMinScore', or 'getOneWithMinScore'.
--
-- >>> defaultSet `add` "Jurassic Park" `add` "Terminator" `add` "The Matrix" `getOne` "percolator"
-- Just "Terminator"
--
-- >>> defaultSet `add` "Shaggy Rogers" `add` "Fred Jones" `add` "Daphne Blake" `add` "Velma Dinkley" `get` "Shaggy Jones"
-- [(0.7692307692307693,"Shaggy Rogers"),(0.5,"Fred Jones")]
--
-- There are also a few functions to inspect sets: 'size', 'isEmpty', and 'values'.
--
-- == More examples
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Data.FuzzySet
-- >
-- > states = [ "Alabama"        , "Alaska"         , "American Samoa"            , "Arizona"       , "Arkansas"
-- >          , "California"     , "Colorado"       , "Connecticut"               , "Delaware"      , "District of Columbia"
-- >          , "Florida"        , "Georgia"        , "Guam"                      , "Hawaii"        , "Idaho"
-- >          , "Illinois"       , "Indiana"        , "Iowa"                      , "Kansas"        , "Kentucky"
-- >          , "Louisiana"      , "Maine"          , "Maryland"                  , "Massachusetts" , "Michigan"
-- >          , "Minnesota"      , "Mississippi"    , "Missouri"                  , "Montana"       , "Nebraska"
-- >          , "Nevada"         , "New Hampshire"  , "New Jersey"                , "New Mexico"    , "New York"
-- >          , "North Carolina" , "North Dakota"   , "Northern Marianas Islands" , "Ohio"          , "Oklahoma"
-- >          , "Oregon"         , "Pennsylvania"   , "Puerto Rico"               , "Rhode Island"  , "South Carolina"
-- >          , "South Dakota"   , "Tennessee"      , "Texas"                     , "Utah"          , "Vermont"
-- >          , "Virginia"       , "Virgin Islands" , "Washington"                , "West Virginia" , "Wisconsin"
-- >          , "Wyoming" ]
-- >
-- > statesSet = fromList states
-- >
-- > main = mapM_ print (get statesSet "Burger Islands")
--
-- The output of this program is:
--
-- > (0.7142857142857143,"Virgin Islands")
-- > (0.5714285714285714,"Rhode Island")
-- > (0.44,"Northern Marianas Islands")
-- > (0.35714285714285715,"Maryland")
--
--  Using the definition of @statesSet@ from previous example:
--
-- > >>> get statesSet "Why-oh-me-ing"
-- > [(0.5384615384615384,"Wyoming")]
--
-- > >>> get statesSet "Connect a cat"
-- > [(0.6923076923076923,"Connecticut")]
--
-- > >>> get statesSet "Transylvania"
-- > [(0.75,"Pennsylvania"),(0.3333333333333333,"California"),(0.3333333333333333,"Arkansas"),(0.3333333333333333,"Kansas")]
--
-- > >>> get statesSet "CanOfSauce"
-- > [(0.4,"Kansas")]
--
-- > >>> get statesSet "Alaska"
-- > [(1.0,"Alaska")]
--
-- > >>> get statesSet "Alaskanbraskansas"
-- > [(0.47058823529411764,"Arkansas"),(0.35294117647058826,"Kansas"),(0.35294117647058826,"Alaska"),(0.35294117647058826,"Alabama"),(0.35294117647058826,"Nebraska")]
--
-- $implementation
--
-- To determine the similarity between entries of the set and the search string,
-- the algorithm translates the strings to vectors and then calculates a metric
-- known as the /cosine similarity/ between these. A detailed explanation, with
-- interactive examples, can be found on the website for the
-- [JavaScript version](http://glench.github.io/fuzzyset.js/ui/) of this library.
-- A brief overview follows here.
--
-- == Cosine similarity
--
-- The cosine similarity of two vectors \(A\) and \(B\) is given by the formula
--
-- \[ \frac{A \cdot B}{||A||\ ||B||} \]
--
-- where \(A \cdot B\) is the dot product of the two vectors, and \(||A||\)
-- denotes the [euclidean norm](Data-FuzzySet-Util.html#v:norm), or /magnitude/,
-- of \(A\). Since the cosine similarity is a measure of the (cosine of the)
-- angle between two vectors, it is always in the range \([0, 1]\).
--
-- == Gram vectors
--
-- The vector we are interested in has as its components the number of times
-- a gram (substring) occurs in the (normalized version of the) string. The
-- function 'gramVector' takes an arbitrary string as input and returns this
-- vector, in dictionary form:
--
-- >>> gramVector "Mississippi" 3
-- fromList [("pi-",1),("ssi",2),("sis",1),("iss",2),("-mi",1),("mis",1),("sip",1),("ppi",1),("ipp",1)]
--
-- This dictionary maps each /n/-gram key to to the number of times it occurs
-- in the string. The below table makes it more evident that this can be thought
-- of as a sparse vector.
--
-- +---------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
-- | /Gram/  | @-mi@ | @mis@ | @iss@ | @ssi@ | @sis@ | @sip@ | @ipp@ | @ppi@ | @pi-@ |
-- +---------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
-- | /Count/ |   1   |   1   |   2   |   2   |   1   |   1   |   1   |   1   |   1   |
-- +---------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
--
-- == Lookup
--
-- The 'FuzzySet' data structure maintains a dictionary with all /n/-grams that
-- occur in the entries of the set for different sizes of grams.
--
-- > "mis" => [ { itemIndex = 4, gramCount = 1 }, { itemIndex = 11, gramCount = 2 } ]
--
-- To compute the cosine similarity score, the function 'Data.FuzzySet.Internal.getMatches'
-- queries the set for the grams that stem from the search string. Here is an
-- example: Let's say we have a set where the string @"coffee"@ appears, and
-- want to search for the string @"covfefe"@. The two strings translate to the
-- following /bigram/ vectors:
--
-- >>> gramVector "coffee" 2
-- fromList [("e-",1),("ff",1),("of",1),("co",1),("ee",1),("fe",1),("-c",1)]
-- >>> gramVector "covfefe" 2
-- fromList [("e-",1),("vf",1),("ef",1),("ov",1),("co",1),("fe",2),("-c",1)]
--
-- +------------------------------------------------+------------------------------------------------+
-- |                 /coffee/                       |            /covfefe/                           |
-- +------+------+------+------+------+------+------+------+------+------+------+------+------+------+
-- | @-c@ | @co@ | @of@ | @ff@ | @fe@ | @ee@ | @e-@ | @-c@ | @co@ | @ov@ | @vf@ | @fe@ | @e-@ | @e-@ |
-- +------+------+------+------+------+------+------+------+------+------+------+------+------+------+
-- |  1   |  1   |  1   |  1   |  1   |  1   |  1   |  1   |  1   |  1   |  1   |  2   |  1   |  1   |
-- +------+------+------+------+------+------+------+------+------+------+------+------+------+------+
--
-- The non-zero entries common to both vectors are then:
--
-- +---------+------+------+------+------+
-- |         | @-c@ | @co@ | @fe@ | @e-@ |
-- +---------+------+------+------+------+
-- | \(a_i\) |  1   |  1   |  1   |  1   |
-- +---------+------+------+------+------+
-- | \(b_i\) |  1   |  1   |  2   |  1   |
-- +---------+------+------+------+------+
--
-- Dotting these we get \(1 \times 1 + 1 \times 1 + 1 \times 2 + 1 \times 1 = 5 \).
-- The function 'matches' computes these dot products and returns a dictionary
-- with the matched indices as keys. If the entry appears at item index, say,
-- 3 in the set's internal list, this would yield a key-value pair @3 => 5@ in
-- the map.
--
-- >>> matches (defaultSet `add` "tea" `add` "biscuits" `add` "cake" `add` "coffee") (gramVector "covfefe" 2)
-- fromList [(2,2),(3,5)]
--
-- We now have the numerators of the cosine similarity scores. The data
-- structure keeps track of the magnitudes of the set entries, so we just need
-- to look this quantity up using the index:
--
-- > {vectorMagnitude = 2.6457513110645907, normalizedEntry = "coffee"}
--
-- Multiplying this by the magnitude of the search string's vector,
--
-- >>> norm $ elems $ gramVector "covfefe" 2
-- 3.1622776601683795
--
-- … we get 8.366600265340756, which is the denominator of the expression.
-- So the similarity score for this entry of the set is
--
-- >>> 5/(3.1622776601683795 * 2.6457513110645907)
-- 0.5976143046671968
--
-- And indeed, this is the score we get if the 'FuzzySet' is initialized with
-- the Levenshtein option set to @False@:
--
-- >>> (emptySet 2 3 False `add` "tea" `add` "biscuits" `add` "cake" `add` "coffee") `get` "covfefe"
-- [(0.5976143046671968,"coffee")]
--
-- Note that the above procedure is repeated for each gram size (starting with
-- the highest) in the selected range, until we either get some results, or all
-- sizes have been exhausted.
--
-- Finally, if the set was initialized with the Levenshtein distance option
-- enabled (e.g., using 'defaultSet'), then only the first 50 results are kept
-- and a new score is computed based on the Levenshtein 'Data.FuzzySet.Util.distance'.
--


-- | Initialize an empty 'FuzzySet'.
--
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
emptySet =
    FuzzySet mempty mempty mempty


-- | An empty 'FuzzySet' with the following defaults:
--
--   * Gram size lower: @2@
--   * Gram size upper: @3@
--   * Use Levenshtein distance: @True@
--
defaultSet :: FuzzySet
defaultSet =
    emptySet 2 3 True


-- | See 'defaultSet'.
--
instance Default FuzzySet where
    def = defaultSet


-- | Try to match a string against the entries in the set, and return a list of
-- all results with a score greater than or equal to the specified minimum score
-- (i.e., the first argument). The results are ordered by similarity score, with
-- the closest match first.
--
getWithMinScore
    :: Double
    -- ^ A minimum score
    -> FuzzySet
    -- ^ The fuzzy string set to compare the string against
    -> Text
    -- ^ The string to search for
    -> [( Double, Text )]
    -- ^ A list of results (score and matched value)
getWithMinScore
      minScore
      set@FuzzySet{ gramSizeLower = lower, gramSizeUpper = upper, .. }
      value =
    case key `HashMap.lookup` exactSet of
        Just match ->
            [( 1, match )]

        Nothing ->
            sizes
                |> fmap (getMatches set key minScore)
                |> find (not . null)
                |> fromMaybe []
  where
    key = Text.toLower value
    sizes = reverse (enumFromTo lower upper)


-- | Try to match the given string against the entries in the set, using a
-- minimum score of 0.33. Return a list of results ordered by similarity score,
-- with the closest match first. Use 'getWithMinScore' to specify a different
-- threshold value.
--
get
    :: FuzzySet
    -- ^ The fuzzy string set to compare the string against
    -> Text
    -- ^ The string to search for
    -> [( Double, Text )]
    -- ^ A list of results (score and matched value)
get =
    getWithMinScore 0.33


-- | Try to match the given string against the entries in the set using the
-- specified minimum score and return the closest match, if one is found.
--
getOneWithMinScore
    :: Double
    -- ^ A minimum score
    -> FuzzySet
    -- ^ The fuzzy string set to compare the string against
    -> Text
    -- ^ The string to search for
    -> Maybe Text
    -- ^ The closest match, if one is found
getOneWithMinScore minScore fuzzySet value =
    case getWithMinScore minScore fuzzySet value of
        [] ->
            Nothing

        head : _ ->
            Just (snd head)


-- | Try to match the given string against the entries in the set, and return
-- the closest match, if one is found. A minimum score of 0.33 is used. To
-- specify a different threshold value, instead use 'getOneWithMinScore'.
--
getOne :: FuzzySet
    -- ^ The fuzzy string set to compare the string against
    -> Text
    -- ^ The string to search for
    -> Maybe Text
    -- ^ The closest match, if one is found
getOne =
    getOneWithMinScore 0.33


-- | Add an entry to the set, or do nothing if a key that matches the string
-- already exists in the set.
--
add
    :: FuzzySet
    -- ^ Set to add the string to
    -> Text
    -- ^ The new entry
    -> FuzzySet
    -- ^ An updated set
add fuzzySet =
    fst . addToSet fuzzySet


-- | Add an entry, unless it is already present in the set. A pair is returned
-- with the new set and a boolean which denotes whether or not anything was
-- inserted.
--
addToSet
    :: FuzzySet
    -- ^ Fuzzy string set to add the entry to
    -> Text
    -- ^ The new entry
    -> ( FuzzySet, Bool )
    -- ^ The updated set and a boolean, which will be 'True' if, and only if,
    -- the value was not already in the set
addToSet set@FuzzySet{ gramSizeLower = lower, gramSizeUpper = upper, .. } value
    | key `elem` exactSet =
        ( set, False )
    | otherwise =
        ( newSet |> updateExactSet value, True )
  where
    newSet = foldr addSize set (enumFromTo lower upper)
    key = Text.toLower value

    addSize :: Int -> FuzzySet -> FuzzySet
    addSize gramSize FuzzySet{..} =
        let
            item = FuzzySetItem (elems grams |> Util.norm) key
        in
        FuzzySet{ items = items |> insert gramSize (itemVector `snoc` item)
                , matchDict = grams |> HashMap.foldrWithKey updateDict matchDict
                , ..  }
      where
        updateDict gram count =
            let
                info = GramInfo (Vector.length itemVector) count
            in
            HashMap.alter (\maybeInfos -> Just $ info : fromMaybe [] maybeInfos) gram

        itemVector =
            items
                |> HashMap.lookup gramSize
                |> fromMaybe Vector.empty
        grams =
            gramVector key gramSize

    updateExactSet :: Text -> FuzzySet -> FuzzySet
    updateExactSet value FuzzySet{..} =
        FuzzySet{ exactSet = exactSet |> insert key value
                , .. }


-- | Add a list of entries to the set, in one go.
--
-- @addMany = foldr (flip add)@
--
addMany :: FuzzySet -> [Text] -> FuzzySet
addMany =
    foldr (flip add)


-- | Create a set from a list of entries, using the default settings.
--
-- @fromList = addMany defaultSet@
--
fromList :: [Text] -> FuzzySet
fromList =
    addMany defaultSet


-- | Return the number of entries in the set.
--
-- >>> size (defaultSet `add` "map" `add` "cap")
-- 2
-- >>> size (defaultSet `add` "bork" `add` "bork" `add` "bork")
-- 1
--
size :: FuzzySet -> Int
size =
    HashMap.size . exactSet


-- | Return a boolean indicating whether the set is empty.
--
-- >>> isEmpty (fromList [])
-- True
-- >>> isEmpty $ fromList ["Aramis", "Porthos", "Athos"]
-- False
--
isEmpty :: FuzzySet -> Bool
isEmpty =
    HashMap.null . exactSet


-- | Return the elements of the set. No particular order is guaranteed.
--
-- >>> values (fromList ["bass", "craze", "space", "lace", "daze", "haze", "ace", "maze"])
-- ["space","daze","bass","maze","ace","craze","lace","haze"]
--
values :: FuzzySet -> [Text]
values =
    elems . exactSet
