{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Data.FuzzySet.Internal
    ( (|>)
    , matches
    , getMatches
    , gramMap
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
-- known as the /pipe/ operator in some other languages.
--
(|>) :: a -> (a -> b) -> b
(|>) = (&)
infixl 1 |>


-- | The dictionary returned by this function is used when computing the cosine
-- similarity between
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


-- | This function does the actual work of querying a set for matches,
-- supported by the other functions in this module.
--
-- A list of /n/-grams is generated for the specified gram size (see 'grams`).
-- 'gramMap' translates this into a dictionary which maps each /n/-gram key to
-- to the number of times it occurs in the list.
--
--
--
getMatches
    :: FuzzySet
    -> Text
    -> Double
    -> Int
    -> [( Double, Text )]
getMatches set@FuzzySet{..} key minScore gramSize =
    results
        |> filter (\pair -> fst pair >= minScore)
        |> fmap (\( score, entry ) -> ( score, exactSet |> lookupDefault "" entry ))
  where
    results =
        let sorted =
                matches set grams
                    |> HashMap.foldrWithKey fun []
                    |> sortBy (comparing (Down . fst))
        in
        if useLevenshtein then
            sorted
                |> take 50
                |> fmap (\( _, entry ) -> ( distance key entry, entry ))
                |> sortBy (comparing (Down . fst))
        else
            sorted

    vectorNorm  = norm (elems grams)
    grams       = gramMap key gramSize
    itemsVector = fromMaybe mempty (gramSize `HashMap.lookup` items)

    fun index score list =
        case itemsVector !? index of
            Nothing ->
                list

            Just FuzzySetItem{..} ->
                ( fromIntegral score / (vectorNorm * vectorMagnitude)
                , normalizedEntry
                ) : list


-- | Generate a list of grams (character substrings) from the normalized input
-- and then create a 'HashMap' with the /n/-grams as keys mapping to the number
-- of occurences of the key in the generated gram list.
--
-- >>> gramMap "xxxx" 2
-- fromList [("-x",1), ("xx",3), ("x-",1)]
--
-- >>> gramMap "bananas" 3
-- fromList [("as-",1),("-ba",1),("ana",2),("nas",1),("ban",1),("nan",1)]
--
-- >>> Data.HashMap.Strict.lookup "nts" (gramMap "intrent'srestaurantsomeoftrent'saunt'santswantsamtorentsomepants" 3)
-- Just 8
--
gramMap
    :: Text
    -- ^ An input string
    -> Int
    -- ^ The gram size /n/, which must be at least /2/
    -> HashMap Text Int
    -- ^ A mapping from /n/-gram keys to the number of times it occurs in the
    -- list returned by 'grams' (all /n/-length substrings of the normalized
    -- input enclosed in hyphens).
gramMap value size =
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
-- list of /3/-grams generated from this normalized string is then
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
    -- ^ The gram length /n/, which must be at least /2/
    -> [Text]
    -- ^ The resulting list of /n/-grams
grams value size
    | size < 2 = error "gram size must be at least 2"
    | otherwise =
        (\offs -> substr size offs str) <$> offsets
  where
    str = normalized value `enclosedIn` '-'
    offsets = [0 .. Text.length str - size]
