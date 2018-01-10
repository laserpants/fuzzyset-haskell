# fuzzyset-haskell [![Build Status](https://img.shields.io/travis/laserpants/fuzzyset-haskell/master.svg?style=flat)](https://travis-ci.org/laserpants/fuzzyset-haskell) [![License](https://img.shields.io/badge/license-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause) [![Language](https://img.shields.io/badge/language-Haskell-yellow.svg)](https://www.haskell.org/) [![Hackage](https://img.shields.io/hackage/v/fuzzyset.svg)](http://hackage.haskell.org/package/fuzzyset)

A fuzzy string set data structure for approximate string matching. This implementation is based on the Python and JavaScript libraries with the same name:

* [JavaScript version](https://github.com/Glench/fuzzyset.js)
* [Python version](https://github.com/axiak/fuzzyset)

## Install

```
cabal install fuzzyset
```

For details, see [Hackage docs](http://hackage.haskell.org/package/fuzzyset). This library is also available on [Stackage](https://www.stackage.org/package/fuzzyset). To install using [Stack](https://www.haskellstack.org/):

```
stack install fuzzyset
```

## How to use

Make sure the `OverloadedStrings` pragma is enabled. Then there are just three steps:

1. Create a set using one of `defaultSet`, `mkSet`, or `fromList`.
2. To add entries, use `add`, `addToSet`, or `addMany`.
3. Then query the set with `get`, `getOne`, or `getWithMinScore`.

```
>>> defaultSet `add` "Jurassic Park" `add` "Terminator" `add` "The Matrix" `getOne` "percolator"
Just "Terminator"
```

```
>>> defaultSet `add` "Shaggy Rogers" `add` "Fred Jones" `add` "Daphne Blake" `add` "Velma Dinkley" `get` "Shaggy Jones"
[(0.7692307692307693,"Shaggy Rogers"),(0.5,"Fred Jones")]
```

See [here](https://github.com/laserpants/fuzzyset-haskell/blob/master/README.md#more-examples) for more examples.

## API

### Initializing

#### `mkSet :: Size -> Size -> Bool -> FuzzySet`

* `Size` The lower bound of gram sizes to use (inclusive)
* `Size`	The upper bound of gram sizes to use (inclusive)
* `Bool`	Whether to use Levenshtein distance to determine the score
* `FuzzySet` An empty fuzzy string set

Initialize a FuzzySet.

#### `defaultSet :: FuzzySet`

A FuzzySet with the following field values:

```
{ gramSizeLower  = 2
, gramSizeUpper  = 3
, useLevenshtein = True
, exactSet       = ε
, matchDict      = ε
, items          = ε }
```

#### `fromList :: [Text] -> FuzzySet`

Create a fuzzy string set with entries from the given list.

```
fromList = addMany defaultSet
```

### Adding

#### `add :: FuzzySet -> Text -> FuzzySet`

Add an entry to the set, or do nothing if a key identical to the provided value already exists in the set.

#### `addToSet :: FuzzySet -> Text -> (FuzzySet, Bool)`

Add an entry to the set and return a pair with the new set, and a boolean to indicate if a new entry was inserted, or not.

#### `addMany :: FuzzySet -> [Text] -> FuzzySet`

Add a list of entries to the set, in one go.

```
addMany = foldr (flip add)
```

### Retrieving

#### `get :: FuzzySet -> Text -> [(Double, Text)]`

Try to match the given string against the entries in the set, using a minimum score of 0.33. Return a list of results ordered by similarity score, with the closest match first.

#### `getWithMinScore :: Double -> FuzzySet -> Text -> [(Double, Text)]`

Try to match the given string against the entries in the set, and return a list of all results with a score greater than or equal to the specified minimum score (i.e., the first argument). The results are ordered by similarity score, with the closest match first.

#### `getOne :: FuzzySet -> Text -> Maybe Text`

Try to match the given string against the entries in the set, and return the closest match, if one is found.

### Inspecting

There are also a few functions to inspect the set.

#### `size :: FuzzySet -> Int`

Return the number of entries in the set.

```
>>> size (defaultSet `add` "map" `add` "cap")
2
```

#### `isEmpty :: FuzzySet -> Bool`

Return a boolean indicating whether the provided set is empty.

```
>>> isEmpty (fromList [])
True
```

#### `values :: FuzzySet -> [Text]`

Return the elements of the set.

```
>>> values (fromList ["bass", "craze", "space", "lace", "daze", "haze", "ace", "maze"])
["space","daze","bass","maze","ace","craze","lace","haze"]
```

### More examples

```
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.FuzzySet

states = [ "Alabama"        , "Alaska"         , "American Samoa"            , "Arizona"       , "Arkansas"
         , "California"     , "Colorado"       , "Connecticut"               , "Delaware"      , "District of Columbia"
         , "Florida"        , "Georgia"        , "Guam"                      , "Hawaii"        , "Idaho"
         , "Illinois"       , "Indiana"        , "Iowa"                      , "Kansas"        , "Kentucky"
         , "Louisiana"      , "Maine"          , "Maryland"                  , "Massachusetts" , "Michigan"
         , "Minnesota"      , "Mississippi"    , "Missouri"                  , "Montana"       , "Nebraska"
         , "Nevada"         , "New Hampshire"  , "New Jersey"                , "New Mexico"    , "New York"
         , "North Carolina" , "North Dakota"   , "Northern Marianas Islands" , "Ohio"          , "Oklahoma"
         , "Oregon"         , "Pennsylvania"   , "Puerto Rico"               , "Rhode Island"  , "South Carolina"
         , "South Dakota"   , "Tennessee"      , "Texas"                     , "Utah"          , "Vermont"
         , "Virginia"       , "Virgin Islands" , "Washington"                , "West Virginia" , "Wisconsin"
         , "Wyoming" ]

statesSet = fromList states

main = mapM_ print (get statesSet "Burger Islands")
```

The output of this program is:

```
(0.7142857142857143,"Virgin Islands")
(0.5714285714285714,"Rhode Island")
(0.44,"Northern Marianas Islands")
(0.35714285714285715,"Maryland")
```

Using the definition of `statesSet` from previous example:

```
>>> get statesSet "Why-oh-me-ing"
[(0.5384615384615384,"Wyoming")]

>>> get statesSet "Connect a cat"
[(0.6923076923076923,"Connecticut")]

>>> get statesSet "Transylvania"
[(0.75,"Pennsylvania"),(0.3333333333333333,"California"),(0.3333333333333333,"Arkansas"),(0.3333333333333333,"Kansas")]

>>> get statesSet "CanOfSauce"
[(0.4,"Kansas")]

>>> get statesSet "Alaska"
[(1.0,"Alaska")]

>>> get statesSet "Alaskanbraskansas"
[(0.47058823529411764,"Arkansas"),(0.35294117647058826,"Kansas"),(0.35294117647058826,"Alaska"),(0.35294117647058826,"Alabama"),(0.35294117647058826,"Nebraska")]
```
