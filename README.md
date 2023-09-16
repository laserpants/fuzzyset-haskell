# fuzzyset [![Build Status]( [![Haskell CI](https://github.com/laserpants/fuzzyset-haskell/actions/workflows/haskell.yml/badge.svg)](https://github.com/laserpants/fuzzyset-haskell/actions/workflows/haskell.yml) [![License](https://img.shields.io/badge/license-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause) [![Language](https://img.shields.io/badge/language-Haskell-yellow.svg)](https://www.haskell.org/) [![Hackage](https://img.shields.io/hackage/v/fuzzyset.svg)](http://hackage.haskell.org/package/fuzzyset)

A fuzzy string set data structure for approximate string matching. 

## Examples

```haskell
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

```haskell
(0.7142857142857143,"Virgin Islands")
(0.5714285714285714,"Rhode Island")
(0.44,"Northern Marianas Islands")
(0.35714285714285715,"Maryland")
```

Using the same definition of `statesSet` from previous example:

```haskell
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
