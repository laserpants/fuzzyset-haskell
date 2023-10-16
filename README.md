# fuzzyset-haskell

[![License](https://img.shields.io/badge/license-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Language](https://img.shields.io/badge/language-Haskell-yellow.svg)](https://www.haskell.org/)
[![Hackage](https://img.shields.io/hackage/v/fuzzyset.svg)](http://hackage.haskell.org/package/fuzzyset)

A fuzzy string set data structure for approximate string matching.

In a nutshell:

1. Add data to the set (see `add`, `add_`, `addMany`, and `addMany_`)
2. Query the set (see `find`, `findMin`, `findOne`, `findOneMin`, `closestMatchMin`, and `closestMatch`)

Refer to the [Haddock docs](http://hackage.haskell.org/package/fuzzyset) for details.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Data.FuzzySet (FuzzySearch, add_, closestMatch, runDefaultFuzzySearch)

findMovie :: Text -> FuzzySearch (Maybe Text)
findMovie title = do
  add_ "Jurassic Park"
  add_ "Terminator"
  add_ "The Matrix"
  closestMatch title

main :: IO ()
main = do
  let result = runDefaultFuzzySearch (findMovie "The Percolator")
  print result
```
