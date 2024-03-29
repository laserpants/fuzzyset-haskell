# fuzzyset-haskell

[![Haskell CI](https://github.com/laserpants/fuzzyset-haskell/actions/workflows/haskell.yml/badge.svg)](https://github.com/laserpants/fuzzyset-haskell/actions/workflows/haskell.yml)
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
module Main where                                                               ```

import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Data.FuzzySet (FuzzySearchT, add_, closestMatch, runDefaultFuzzySearchT)

findMovie :: Text -> FuzzySearchT IO (Maybe Text)
findMovie = closestMatch

prog :: FuzzySearchT IO ()
prog = do
  add_ "Jurassic Park"
  add_ "Terminator"
  add_ "The Matrix"
  result <- findMovie "The Percolator"
  lift (print result)

main :: IO ()
main = runDefaultFuzzySearchT prog
```
