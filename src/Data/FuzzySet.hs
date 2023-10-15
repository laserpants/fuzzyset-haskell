-- |
--
-- Module      : Data.FuzzySet
-- Copyright   : (c) 2017-present Heikki Johannes Hildén
-- License     : BSD3
-- Maintainer  : hildenjohannes@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Data.FuzzySet
  (
    -- * How to use this library
    -- $howto

    -- * FuzzySearch monad
    FuzzySearch
  , MonadFuzzySearch
  , runFuzzySearch
  , runDefaultFuzzySearch

    -- * FuzzySearch monad transformer
  , FuzzySearchT
  , runFuzzySearchT
  , runDefaultFuzzySearchT

    -- * Insertion
  , add
  , add_
  , addMany
  , addMany_

    -- * Lookup
  , find
  , findMin
  , findOne
  , findOneMin
  , closestMatchMin
  , closestMatch

    -- * Inspection
  , values
  , size
  , isEmpty
  ) where

import Data.FuzzySet.FuzzySearch
  ( MonadFuzzySearch
  , FuzzySearchT(..)
  , FuzzySearch
  , runDefaultFuzzySearchT
  , runFuzzySearchT
  , runFuzzySearch
  , runDefaultFuzzySearch
  , add
  , add_
  , addMany
  , addMany_
  , find
  , findOne
  , findOneMin
  , closestMatch
  , findMin
  , closestMatchMin
  , isEmpty
  , size
  , values
  )

-- $howto
--
-- This library provides two similar, but independent APIs. The `Data.FuzzySet.Simple`
-- module offers a simpler (pure) interface for working with the `FuzzySet` data
-- structure directly (similar to earlier versions of the library). A
-- disadvantage of this approach is that it scales poorly in code that involves
-- IO, and possibly other effects. For most real-world use cases, it is
-- therefore recommended to use the default API and the `FuzzySearch` monad
-- exposed by `Data.FuzzySet` (see examples below).
--
-- The library uses the `Text` type to represent strings. Add the
-- @OverloadedStrings@ pragma to add support for literals of this type. This is
-- used in most of the examples on this page. Then import the default module:
--
-- > import Data.FuzzySet
--
-- There are three types of operations:
--
--   * __Insertion:__ To add entries to the set, use `add`, `add_`, `addMany`, and `addMany_`.
--   * __Lookup:__ To match a string against the values of the set, use `find`, `findMin`, `findOne`, `findOneMin`, `closestMatchMin`, and `closestMatch`.
--   * __Inspection:__ The function `values` returns all strings currently in the set. `size` and `isEmpty` are mostly self-explanatory.
--
-- Finally use `runFuzzySearch`, `runDefaultFuzzySearch`, `runFuzzySearchT`, or `runDefaultFuzzySearchT`
-- to get the result of the computation.
--
-- === Movie title search example
--
-- The following is a simple program to serve as a 'Hello World' example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Data.Text (Text)
-- > import Data.FuzzySet
-- >
-- > findMovie :: Text -> FuzzySearch (Maybe Text)
-- > findMovie title = do
-- >   add_ "Jurassic Park"
-- >   add_ "Terminator"
-- >   add_ "The Matrix"
-- >   closestMatch title
-- >
-- > main :: IO ()
-- > main = do
-- >   let result = runDefaultFuzzySearch (findMovie "percolator")
-- >   print result
--
-- The output of this program is:
--
-- > Just "Terminator"
--
-- === Adding IO
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Control.Monad.Trans.Class (lift)
-- > import Data.Text (Text)
-- > import Data.FuzzySet
-- >
-- > findMovie :: Text -> FuzzySearchT IO (Maybe Text)
-- > findMovie = closestMatch
-- >
-- > prog :: FuzzySearchT IO ()
-- > prog = do
-- >   add_ "Jurassic Park"
-- >   add_ "Terminator"
-- >   add_ "The Matrix"
-- >   result <- findMovie "percolator"
-- >   lift (print result)
-- >
-- > main :: IO ()
-- > main = runDefaultFuzzySearchT prog
--
-- The output of this program is:
--
-- > Just "Terminator"
--
-- To make the search more strict, we can set a custom min score:
--
-- > findMovie :: Text -> FuzzySearchT IO (Maybe Text)
-- > findMovie = closestMatchMin 0.8
--
-- The output is now:
--
-- > Nothing
--
-- === Another example: Favorite fruit repl
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Control.Monad (when)
-- > import Control.Monad.IO.Class (liftIO)
-- > import Data.FuzzySet
-- > import Data.Text (Text, pack, unpack)
-- > import qualified Data.Text as Text
-- >
-- > repl :: FuzzySearchT IO ()
-- > repl = do
-- >   str <- liftIO $ do
-- >     putStrLn "Enter your favorite fruit below, or type \".exit\"."
-- >     putStr "> "
-- >     getLine
-- >   when (str /= ".exit") $ do
-- >     result <- findOneMin 0.6 (pack str)
-- >     liftIO $ case result of
-- >       Nothing ->
-- >         putStrLn "I don't know that fruit."
-- >       Just (1, match) ->
-- >         putStrLn ("You like " <> unpack (Text.toLower match) <> ". Me too!")
-- >       Just (_, match) ->
-- >         putStrLn ("Did you mean \"" <> unpack match <> "\"?")
-- >     repl
-- >
-- > main :: IO ()
-- > main = runDefaultFuzzySearchT $ do
-- >   addMany_ fruits
-- >   repl
-- >
-- > fruits :: [Text]
-- > fruits = [ "Apple", "Apricot", "Avocado", "Banana", "Bilberry", "Blackberry", "Blackcurrant", "Blueberry", "Boysenberry", "Currant", "Cherry", "Cherimoya", "Chico fruit", "Cloudberry", "Coconut", "Cranberry", "Cucumber", "Custard apple", "Damson", "Date", "Dragonfruit", "Durian", "Elderberry", "Feijoa", "Fig", "Goji berry", "Gooseberry", "Grape", "Raisin", "Grapefruit", "Guava", "Honeyberry", "Huckleberry", "Jabuticaba", "Jackfruit", "Jambul", "Jujube", "Juniper berry", "Kiwano", "Kiwifruit", "Kumquat", "Lemon", "Lime", "Loquat", "Longan", "Lychee", "Mango", "Mangosteen", "Marionberry", "Melon", "Cantaloupe", "Honeydew", "Watermelon", "Miracle fruit", "Mulberry", "Nectarine", "Nance", "Olive", "Orange", "Blood orange", "Clementine", "Mandarine", "Tangerine", "Papaya", "Passionfruit", "Peach", "Pear", "Persimmon", "Physalis", "Plantain", "Plum", "Prune", "Pineapple", "Plumcot", "Pomegranate", "Pomelo", "Purple mangosteen", "Quince", "Raspberry", "Salmonberry", "Rambutan", "Redcurrant", "Salal berry", "Salak", "Satsuma", "Soursop", "Star fruit", "Solanum quitoense", "Strawberry", "Tamarillo", "Tamarind", "Ugli fruit", "Yuzu" ]
