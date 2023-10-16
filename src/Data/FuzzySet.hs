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
    -- * Getting started
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

import Data.FuzzySet.Monad
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
-- This library provides two similar, but independent APIs. The "Data.FuzzySet.Simple"
-- module offers a simpler (pure) interface for working with the t'FuzzySet' data
-- structure directly (similar to earlier versions of the library). A
-- disadvantage of this approach is that it scales poorly when the code involves
-- IO, and possibly other effects. For most real-world use cases, it is
-- therefore recommended to use the default API and the t'FuzzySearch' monad
-- exposed by "Data.FuzzySet" (see below for more examples).
--
-- > findPlanet :: (MonadIO m, MonadFuzzySearch m) => Text -> m ()
-- > findPlanet planetName = do
-- >   addMany_ [ "Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune" ]
-- >   findOne planetName >>= liftIO . print
--
-- >>> runDefaultFuzzySearchT (findPlanet "Joopiter")
-- Just (0.75,"Jupiter")
--
-- Note that all strings are represented as [text](https://hackage.haskell.org/package/text)
-- values. Examples on this page use the @OverloadedStrings@ language extension
-- to allow string literals to be translated to this representation form.
--
-- Import the main module:
--
-- > import Data.FuzzySet
--
-- Fuzzy search involves three types of operations:
--
--   * __Insertion:__ For adding entries to the set, see `add`, `add_`, `addMany`, and `addMany_`.
--   * __Lookup:__ To match a string against all values of the set, use `find`, `findMin`, `findOne`, `findOneMin`, `closestMatchMin`, and `closestMatch`.
--   * __Inspection:__ The function `values` returns all strings currently in the set. `size` and `isEmpty` are mostly self-explanatory.
--
-- Finally, use `runFuzzySearch`, `runDefaultFuzzySearch`, `runFuzzySearchT`, or `runDefaultFuzzySearchT`
-- to get the result of the computation from the monad.
--
-- === Simple search example
--
-- The following is a simple program to serve as a 'Hello World' example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Data.Text (Text)
-- > import Data.FuzzySet (FuzzySearch, add_, closestMatch, runDefaultFuzzySearch)
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
-- >   let result = runDefaultFuzzySearch (findMovie "The Percolator")
-- >   print result
--
-- The output of this program is:
--
-- > Just "Terminator"
--
-- === Adding IO
--
-- Changing the previous example to instead use the `FuzzySearchT` transformer,
-- we can combine the search monad with IO and other effects.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Control.Monad.Trans.Class (lift)
-- > import Data.Text (Text)
-- > import Data.FuzzySet (FuzzySearchT, add_, closestMatch, runDefaultFuzzySearchT)
-- >
-- > findMovie :: Text -> FuzzySearchT IO (Maybe Text)
-- > findMovie = closestMatch
-- >
-- > prog :: FuzzySearchT IO ()
-- > prog = do
-- >   add_ "Jurassic Park"
-- >   add_ "Terminator"
-- >   add_ "The Matrix"
-- >   result <- findMovie "The Percolator"
-- >   lift (print result)
-- >
-- > main :: IO ()
-- > main = runDefaultFuzzySearchT prog
--
-- To make the search more restrictive, we can set a custom min score:
--
-- > findMovie :: Text -> FuzzySearchT IO (Maybe Text)
-- > findMovie = closestMatchMin 0.8
--
-- The output is now:
--
-- > Nothing
--
-- === Another example: Favorite fruit
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
