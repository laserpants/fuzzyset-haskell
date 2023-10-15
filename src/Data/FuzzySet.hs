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
    -- * Alternative API
    --
    -- | This

    -- * How to use this library
    -- $howto

    -- * Monad
    FuzzySearch
  , MonadFuzzySearch
  , runFuzzySearch
  , runDefaultFuzzySearch

    -- * Monad transformer
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
-- Make sure the @OverloadedStrings@ pragma is enabled and import the module:
--
-- > import Data.FuzzySet
--
--
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Data.Text (Text)
-- > import Data.FuzzySet
-- >
-- > findMovie :: Text -> FuzzySet (Maybe Text)
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
--
-- > findMovie :: Text -> FuzzySearchT IO (Maybe Text)
-- > findMovie = closestMatchMin 0.8
--
-- The output is now:
--
-- > Nothing
--
--
--
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
