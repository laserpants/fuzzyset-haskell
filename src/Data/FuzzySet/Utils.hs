module Data.FuzzySet.Utils
  ( (<$$>)
  , (<$$$>)
  , safeHead
  , enclosedIn
  , substr
  ) where

import Data.Text (Text, cons, snoc)
import qualified Data.Text as Text

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixr 8 <$$>

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

infixr 8 <$$$>

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (h : _) = Just h

enclosedIn :: Text -> Char -> Text
enclosedIn str char = char `cons` str `snoc` char

substr :: Int -> Int -> Text -> Text
substr n m = Text.take n . Text.drop m
