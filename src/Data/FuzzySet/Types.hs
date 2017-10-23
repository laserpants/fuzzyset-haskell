{-# LANGUAGE UnicodeSyntax #-}
module Data.FuzzySet.Types where

import Data.Text           ( Text )
import Data.HashMap.Strict ( HashMap )
import Data.Vector         ( Vector )

data FuzzySetItem = FuzzySetItem
  { vectorMagnitude ∷ !Double
  , normalizedEntry ∷ !Text
  } deriving (Eq, Show)

data GramInfo = GramInfo
  { itemIndex ∷ !Int
  , gramCount ∷ !Int
  } deriving (Eq, Show)

type Size      = Int
type ExactSet  = HashMap Text Text
type MatchDict = HashMap Text [GramInfo]
type ItemMap   = HashMap Size (Vector FuzzySetItem)

data FuzzySet = FuzzySet
  { gramSizeLower  ∷ !Size
  , gramSizeUpper  ∷ !Size
  , useLevenshtein ∷ !Bool
  , exactSet       ∷ !ExactSet
  , matchDict      ∷ !MatchDict
  , items          ∷ !ItemMap
  } deriving (Eq, Show)

type Matches = HashMap Int Int

data GetContext = GetContext
  { key      ∷ !Text
  , minScore ∷ !Double
  , set      ∷ !FuzzySet
  } deriving (Show)
