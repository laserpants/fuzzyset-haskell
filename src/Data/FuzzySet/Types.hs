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

-- | Type alias for representing gram sizes.
type Size      = Int
type ExactSet  = HashMap Text Text
type MatchDict = HashMap Text [GramInfo]
type ItemMap   = HashMap Size (Vector FuzzySetItem)

-- | Opaque fuzzy string set data type. Use 'Data.FuzzySet.defaultSet', 
--   'Data.FuzzySet.mkSet', or 'Data.FuzzySet.fromList' to create 'FuzzySet's.
data FuzzySet = FuzzySet
  { gramSizeLower  ∷ !Size
  , gramSizeUpper  ∷ !Size
  , useLevenshtein ∷ !Bool
  , exactSet       ∷ !ExactSet
  , matchDict      ∷ !MatchDict
  , items          ∷ !ItemMap
  } deriving (Eq, Show)

instance Default FuzzySet where
  def = defaultSet

data GetContext = GetContext
  { key      ∷ !Text
  , minScore ∷ !Double
  , set      ∷ !FuzzySet
  } deriving (Show)

-- | A 'FuzzySet' with the following field values:
--
-- > { gramSizeLower  = 2
-- > , gramSizeUpper  = 3
-- > , useLevenshtein = True
-- > , exactSet       = ε
-- > , matchDict      = ε
-- > , items          = ε }
defaultSet ∷ FuzzySet
defaultSet = FuzzySet 2 3 True ε ε ε

