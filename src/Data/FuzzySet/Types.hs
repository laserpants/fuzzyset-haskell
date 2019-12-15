module Data.FuzzySet.Types 
    ( FuzzySetItem(..)
    , GramInfo(..)
    , ExactSet
    , MatchDict
    , ItemMap
    , FuzzySet(..)
    ) where

import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Data.Text (Text)


data FuzzySetItem = FuzzySetItem
    { vectorMagnitude :: !Double
    , normalizedEntry :: !Text
    } deriving (Eq, Show)


data GramInfo = GramInfo
    { itemIndex :: !Int
    , gramCount :: !Int
    } deriving (Eq, Show)


-- | TODO docs
--
type ExactSet = HashMap Text Text


-- | TODO docs
--
type MatchDict = HashMap Text [GramInfo]


-- | TODO docs
--
type ItemMap = HashMap Int (Vector FuzzySetItem)


-- | Fuzzy string set data type representation. Use 'Data.FuzzySet.defaultSet', 
-- 'Data.FuzzySet.mkSet', or 'Data.FuzzySet.fromList' to create 'FuzzySet's.
--
data FuzzySet = FuzzySet
    { exactSet       :: !ExactSet
    , matchDict      :: !MatchDict
    , items          :: !ItemMap
    , gramSizeLower  :: !Int
    , gramSizeUpper  :: !Int
    , useLevenshtein :: !Bool
    } deriving (Eq, Show)
