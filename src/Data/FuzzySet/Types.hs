module Data.FuzzySet.Types
    ( FuzzySetItem(..)
    , GramInfo(..)
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


-- | Main fuzzy string set data type. Use 'Data.FuzzySet.defaultSet',
-- 'Data.FuzzySet.mkSet', or 'Data.FuzzySet.fromList' to create 'FuzzySet's.
--
data FuzzySet = FuzzySet
    { exactSet       :: !(HashMap Text Text)
    , matchDict      :: !(HashMap Text [GramInfo])
    , items          :: !(HashMap Int (Vector FuzzySetItem))
    , gramSizeLower  :: !Int
    , gramSizeUpper  :: !Int
    , useLevenshtein :: !Bool
    } deriving (Eq, Show)
