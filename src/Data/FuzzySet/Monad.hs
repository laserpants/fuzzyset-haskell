{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FuzzySet.Monad
  ( add
  , add_
  , findMin
  , values
  , addMany
  , addMany_
  , find
  , findOne
  , findOneMin
  , closestMatch
  , closestMatchMin
  , size
  , isEmpty
  , FuzzySearchT (..)
  , FuzzySearch
  , runFuzzySearchT
  , runDefaultFuzzySearchT
  , runFuzzySearch
  , runDefaultFuzzySearch
  , MonadFuzzySearch
  ) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Fix
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, gets)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Writer (WriterT)
import Data.FuzzySet.Simple (FuzzySet, FuzzyMatch, emptySet)
import qualified Data.FuzzySet.Simple as Simple
import qualified Data.FuzzySet.Internal as FuzzySet
import Data.FuzzySet.Utils ((<$$$>), (<$$>))
import Data.Text (Text)

-- | FuzzySearch monad transformer
newtype FuzzySearchT m a = FuzzySearchT { getFuzzySearchT :: StateT FuzzySet m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState FuzzySet
    , MonadIO
    , MonadFix
    )

-- | Evaluate a `FuzzySearchT` computation with the given options.
runFuzzySearchT :: (Monad m)
  => FuzzySearchT m a
  -> Int
  -- ^ Lower bound on gram sizes to use (inclusive)
  -> Int
  -- ^ Upper bound on gram sizes to use (inclusive)
  -> Bool
  -- ^ Whether or not to use the Levenshtein distance to determine the score
  -> m a
  -- ^ The result of running the computation in the inner monad
runFuzzySearchT value = evalStateT ( getFuzzySearchT value ) <$$$> emptySet

-- | Evaluate a `FuzzySearchT` computation with the following defaults:
--
--   * Gram size lower: @2@
--   * Gram size upper: @3@
--   * Use Levenshtein distance: @True@
runDefaultFuzzySearchT :: (Monad m) => FuzzySearchT m a -> m a
runDefaultFuzzySearchT value = runFuzzySearchT value 2 3 True

-- | FuzzySearch monad
type FuzzySearch = FuzzySearchT Identity

-- | Evaluate a `FuzzySearch` computation with the given options.
runFuzzySearch
  :: FuzzySearch a
  -> Int
  -- ^ Lower bound on gram sizes to use (inclusive)
  -> Int
  -- ^ Upper bound on gram sizes to use (inclusive)
  -> Bool
  -- ^ Whether or not to use the Levenshtein distance to determine the score
  -> a
  -- ^ The result of running the computation
runFuzzySearch value = runIdentity <$$$> runFuzzySearchT value

-- | Evaluate a `FuzzySearch` computation with the following defaults:
--
--   * Gram size lower: @2@
--   * Gram size upper: @3@
--   * Use Levenshtein distance: @True@
runDefaultFuzzySearch :: FuzzySearch a -> a
runDefaultFuzzySearch value = runFuzzySearch value 2 3 True

class (MonadState FuzzySet m) => MonadFuzzySearch m where
  -- | Add a string to the set. A boolean is returned which is @True@ if the
  --   string was inserted, or @False@ if it already existed in the set.
  add     :: Text
          -- ^ The new entry
          -> m Bool
          -- ^ A flag to indicate whether the value was added (i.e., did not
          --   already exist in the set)
  -- | Try to match a string against the entries in the set, and return a list
  --   of all results with a score greater than or equal to the specified
  --   minimum score (i.e., the first argument). The results are ordered by
  --   similarity, with the closest match first.
  findMin :: Double
          -- ^ A minimum score
          -> Text
          -- ^ The string to search for
          -> m [FuzzyMatch]
          -- ^ A list of results (score and matched value)

instance MonadTrans FuzzySearchT where
  lift = FuzzySearchT . lift

instance (Monad m) => MonadFuzzySearch (FuzzySearchT m) where
  add     = FuzzySet.add_
  findMin = gets <$$> Simple.findMin

instance (MonadFuzzySearch m) => MonadFuzzySearch (StateT FuzzySet m) where
  add     = lift . add
  findMin = lift <$$> findMin

instance (MonadFuzzySearch m) => MonadFuzzySearch (ExceptT e m) where
  add     = lift . add
  findMin = lift <$$> findMin

instance (MonadFuzzySearch m) => MonadFuzzySearch (ReaderT r m) where
  add     = lift . add
  findMin = lift <$$> findMin

instance (MonadFuzzySearch m, Monoid w) => MonadFuzzySearch (WriterT w m) where
  add     = lift . add
  findMin = lift <$$> findMin

instance (MonadFuzzySearch m) => MonadFuzzySearch (MaybeT m) where
  add     = lift . add
  findMin = lift <$$> findMin

instance (MonadFuzzySearch m) => MonadFuzzySearch (ContT r m) where
  add     = lift . add
  findMin = lift <$$> findMin

instance (MonadFuzzySearch m, MonadState FuzzySet (SelectT s m)) => MonadFuzzySearch (SelectT s m) where
  add     = lift . add
  findMin = lift <$$> findMin

-- | Add a string to the set, or do nothing if a key that matches the string
--   already exists.
--
-- This function is identical to 'add', except that the latter returns a
-- boolean to indicate whether any new value was added.
add_ :: (MonadFuzzySearch m) => Text -> m ()
add_ = void . add

-- | Add a list of strings to the set, all at once.
--
-- Unless you need to know the subset of values that were actually inserted,
-- use 'addMany_' instead.
addMany :: (MonadFuzzySearch m)
  => [Text]
  -- ^ A list of strings to add to the set
  -> m [Text]
  -- ^ A list of values that were inserted
addMany = FuzzySet.addMany_

-- | Add a list of strings to the set, all at once.
--
-- This function is identical to 'addMany', except that the latter returns a
-- list of all values that were inserted.
addMany_ :: (MonadFuzzySearch m)
  => [Text]
  -- ^ A list of strings to add to the set
  -> m ()
addMany_ = void . addMany

-- | Try to match the given string against the entries in the set, using a
--   minimum score of 0.33. Return a list of results ordered by similarity
--   score, with the closest match first. Use 'findMin' if you need to specify
--   a custom threshold value.
find :: (MonadFuzzySearch m)
  => Text
  -- ^ The string to search for
  -> m [FuzzyMatch]
  -- ^ A list of results (score and matched value)
find str = gets (Simple.find str)

-- | Try to match the given string against the entries in the set using the
--   specified minimum score and return the closest match, if one is found.
findOneMin :: (MonadFuzzySearch m)
  => Double
  -- ^ A minimum score
  -> Text
  -- ^ The string to search for
  -> m (Maybe FuzzyMatch)
  -- ^ The closest match, if one is found
findOneMin minScore str = gets (Simple.findOneMin minScore str)

-- | Try to match the given string against the entries in the set, and return
--   the closest match, if one is found. A minimum score of 0.33 is used. To
--   specify a custom threshold value, instead use 'findOneMin'.
findOne :: (MonadFuzzySearch m)
  => Text
  -- ^ The string to search for
  -> m (Maybe FuzzyMatch)
  -- ^ The closest match, if one is found
findOne str = gets (Simple.findOne str)

-- | Try to match the given string against the entries in the set using the
--   specified minimum score and return the string that most closely matches
--   the input, if a match is found.
closestMatchMin :: (MonadFuzzySearch m)
  => Double
  -- ^ A minimum score
  -> Text
  -- ^ The string to search for
  -> m (Maybe Text)
  -- ^ The string most closely matching the input, if a match is found
closestMatchMin minScore str = gets (Simple.closestMatchMin minScore str)

-- | Try to match the given string against the entries in the set, and return
--   the string that most closely matches the input, if a match is found. A
--   minimum score of 0.33 is used. To specify a custom threshold value,
--   instead use 'closestMatchMin'.
closestMatch :: (MonadFuzzySearch m)
  => Text
  -- ^ The string to search for
  -> m (Maybe Text)
  -- ^ The string most closely matching the input, if a match is found
closestMatch str = gets (Simple.closestMatch str)

-- | Return the elements of the set. No particular order is guaranteed.
values :: (MonadFuzzySearch m) => m [Text]
values = gets Simple.values

-- | Return the number of entries in the set.
size :: (MonadFuzzySearch m) => m Int
size = length <$> values

-- | Return a boolean indicating whether the set is empty.
isEmpty :: (MonadFuzzySearch m) => m Bool
isEmpty = null <$> values
