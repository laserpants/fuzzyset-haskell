{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  , FuzzySetT (..)
  , FuzzySet
  , runFuzzySetT
  , runDefaultFuzzySetT
  , runFuzzySet
  , runDefaultFuzzySet
  , FuzzySetMonad
  ) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, get, gets)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Writer (WriterT)
import Data.FuzzySet.Internal (FuzzyMatch)
import Data.FuzzySet.Simple (emptySet)
import qualified Data.FuzzySet.Simple as Simple
import qualified Data.FuzzySet.Internal as FuzzySet
import Data.FuzzySet.Utils ((<$$$>), (<$$>))
import Data.Text (Text)

-- | FuzzySet monad transformer
newtype FuzzySetT m a = FuzzySetT {getFuzzySetT :: StateT Simple.FuzzySet m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Simple.FuzzySet
    , MonadIO
    )

-- | Evaluate a `FuzzySetT` computation with the given options.
runFuzzySetT :: (Monad m)
  => FuzzySetT m a
  -> Int
  -- ^ Lower bound on gram sizes to use (inclusive)
  -> Int
  -- ^ Upper bound on gram sizes to use (inclusive)
  -> Bool
  -- ^ Whether or not to use the Levenshtein distance to determine the score
  -> m a
  -- ^ The result of running the computation in the inner monad
runFuzzySetT value = evalStateT (getFuzzySetT value) <$$$> emptySet

-- | Evaluate a `FuzzySetT` computation with the default options. This is a
--   short form for @runFuzzySetT value 2 3 True@.
runDefaultFuzzySetT :: (Monad m) => FuzzySetT m a -> m a
runDefaultFuzzySetT value = runFuzzySetT value 2 3 True

-- | FuzzySet monad
type FuzzySet = FuzzySetT Identity

-- | Evaluate a `FuzzySet` computation with the given options.
runFuzzySet
  :: FuzzySet a
  -> Int
  -- ^ Lower bound on gram sizes to use (inclusive)
  -> Int
  -- ^ Upper bound on gram sizes to use (inclusive)
  -> Bool
  -- ^ Whether or not to use the Levenshtein distance to determine the score
  -> a
  -- ^ The result of running the computation
runFuzzySet value = runIdentity <$$$> runFuzzySetT value

-- | Evaluate a `FuzzySet` computation with the default options. This is a
--   short form for @runFuzzySet value 2 3 True@.
runDefaultFuzzySet :: FuzzySet a -> a
runDefaultFuzzySet value = runFuzzySet value 2 3 True

class (Monad m) => FuzzySetMonad m where
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
  -- | Return the elements of the set. No particular order is guaranteed.
  values  :: m [Text]
  _get    :: m Simple.FuzzySet

instance MonadTrans FuzzySetT where
  lift = FuzzySetT . lift

instance (Monad m) => FuzzySetMonad (FuzzySetT m) where
  add     = FuzzySet.add_
  findMin = gets <$$> Simple.findMin
  values  = gets Simple.values
  _get    = get

instance (FuzzySetMonad m) => FuzzySetMonad (StateT s m) where
  add     = lift . add
  findMin = lift <$$> findMin
  values  = lift values
  _get    = lift _get

instance (FuzzySetMonad m) => FuzzySetMonad (ExceptT e m) where
  add     = lift . add
  findMin = lift <$$> findMin
  values  = lift values
  _get    = lift _get

instance (FuzzySetMonad m) => FuzzySetMonad (ReaderT r m) where
  add     = lift . add
  findMin = lift <$$> findMin
  values  = lift values
  _get    = lift _get

instance (FuzzySetMonad m, Monoid w) => FuzzySetMonad (WriterT w m) where
  add     = lift . add
  findMin = lift <$$> findMin
  values  = lift values
  _get    = lift _get

instance (FuzzySetMonad m) => FuzzySetMonad (MaybeT m) where
  add     = lift . add
  findMin = lift <$$> findMin
  values  = lift values
  _get    = lift _get

instance (FuzzySetMonad m) => FuzzySetMonad (ContT r m) where
  add     = lift . add
  findMin = lift <$$> findMin
  values  = lift values
  _get    = lift _get

instance (FuzzySetMonad m) => FuzzySetMonad (SelectT r m) where
  add     = lift . add
  findMin = lift <$$> findMin
  values  = lift values
  _get    = lift _get

-- | Add a string to the set, or do nothing if a key that matches the string
--   already exists.
--
-- This function is identical to 'add', except that the latter returns a
-- boolean to indicate whether any new value was added.
add_ :: (FuzzySetMonad m) => Text -> m ()
add_ = void . add

-- | Add a list of strings to the set, all at once.
--
-- Unless you need to know the subset of values that were actually inserted,
-- use 'addMany_' instead.
addMany :: (MonadState Simple.FuzzySet m)
  => [Text]
  -- ^ A list of strings to add to the set
  -> m [Text]
  -- ^ A list of values that were inserted
addMany = FuzzySet.addMany_

-- | Add a list of strings to the set, all at once.
--
-- This function is identical to 'addMany', except that the latter returns a
-- list of all values that were inserted.
addMany_ :: (MonadState Simple.FuzzySet m)
  => [Text]
  -- ^ A list of strings to add to the set
  -> m ()
addMany_ = void . addMany

-- | Try to match the given string against the entries in the set, using a
--   minimum score of 0.33. Return a list of results ordered by similarity
--   score, with the closest match first. Use 'findMin' if you need to specify
--   a custom threshold value.
find :: (FuzzySetMonad m)
  => Text
  -- ^ The string to search for
  -> m [FuzzyMatch]
  -- ^ A list of results (score and matched value)
find str = Simple.find str <$> _get

-- | Try to match the given string against the entries in the set using the
--   specified minimum score and return the closest match, if one is found.
findOneMin :: (FuzzySetMonad m)
  => Double
  -- ^ A minimum score
  -> Text
  -- ^ The string to search for
  -> m (Maybe FuzzyMatch)
  -- ^ The closest match, if one is found
findOneMin minScore str = Simple.findOneMin minScore str <$> _get

-- | Try to match the given string against the entries in the set, and return
--   the closest match, if one is found. A minimum score of 0.33 is used. To
--   specify a custom threshold value, instead use 'findOneMin'.
findOne :: (FuzzySetMonad m)
  => Text
  -- ^ The string to search for
  -> m (Maybe FuzzyMatch)
  -- ^ The closest match, if one is found
findOne str = Simple.findOne str <$> _get

-- | Try to match the given string against the entries in the set using the
--   specified minimum score and return the string that most closely matches
--   the input, if a match is found.
closestMatchMin :: (FuzzySetMonad m)
  => Double
  -- ^ A minimum score
  -> Text
  -- ^ The string to search for
  -> m (Maybe Text)
  -- ^ The string most closely matching the input, if a match is found
closestMatchMin minScore str = Simple.closestMatchMin minScore str <$> _get

-- | Try to match the given string against the entries in the set, and return
--   the string that most closely matches the input, if a match is found. A
--   minimum score of 0.33 is used. To specify a custom threshold value,
--   instead use 'closestMatchMin'.
closestMatch :: (FuzzySetMonad m)
  => Text
  -- ^ The string to search for
  -> m (Maybe Text)
  -- ^ The string most closely matching the input, if a match is found
closestMatch str = Simple.closestMatch str <$> _get

-- | Return the number of entries in the set.
size :: (FuzzySetMonad m) => m Int
size = length <$> values

-- | Return a boolean indicating whether the set is empty.
isEmpty :: (FuzzySetMonad m) => m Bool
isEmpty = null <$> values
