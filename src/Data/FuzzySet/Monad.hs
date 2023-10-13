{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.FuzzySet.Monad
  ( add
  , findMin
  , values
  , addMany
  , find
  , findClosest
  , findClosestMin
  , size
  , isEmpty
  , FuzzySetT (..)
  , runFuzzySetT
  , runDefaultFuzzySetT
  , FuzzySetMonad
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, get, gets)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Writer (WriterT)
import Data.FuzzySet.Internal (FuzzyMatch, FuzzySet, emptySet)
import qualified Data.FuzzySet.Internal as FuzzySet
import Data.FuzzySet.Utils ((<$$$>), (<$$>))
import Data.Text (Text)

newtype FuzzySetT m a = FuzzySetT {getFuzzySetT :: StateT FuzzySet m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState FuzzySet
    , MonadIO
    )

runFuzzySetT :: (Monad m) => FuzzySetT m a -> Int -> Int -> Bool -> m a
runFuzzySetT value = evalStateT (getFuzzySetT value) <$$$> emptySet

runDefaultFuzzySetT :: (Monad m) => FuzzySetT m a -> m a
runDefaultFuzzySetT value = runFuzzySetT value 2 3 True

class (Monad m) => FuzzySetMonad m where
  add :: Text -> m Bool
  findMin :: Double -> Text -> m [FuzzyMatch]
  values :: m [Text]
  _get :: m FuzzySet

instance MonadTrans FuzzySetT where
  lift = FuzzySetT . lift

instance (Monad m) => FuzzySetMonad (FuzzySetT m) where
  add = FuzzySet.add_
  findMin minScore str = gets (FuzzySet.findMin minScore str)
  values = gets FuzzySet.values
  _get = get

instance (FuzzySetMonad m) => FuzzySetMonad (StateT s m) where
  add = lift . add
  findMin = lift <$$> findMin
  values = lift values
  _get = lift _get

instance (FuzzySetMonad m) => FuzzySetMonad (ExceptT e m) where
  add = lift . add
  findMin = lift <$$> findMin
  values = lift values
  _get = lift _get

instance (FuzzySetMonad m) => FuzzySetMonad (ReaderT r m) where
  add = lift . add
  findMin = lift <$$> findMin
  values = lift values
  _get = lift _get

instance (FuzzySetMonad m, Monoid w) => FuzzySetMonad (WriterT w m) where
  add = lift . add
  findMin = lift <$$> findMin
  values = lift values
  _get = lift _get

instance (FuzzySetMonad m) => FuzzySetMonad (MaybeT m) where
  add = lift . add
  findMin = lift <$$> findMin
  values = lift values
  _get = lift _get

instance (FuzzySetMonad m) => FuzzySetMonad (ContT r m) where
  add = lift . add
  findMin = lift <$$> findMin
  values = lift values
  _get = lift _get

instance (FuzzySetMonad m) => FuzzySetMonad (SelectT r m) where
  add = lift . add
  findMin = lift <$$> findMin
  values = lift values
  _get = lift _get

addMany :: (MonadState FuzzySet m) => [Text] -> m [Text]
addMany = FuzzySet.addMany_

find :: (FuzzySetMonad m) => Text -> m [FuzzyMatch]
find str = FuzzySet.find str <$> _get

findClosestMin :: (FuzzySetMonad m) => Double -> Text -> m (Maybe FuzzyMatch)
findClosestMin minScore str = FuzzySet.findClosestMin minScore str <$> _get

findClosest :: (FuzzySetMonad m) => Text -> m (Maybe FuzzyMatch)
findClosest str = FuzzySet.findClosest str <$> _get

size :: (FuzzySetMonad m) => m Int
size = length <$> values

isEmpty :: (FuzzySetMonad m) => m Bool
isEmpty = null <$> values
