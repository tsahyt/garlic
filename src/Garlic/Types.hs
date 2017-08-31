{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Garlic.Types
(
    Garlic,
    Event,
    Consumer,
    consume,
    ioConsumer,
    dbConsumer,

    stdout,

    Fetcher,
    fetch,
    dbFetcher,
    
    -- * Misc
    makeGetters,
)
where

import Control.Lens
import Control.Monad.Reader
import Reactive.Banana
import Reactive.Banana.Frameworks
import Database.Persist.Sql

-- | The monad the garlic application is running in.
type Garlic a = SqlPersistT MomentIO a

instance MonadMoment m => MonadMoment (ReaderT r m) where
    liftMoment = lift . liftMoment

-- | A consumer can eat an event, performing some specified action on each
-- occurrence.  Mostly used by views offering up ways to change themselves.
newtype Consumer a = Consumer { consume :: Event a -> Garlic () }
infixr 1 `consume`

-- | Produce a consumer from an IO action. For GUI operations.
ioConsumer :: (a -> IO ()) -> Consumer a
ioConsumer k = Consumer $ \e -> lift (reactimate (k <$> e))

stdout :: Consumer String
stdout = ioConsumer putStrLn

-- | Produce a consumer from a DB action. For DB operations.
dbConsumer :: (a -> SqlPersistT IO ()) -> Consumer a
dbConsumer k = Consumer $ \e -> do
    backend <- ask
    lift $ reactimate (flip runReaderT backend . k <$> e)

-- | A fetcher can produce an event stream from another event stream, performing
-- some action in the process to do the transformation.
newtype Fetcher a b = Fetcher { fetch :: Event a -> Garlic (Event b) }

-- | Perform DB action on event to obtain a new event.
dbFetcher :: (a -> SqlPersistT IO b) -> Fetcher a b
dbFetcher k = Fetcher $ \e -> do
    backend <- ask
    lift $ mapEventIO (\x -> runReaderT (k x) backend) e

-- | Read only lens generation for GUI records
makeGetters = makeLensesWith (set generateUpdateableOptics False lensRules)
