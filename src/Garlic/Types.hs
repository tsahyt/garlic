{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Garlic.Types
(
    Garlic,
    Event,
    Behavior,
    Consumer,
    consume,
    consumeMaybe,
    ioConsumer,
    dbConsumer,

    stdout,

    Fetcher,
    fetch,
    fetchThrough,
    dynamicFetcher,
    dbFetcher,
    ioFetcher,
    filterMaybe,
    
    -- * Misc
    makeGetters,
    (<:>),
    plainChanges,
    (<$$>),
    (<$$),
    mtext,
    parseNum,
    spread,
    delay,
    unionl
)
where

import Control.Lens
import Control.Category
import Control.Monad.Reader
import Data.Functor.Compose
import Data.Text (Text)
import Data.Maybe
import Text.Read (readMaybe)
import Reactive.Banana
import Data.Functor.Contravariant.Divisible
import Reactive.Banana.Frameworks
import Database.Persist.Sql

import Prelude hiding ((.), id)

import qualified Data.Text as T

-- | The monad the garlic application is running in.
type Garlic a = SqlPersistT MomentIO a

instance MonadMoment m => MonadMoment (ReaderT r m) where
    liftMoment = lift . liftMoment

-- | A consumer can eat an event, performing some specified action on each
-- occurrence.  Mostly used by views offering up ways to change themselves.
newtype Consumer a = Consumer { consume :: Event a -> Garlic () }
infixr 1 `consume`

instance Contravariant Consumer where
    contramap f x = Consumer $ \e -> consume x (fmap f e)

instance Divisible Consumer where
    divide f b c = Consumer $ \e ->
        let (eb, ec) = (fmap (fst . f) e, fmap (snd . f) e)
         in consume b eb >> consume c ec

    conquer = Consumer (const (return ()))

instance Decidable Consumer where
    choose f x y = Consumer $ \e ->
        let (ls, rs) = split $ f <$> e
         in consume x ls >> consume y rs

    lose _ = mempty

instance Monoid (Consumer a) where
    mempty = conquer
    mappend = divide (\x -> (x,x))

consumeMaybe :: Consumer a -> Event (Maybe a) -> Garlic ()
consumeMaybe x = consume (choose go conquer x)
    where go Nothing  = Left ()
          go (Just a) = Right a
infixr 1 `consumeMaybe`

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
infixr 1 `fetch`

instance Profunctor Fetcher where
    dimap f g k = Fetcher $ \a ->
        let b = fmap f a
         in g <$$> fetch k b

instance Category Fetcher where
    id = Fetcher pure
    a . b = Fetcher $ fetch a <=< fetch b

-- | Can be used to register new handlers/events dynamically in a fetcher.
dynamicFetcher :: (a -> MomentIO b) -> Fetcher a b
dynamicFetcher k = Fetcher $ \e -> lift $ execute (k <$> e)

-- | Perform DB action on event to obtain a new event.
dbFetcher :: (a -> SqlPersistT IO b) -> Fetcher a b
dbFetcher k = Fetcher $ \e -> do
    backend <- ask
    lift $ mapEventIO (\x -> runReaderT (k x) backend) e

ioFetcher :: (a -> IO b) -> Fetcher a b
ioFetcher k = Fetcher $ \e -> lift $ mapEventIO k e

filterMaybe :: Fetcher a (Maybe b) -> Fetcher a b
filterMaybe (Fetcher x) = Fetcher (filterJust <$$> x)

fetchThrough :: Fetcher a b -> Fetcher a (a,b)
fetchThrough k = Fetcher $ \a -> do
    aB <- stepper (error "impossible") a
    b  <- fetch k a
    pure $ ((,) <$> aB) <@> b

-- | Read only lens generation for GUI records
makeGetters = makeLensesWith (set generateUpdateableOptics False lensRules)

-- | Left-biased union
(<:>) :: Event a -> Event a -> Event a
a <:> b = unionWith const a b
infixr 2 <:>

plainChanges :: Behavior a -> Garlic (Event a)
plainChanges b = lift $ do
    (e, handle) <- newEvent
    eb <- changes b
    reactimate' $ fmap handle <$> eb
    return e

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = getCompose . fmap f . Compose $ x

(<$$) :: (Functor f, Functor g) => b -> f (g a) -> f (g b)
f <$$ x = getCompose . (f <$) . Compose $ x

mtext :: Text -> Maybe Text
mtext x  = if T.null x then Nothing else Just x

parseNum :: (Num a, Read a) => Text -> a
parseNum = fromMaybe 0 . readMaybe . T.unpack

-- | Converts an event where each occurrence is a list of values to an event
-- stream in which the values are happening sequentially. This of course does
-- not all happen at the same time. The list is traversed left to right.
spread :: Event [a] -> Garlic (Event a)
spread evs = lift $ do
    (e, handle) <- newEvent 
    reactimate $ mapM_ handle <$> evs
    pure e

delay :: Event a -> Garlic (Event a)
delay e = lift $ do
    (e', handle) <- newEvent
    reactimate $ handle <$> e
    pure e'

-- | Left-biased event union
unionl :: [Event a] -> Event a
unionl = foldr (<:>) never
