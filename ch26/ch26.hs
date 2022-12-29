-- Chapter 26: Monad Transformers
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Reader as R
import Control.Monad.Identity

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m ) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT $ pure $ pure x
  (<*>) :: Applicative m => MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  (>>=) :: MaybeT m a
        -> (a -> MaybeT m b)
        -> MaybeT m b
  (MaybeT ma) >>= famb = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT (famb y)

------------------------------------------------------------
-- 26.3 EitherT
-- Exercises: EitherT

newtype EitherT left m right =
  EitherT { runEitherT :: m (Either left right) }

-- 1. Write the Functor instance for EitherT

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT m) = EitherT $ (fmap . fmap) f m

-- 2. Write the Applicative instance for EitherT

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ pure (Right a)
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT mf) <*> (EitherT m) = EitherT $ (<*>) <$> mf <*> m

-- 3. Write the Monad instance for EitherT

instance Monad m => Monad (EitherT e m) where
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT ema) >>= f = EitherT $ do
    eith <- ema
    case eith of
      Left e -> return $ Left e
      Right b -> runEitherT (f b)

-- 4. Write the swapEitherT helper function fo EitherT

swapEither :: Either e a-> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m)
             => EitherT e m a
             -> EitherT a m e
swapEitherT (EitherT m) = EitherT (swapEither <$> m)

-- 5. Write the transformer variant of the either catamorphism

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT m) = do
  eith <- m
  case eith of
    Left a -> f a
    Right b -> g b

------------------------------------------------------------
-- 26.4 ReaderT

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a}

instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT rma) = ReaderT $ (f <$>) . rma

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT $ pure (pure x)
  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT f) <*> (ReaderT rma) = ReaderT $ (<*>) <$> f <*> rma

instance Monad m => Monad (ReaderT r m) where
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

------------------------------------------------------------
-- 26.5 StateT

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) = StateT $
    \r -> let monad = sma r
          in  fmap (\(a,s) -> (f a, s)) monad

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $
    \s -> pure (a, s)
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smf) <*> (StateT sma) = StateT $ \s -> do
    (fun, s') <- smf s
    (a, s'') <- sma s'
    pure (fun a, s'')

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'

------------------------------------------------------------
-- Exercise: Wrap It Up

embedded :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded =  MaybeT $ ExceptT $ ReaderT (const (pure $ Right (Just 1)))

------------------------------------------------------------
-- 26.9 MonadTrans
-- Exercises: Lift More

-- 1.
instance MonadTrans (EitherT e) where
  lift ma = EitherT $ liftM Right ma

-- 2.
instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift ma = StateT $ \s -> liftM (,s) ma

------------------------------------------------------------
-- 26.10 MonadIO
-- Exercises: Some Instances

-- 1. MaybeT
instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO ioa = MaybeT $ liftM Just (liftIO ioa)
  
-- 2. ReaderT
instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO ioa = ReaderT $ \_ -> liftIO ioa
  
-- 3. StateT
instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO :: IO a -> StateT s m a
  liftIO ioa = StateT $ \s -> liftM (,s) (liftIO ioa)  
  
------------------------------------------------------------
-- 26.10 Chapter Exercises
-- Write the code

-- 1. rDec is a function that should get its argument in the context
--    of Reader and return a value decremented by one.
  
rDec :: Num a => R.Reader a a
rDec = R.ReaderT $ \r -> Identity (r-1) 

-- 3. rShow is show, but in Reader.
rShow :: Show a => R.ReaderT a Identity String
rShow = R.ReaderT $ \r -> Identity (show r)
  
-- 5. rPrintAndInc will first print the input with a greeting, then
--    return the input incremented by one.
rPrintAndInc :: (Num a, Show a) => R.ReaderT a IO a
rPrintAndInc = R.ReaderT $ \r -> do
  putStrLn $ "Hello bud: " <> show r
  pure $ r + 1
  
-- 6. sPrintIncAccum first prints the input with a greeting, then puts
--    the incremented input as the new state, and returns the original 
--    input as a String.
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \a -> do
  putStrLn $ "Hi: " <> show a
  return (show a, a+1)
  
