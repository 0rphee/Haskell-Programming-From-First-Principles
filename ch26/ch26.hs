-- Chapter 26: Monad Transformers
{-# LANGUAGE InstanceSigs #-}

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
  fmap f m = undefined




