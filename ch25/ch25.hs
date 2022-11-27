-- Chapter 25: Composing Types

{-# LANGUAGE InstanceSigs #-}
import Control.Monad (join)

newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose $ pure $ pure a
  (Compose fga) <*> (Compose fgb) = Compose $ (<*>) <$> fga <*> fgb

------------------------------------------------------------------
-- 25.6 Exercises: Compose Instances

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = foldMap (foldMap f) fga


instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

------------------------------------------------------------------

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = bimap f g

  first :: (a -> b) -> p a c -> p b c
  first f = first f

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

------------------------------------------------------------------

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

------------------------------------------------------------------

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

------------------------------------------------------------------

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

------------------------------------------------------------------

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

------------------------------------------------------------------

data Quadriceps a b c d = Quadz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadz a b c d) = Quadz a b (f c) (g d)

------------------------------------------------------------------

instance Bifunctor Either where
  -- | Documentation fo bimap
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)

--

------------------------------------------------------------------
-- 25.7 Monad Transformers

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a = IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Applicative m => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) =
    IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure
  (Identity a) >>= fa = fa a

-- instance Monad m => Monad (IdentityT m) where
--   return = pure
--   (IdentityT ma) >>= f = IdentityT $ ma >>= ( runIdentityT . f)
  
-- Implementing the bind, step by step
instance (Monad m) => Monad (IdentityT m) where
  (>>=) :: IdentityT m a
        -> (a -> IdentityT m b)
        -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f


