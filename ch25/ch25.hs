
import Control.Applicative (Applicative(liftA2))

newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose $ pure $ pure a
  (Compose fga) <*> (Compose fgb) = Compose $ ((<*>) <$> fga) <*> fgb


main = print $ (5*) <$> Compose (Just (Compose [Just 1, Nothing]))





