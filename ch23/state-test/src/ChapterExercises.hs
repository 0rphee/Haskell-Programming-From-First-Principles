module ChapterExercises where

newtype State s a =  State { myrunState :: s -> (a, s) }  

instance Functor (State s) where 
  fmap f (State g) = State $ \r -> let (a, b) = g r   
                               in (f a, b)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (State f) <*> (State g) = State 
    $ \r -> let (l, n) = f r 
                (a, n') = g n
            in (l a, n')

instance Monad (State s) where
  (State f) >>= g = State $ 
    \ s -> let (a, s') = f s
               (State sb) = g a
           in sb s'

-- 23.8 Chapter Exercises
-- 1. 
myget :: State s s 
myget = State (\s -> (s, s))

-- 2.
myput :: s -> State s ()
myput s =  State $ const ((), s)

-- 3.
myexec :: State s a -> s -> s
myexec (State sa) s = sr
  where (_, sr) = sa s

-- 4. 
eval :: State s a -> s -> a
eval (State sa) s = let (a,_) = sa s
                    in a

-- 5.
mymodify :: (s -> s) -> State s ()  
mymodify s = State $ \n -> ((), s n)


