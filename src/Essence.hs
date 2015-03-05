{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Essence where

-- Someone has done the same thing here
-- https://gist.github.com/k0001/4500051

import           Control.Applicative
import           Control.Monad.State hiding (sequence)
import           Data.Monoid hiding (Product)
import           Data.Foldable
import           Data.Traversable
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Functor.Product
import           Prelude hiding (sequence)

main = putStrLn "hello"

{-
-- Control.Applicative.Const
newtype Const b a = Const { unConst :: b }

instance Monoid b => Applicative (Const b) where
  pure = Const mzero
  x <*> y = Const (unConst mappend unConst y)
-}

{-
-- https://hackage.haskell.org/package/transformers-0.2.2.0/docs/src/Data-Functor-Product.html
data Product f g a = Pair (f a) (g a)
-}

{-
http://hackage.haskell.org/package/transformers-0.4.2.0/docs/Data-Functor-Compose.html#v:Compose
newtype Compose f g a = Compose { getCompose :: f (g a) }
-}

-- foldMapDefault
reduce :: (Monoid c, Traversable t) => (a -> c) -> t a -> c
reduce f = getConst . traverse (Const . f)

crush :: (Traversable t, Monoid m) => t m -> m
crush = reduce id

--

class Coerce a b | a -> b where
  up :: b -> a
  down :: a -> b

instance Coerce (Identity a) a where
  up = Identity
  down = runIdentity

instance Coerce (Const a b) a where
  up = Const
  down = getConst

instance (Coerce (m a) b, Coerce (n a) c) => Coerce (Product m n a) (b, c) where
  up (x, y) = Pair (up x) (up y)
  down (Pair a b) = (down a, down b)

instance (Functor m, Functor n, Coerce (m b) c, Coerce (n a) b) => Coerce (Compose m n a) c where
  up = Compose . fmap up . up
  down = down . fmap down . getCompose

instance Coerce (Maybe a) (Maybe a) where
  up = id
  down = id

instance Coerce (State s a) (s -> (a, s)) where
  up = StateT . (Identity .)
  down = runState

-- 4.1 Shape and contents

contentsBody :: a -> Const [a] b
contentsBody x = Const [x]

contents :: Traversable t => t a -> Const [a] (t b)
contents = traverse contentsBody

run :: (Coerce b c, Traversable t) => (t a -> b) -> t a -> c
run program = down . program

shapeBody :: a -> Identity ()
shapeBody _ = up ()

shape :: Traversable t => t a -> Identity (t ())
shape = traverse shapeBody

-- TODO Surely this exists?!?
x :: (Functor m, Functor n) => (a -> m b) -> (a -> n b) -> (a -> Product m n b)
x f g y = Pair (f y) (g y)

o :: (Functor m, Functor n) => (b -> n c) -> (a -> m b) -> (a -> Compose m n c)
o f g = Compose . fmap f . g

decomposeSlow :: Traversable t => t a -> (Product Identity (Const [a]) (t ()))
decomposeSlow = (x) shape contents

decompose :: Traversable t => t a -> (Product Identity (Const [a]) (t ()))
decompose = traverse (shapeBody `x` contentsBody)

reassembleBody :: () -> (Compose (State [a]) Maybe a)
reassembleBody =  up . takeHead
  where takeHead _ [] = (Nothing, [])
        takeHead _ (y : ys) = (Just y, ys)

reassemble :: Traversable t => t () -> (Compose (State [a]) Maybe (t a))
reassemble = traverse reassembleBody

runReassemble :: Traversable t => (t (), [a]) -> Maybe (t a)
runReassemble = fst . uncurry (run reassemble)

collect :: (Traversable t, Applicative m) => (a -> m ()) -> (a -> b) -> t a -> m (t b)
collect f g = traverse (\a -> (\() -> g a) <$> f a)

loop :: Traversable t => (a -> b) -> t a -> (State Integer (t b))
loop touch = collect (\_ -> get >>= put . (1 +)) touch

disperse :: (Traversable t, Applicative m) => m b -> (a -> b -> c) -> t a -> m (t c)
disperse mb g = traverse (\a -> g a <$> mb)

label :: Traversable t => t a -> State Integer (t Integer)
label = disperse step (curry snd)

step :: State Integer Integer
step = get >>= \n -> put (n + 1) >> pure n

-- 4.3 Backwards traversal

newtype Backwards m a = Backwards { runBackwards :: m a } deriving Functor

instance Applicative m => Applicative (Backwards m) where
  pure = Backwards . pure
  f <*> x = Backwards (flip ($) <$> runBackwards x <*> runBackwards f)

data AppAdapter m where
  AppAdapter :: Applicative (g m) => (forall a. m a -> g m a) -> (forall a. g m a -> m a) -> AppAdapter m

backwards :: Applicative m => AppAdapter m
backwards = AppAdapter Backwards runBackwards

ptraverse :: (Applicative m, Traversable t) => AppAdapter m -> (a -> m b) -> t a -> m (t b)
ptraverse (AppAdapter insert retrieve) f = retrieve . traverse (insert . f)

lebal :: (Traversable t) => t a -> State Integer (t Integer)
lebal = ptraverse backwards (const step)

-- -5.4 Sequential composition of monadic traversals

update1 :: a -> State Integer a
update1 x = get >>= \var -> put (var * 2) >> pure x

update2 :: a -> State Integer a
update2 x = get >>= \var -> put (var + 1) >> pure x

monadicUpdate1 :: (Traversable t) => t a -> State Integer (t a)
monadicUpdate1 = traverse update1 >=> traverse update2

monadicUpdate2 :: (Traversable t) => t a -> State Integer (t a)
monadicUpdate2 = traverse (update1 >=> update2)

applicativeUpdate1 :: (Traversable t) => t a -> State Integer (State Integer (t a))
applicativeUpdate1 = getCompose . (traverse update1 `o` traverse update2)

applicativeUpdate2 :: (Traversable t) => t a -> State Integer (State Integer (t a))
applicativeUpdate2 = getCompose . traverse (update1 `o` update2)


-- 5.5 No duplication of elements

newtype BadList a = BadList { unBadList :: [a] } deriving (Functor, Foldable, Show, Eq)

instance Traversable BadList where
  traverse _ (BadList []) = pure (BadList [])
  traverse f (BadList (x : xs)) = fmap BadList ((const (:)) <$> f x <*> f x <*> traverse f xs)

index :: Traversable t => t a -> (t Integer, Integer)
index xs = run label xs 0

newtype Pair a = P (a, a) deriving (Show, Eq, Functor)

instance Applicative Pair where
  pure a = P (a,a)
  P (f, f') <*> P (a, a') = P (f a, f' a')

instance Coerce (Pair a) (a,a) where
  down (P x) = x
  up = P

