{-# LANGUAGE FlexibleContexts #-}
module WordCount where

import           Essence
import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Writer hiding (Product)
import           Control.Monad.Reader
import           Data.Char
import           Data.Monoid hiding (Product)
import           Data.Foldable
import           Data.Traversable hiding (mapM)
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Functor.Product

instance Monoid Integer where
  mempty = 0
  mappend = (+)

type Count = Const Integer

count :: a -> Count b
count _ = Const 1

cciBody :: Char -> Count a
cciBody = count

cci :: String -> Count [a]
cci = traverse cciBody

test :: Bool -> Integer
test b = if b then 1 else 0

lciBody :: Char -> Count a
lciBody c = up (test (c == '\n'))

lci :: String -> Count [a]
lci = traverse lciBody

wciBody :: Char -> Compose (State Bool) Count a
wciBody c = up (updateState c) where
  updateState :: Char -> Bool -> (Integer, Bool)
  updateState c w = let s = not (isSpace c) in (test (not w && s), s)

wci :: String -> Compose (State Bool) Count [a]
wci = traverse wciBody

runWci :: String -> Integer
runWci s = fst (run wci s False)

clci :: String -> Product Count Count [a]
-- clciSlow = cci `x` lci
clci = traverse (cciBody `x` lciBody)

clwci :: String -> (Product (Product Count Count) (Compose (State Bool) Count) [a])
clwci = traverse (cciBody `x` lciBody `x` wciBody)

quiBody :: Char -> Pair Bool
quiBody c = P (c == 'q', c == 'u')

qui :: String -> Pair [Bool]
qui = traverse quiBody

ccqui :: String -> Product Count Pair [Bool]
ccqui = traverse (cciBody `x` quiBody)

wcqui :: String -> Product Pair (Compose (State Bool) Count) [Bool]
wcqui = traverse (quiBody `x` wciBody)

wcqui' :: String -> Compose (Product Identity (Compose (State Bool) Count)) Pair [Bool]
wcqui' = traverse (quiBody `o` (Identity `x` wciBody))

-- 6.3 Modular iterations, monadically

ccmBody :: Char -> Writer Integer Char
ccmBody c = tell 1 >> pure c

ccm :: String -> Writer Integer String
ccm = mapM ccmBody

lcmBody :: Char -> Writer Integer Char
lcmBody c = tell (test (c == '\n')) >> pure c

lcm :: String -> Writer Integer String
lcm = mapM lcmBody

wcmBody :: Char -> State (Integer, Bool) Char
wcmBody c = do
  let s = not (isSpace c)
  (n, w) <- get
  put (n + test (not w && s), s)
  pure c

wcm :: String  -> State (Integer, Bool) String
wcm = mapM wcmBody

clwcm :: String -> Product (Product (Writer Integer) (Writer Integer)) (State (Integer, Bool)) String
clwcm = mapM (ccmBody `x` lcmBody `x` wcmBody)

(<=<^) :: (Monad m, MonadTrans t, Monad (t m))
       => (b -> t m c) -> (a -> m b) -> (a -> t m c)
ktf <=<^ kg = ktf <=< (lift . kg)

(^<=<) :: (Monad m, MonadTrans t, Monad (t m))
       => (b -> m c) -> (a -> t m b) -> (a -> t m c)
kf ^<=< ktg = (lift . kf) <=< ktg

qumBody :: Char -> Reader Bool Bool
qumBody c = do
  b <- ask
  pure $ if b then c == 'q' else c == 'u'

-- (Notice that the definition is identical; only the type has changed.)
wcmBody' :: MonadState (Integer, Bool) m => Char -> m Char
wcmBody' c = do
  let s = not (isSpace c)
  (n,w) <- get
  put (n + (test (not w && s)), s)
  return c

quwcm :: String -> StateT (Integer, Bool) (Reader Bool) [Bool]
quwcm = mapM (qumBody ^<=< wcmBody') -- == mapM qumBody ^<=< mapM wcmBody'

