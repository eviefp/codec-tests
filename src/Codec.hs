module Codec where

import Data.Functor.Invariant (Invariant2(invmap2))
import Data.Profunctor (Star(Star, runStar), dimap)
import Prelude
import Control.Category (Category)
import Control.Applicative (Alternative(..))
import qualified Control.Category as C

data Codec m n a b = Codec (Star m a b) (Star n b a)

-- Map over 'a' and 'b'.
instance (Functor m, Functor n) => Invariant2 (Codec m n) where
  invmap2
      :: (a -> c)
      -> (c -> a)
      -> (b -> d)
      -> (d -> b)
      -> Codec m n a b
      -> Codec m n c d
  invmap2 a2c c2a b2d d2b (Codec mab nba) =
    Codec (dimap c2a b2d mab) (dimap d2b a2c nba)

-- Map over 'm' and 'n'.
hoist
  :: (forall x. m x -> m' x)
  -> (forall x. n x -> n' x)
  -> Codec m n a b
  -> Codec m' n' a b
hoist f g (Codec mab nba) =
    Codec
        (Star $ f . runStar mab)
        (Star $ g . runStar nba)

-- Monadic composition for when short-circuing is fine.
instance (Monad m, Monad n) => Category (Codec m n) where
  id :: Codec m n a a
  id = Codec C.id C.id

  (.) :: Codec m n b c -> Codec m n a b -> Codec m n a c
  (.) (Codec mbc ncb) (Codec mab nba) =
      Codec (mbc C.. mab) (nba C.. ncb)

-- Manual composition for things like 'Validation'.
manualCompose
    :: forall m n a b c
    .  (forall x y. m x -> (x -> m y) -> m y)
    -> (forall x y. n x -> (x -> n y) -> n y)
    -> Codec m n b c
    -> Codec m n a b
    -> Codec m n a c
manualCompose bindM bindN (Codec mbc ncb) (Codec mab nba) =
    Codec (Star mac) (Star nca)
  where
    mac :: a -> m c
    mac a = runStar mab a `bindM` runStar mbc

    nca :: c -> n a
    nca c = runStar ncb c `bindN` runStar nba

(**)
    :: forall m n a b c
    .  Applicative m
    => Alternative n
    => Codec m n a b
    -> Codec m n a c
    -> Codec m n a (b, c)
(**) (Codec mab nba) (Codec mac nca) =
    Codec (Star mabc) (Star nbca)
  where
    mabc :: a -> m (b, c)
    mabc a = (,) <$> runStar mab a <*> runStar mac a

    nbca :: (b, c) -> n a
    nbca (b, c) = runStar nba b <|> runStar nca c

(//)
    :: forall m n a b c
    .  Alternative m
    => Codec m n a b
    -> Codec m n a c
    -> Codec m n a (Either b c)
(//) (Codec mab nba) (Codec mac nca) =
    Codec (Star mabc) (Star nbca)
  where
    mabc :: a -> m (Either b c)
    mabc a = Left <$> runStar mab a <|> Right <$> runStar mac a

    nbca :: Either b c -> n a
    nbca = either (runStar nba) (runStar nca)

-- But what about invariant2?
class Inapplicative f where
  inpure :: a -> b -> f a b
  (<**>) :: f (a -> b) (c -> d) -> f a c -> f b d -- ???

instance (Applicative m, Applicative n) => Inapplicative (Codec m n) where
  inpure :: a -> b -> Codec m n a b
  inpure a b = Codec (pure b) (pure a)

  (<**>)
      :: forall a b c d
      .  Codec m n (a -> b) (c -> d) -- Essentially an isomorphism under m and n, right?
      -> Codec m n a c -- so if a and c are the same thing
      -> Codec m n b d -- so should b and d
  Codec (Star ab2mcd) (Star cd2nab) <**> Codec (Star a2mc) (Star c2na) =
      Codec (Star mbd) (Star ndb)
    where
      {-
        ab2mcd :: (a -> b) -> m (c -> d)
        cd2nab :: (c -> d) -> n (a -> b)
        a2mc   :: a        -> m c
        c2na   :: c        -> n a
    -}
      someA :: a
      someA = undefined

      someC :: c
      someC = undefined

      mbd :: b -> m d
      mbd b = ab2mcd (const b) <*> a2mc someA

      ndb :: d -> n b
      ndb d = cd2nab (const d) <*> c2na someC
