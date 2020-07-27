module Codec where

import Control.Monad(join)
import Data.Functor.Invariant (Invariant2(invmap2))
import Data.Profunctor (Star(Star, runStar), dimap)
import Prelude
import Control.Category (Category)
import Control.Applicative (Alternative(..))
import qualified Control.Category as C

-- | A codec is a bi-directional parser. It's encoded using Star, but it's
-- essentially a pair of functions (i -> m o) and (o -> n i) for decoding and
-- encoding.
--
-- The 'm :: * -> *' will most likely be something like (Either Err) or
-- (Validation Err).
-- The 'n :: * -> *' will usually be Identity, so consider removing it.
-- 'i :: *' is the input type.
-- 'o :: *' is the output type.
data Codec m n i o = Codec (Star m i o) (Star n o i)

-- Map over 'i' and 'o'.
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

manualIdentity
    :: Applicative m
    => Applicative n
    => Codec m n a a
manualIdentity = Codec (Star pure) (Star pure)

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

mkCodec :: (a -> m b) -> (b -> n a) -> Codec m n a b
mkCodec decoder encoder = Codec (Star decoder) (Star encoder)

(***)
    :: forall m n a b c
    .  Applicative m
    => Applicative n
    => Semigroup a
    => Codec m n a b
    -> Codec m n a c
    -> Codec m n a (b, c)
(***) (Codec mab nba) (Codec mac nca) =
    Codec (Star mabc) (Star nbca)
  where
    mabc :: a -> m (b, c)
    mabc a = (,) <$> runStar mab a <*> runStar mac a

    nbca :: (b, c) -> n a
    nbca (b, c) = (<>) <$> runStar nba b <*> runStar nca c

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

-- This is not very useful :(
both
    :: forall m n a b c d
    .  Monad m
    => Monad n
    => Alternative m
    => Codec m n a (b, c)
    -> Codec m n b d
    -> Codec m n c d
    -> Codec m n a d
both
  (Codec (Star a2mbc) (Star bc2na))
  (Codec (Star b2md) (Star d2nb))
  (Codec (Star c2md) (Star d2nc)) =
    Codec (Star a2md) (Star d2na)
  where
    a2md :: a -> m d
    a2md a = a2mbc a >>= \(b, c) -> b2md b <|> c2md c

    d2na :: d -> n a
    d2na d = join $ curry bc2na <$> d2nb d <*> d2nc d

(^^^)
    :: forall m n b c d e
    .  Applicative m
    => Applicative n
    => Codec m n b d
    -> Codec m n c e
    -> Codec m n (b, c) (d, e)
(^^^) (Codec (Star b2md) (Star d2nb)) (Codec (Star c2me) (Star e2nc)) =
    Codec (Star bc2mde) (Star de2nbc)
  where
    bc2mde :: (b, c) -> m (d, e)
    bc2mde (b, c) = (,) <$> b2md b <*> c2me c

    de2nbc :: (d, e) -> n (b, c)
    de2nbc (d, e) = (,) <$> d2nb d <*> e2nc e

-- Codec m n Attribute Currency
-- Codec m n Attribute Bank

-- Codec m n Node [Attribute]
--    -> Codec m n [Attribute] (Attribute, Attribute)

-- Codec m n i [Attribute] *** Codec m n i [XmlNode]
--     = Codec m n i ([Attribute], [XmlNode]
-- Codec m n [Attribute] T `thing` Codec m n [XmlNode] U
--     = Codec m n ([Attribute], [XmlNode]) T

-- Currency from Attribute
-- Amount from XmlNode

-- <payment currency="USD">123</payment>
-- <currency from="USD" bank="SomeBank">123</currency>

-- try this out:
-- Codec m n a b -> Codec m n c d -> Codec m n (a, c) (b, d)

class Inapplicative f where
  inpure :: a -> b -> f a b
  -- (<**>) :: f (a -> b) (c -> d) -> f x c -> f x d -- nope :(
  lift2 :: ((a, b) -> c) -> (c -> (a, b)) -> f x a -> f x b -> f x c

instance (Applicative m, Alternative n) => Inapplicative (Codec m n) where
  inpure :: a -> b -> Codec m n a b
  inpure a b = Codec (pure b) (pure a)

  lift2
      :: forall a b c x
      .  ((a, b) -> c)
      -> (c -> (a, b))
      -> Codec m n x a
      -> Codec m n x b
      -> Codec m n x c
  lift2 ab2c c2ab (Codec (Star x2ma) (Star a2nx)) (Codec (Star x2mb) (Star (b2nx))) =
      Codec (Star x2mc) (Star c2nx)
    where
      x2mc :: x -> m c
      x2mc x = curry ab2c <$> x2ma x <*> x2mb x

      c2nx :: c -> n x
      c2nx c =
        let (a, b) = c2ab c
        in a2nx a <|> b2nx b
