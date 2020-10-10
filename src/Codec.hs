{-# language ExplicitForAll #-}
{-# language InstanceSigs   #-}

module Codec where

import Control.Applicative
    (Alternative (..))
import Control.Category
import Data.Functor.Invariant
    (Invariant2 (invmap2))
import Data.Profunctor
    (Star (Star, runStar), dimap)
import Prelude                hiding
    (id, (.))

-- Exactly isomorphic to ReaderT, except the argument order is different
-- newtype ReaderT a f b = ReaderT { runReaderT :: a -> f b }
-- newtype Star    f a b = Star    { runStar    :: a -> f b }

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

-- Simple helper function to create codecs.
mkCodec :: (a -> m b) -> (b -> n a) -> Codec m n a b
mkCodec decoder encoder = Codec (Star decoder) (Star encoder)

-- Map over 'i' and 'o'.
instance ( Functor m, Functor n ) => Invariant2 (Codec m n) where
    invmap2
        :: (a -> c) -> (c -> a)
        -> (b -> d) -> (d -> b)
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
    Codec (Star $ f . runStar mab) (Star $ g . runStar nba)

-- Monadic composition for when short-circuing is fine.
instance (Monad m, Monad n) => Category (Codec m n) where
    id :: Codec m n a a
    id = Codec id id

    (.) :: Codec m n b c -> Codec m n a b -> Codec m n a c
    (.) (Codec mbc ncb) (Codec mab nba) = Codec (mbc . mab) (nba . ncb)

-- Since Category requires 'm' and 'n' to be Monads, we provide stand-alone
-- functions for identity and composition (pure and bind) for when
-- short-circuiting is not acceptable (e.g. 'Validation' parsing).
manualIdentity :: Applicative m => Applicative n => Codec m n a a
manualIdentity = Codec (Star pure) (Star pure)

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
    mabc :: a -> m ( b, c )
    mabc a = (,) <$> runStar mab a <*> runStar mac a

    nbca :: ( b, c ) -> n a
    nbca ( b, c ) = (<>) <$> runStar nba b <*> runStar nca c

-- Try parsing 'b' and tag it with 'Left' if it succeeds. Parse it with 'c'
-- otherwise and tag it with 'Right'.
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

(^^^)
    :: forall m n b c d e
    .  Applicative m
    => Applicative n
    => Codec m n b d
    -> Codec m n c e
    -> Codec m n ( b, c ) ( d, e )
(^^^) (Codec (Star b2md) (Star d2nb)) (Codec (Star c2me) (Star e2nc)) =
    Codec (Star bc2mde) (Star de2nbc)
  where
    bc2mde :: ( b, c ) -> m ( d, e )
    bc2mde ( b, c ) = (,) <$> b2md b <*> c2me c

    de2nbc :: ( d, e ) -> n ( b, c )
    de2nbc ( d, e ) = (,) <$> d2nb d <*> e2nc e

-- Think about whether we could return an isomorphism instead of ->.
class Inapplicative f where
    -- inpure :: a -> b -> f a b
    -- (<**>) :: f (a -> b) (c -> d) -> f x c -> f x d -- nope :(
    lift2 :: ((a, b) -> c) -> (c -> (a, b)) -> f a -> f b -> f c

  -- good ol' lift2 :: (a -> b -> c) -> f a -> f b -> f c
  -- good ol' <*>   :: f (a -> b) -> f a -> f b
instance (Applicative m, Alternative n) => Inapplicative (Codec m n x) where
    -- inpure :: a -> b -> Codec m n a b
    -- inpure a b = Codec (pure b) (pure a)

    lift2
        :: forall a b c
        .  ((a, b) -> c)
        -> (c -> (a, b))
        -> Codec m n x a
        -> Codec m n x b
        -> Codec m n x c
    lift2
        ab2c
        c2ab
        (Codec (Star x2ma) (Star a2nx))
        (Codec (Star x2mb) (Star b2nx))
      =
        Codec (Star x2mc) (Star c2nx)
      where
        x2mc :: x -> m c
        x2mc x = curry ab2c <$> x2ma x <*> x2mb x

        c2nx :: c -> n x
        c2nx c = let (a, b) = c2ab c in a2nx a <|> b2nx b

