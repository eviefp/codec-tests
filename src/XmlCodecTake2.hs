{-# language ExplicitForAll #-}

module XmlCodecTake2 where

import Codec
import Control.Applicative
    (Alternative, empty)
import Control.Category
    (id, (>>>))
import Data.Functor.Identity
    (Identity, runIdentity)
import Data.Functor.Invariant
    (Invariant2 (invmap2))
import Data.Profunctor
    (Star (Star))
import Prelude                hiding
    (id)

type XmlCodec a b = Codec Maybe Identity a b

data XmlNode
    = Node String [ Attribute ] [ XmlNode ]
    | Text String
    deriving stock (Eq, Show)

data Attribute = Attribute String String
    deriving stock (Eq, Show)

example :: XmlNode
example =
    Node
        "parent"
        [ Attribute "key1" "value1"
        , Attribute "key2" "value2"
        ]
        []

example' :: XmlNode
example' =
    Node
        "parent"
        [ Attribute "key1" "value1"
        , Attribute "key2" "value2"
        , Attribute "key3" "value3"
        ]
        []

element :: XmlCodec XmlNode (String, ([Attribute], [XmlNode]))
element = mkCodec decode encode
  where
    decode :: XmlNode -> Maybe (String, ([Attribute], [XmlNode]))
    decode (Node tag attrs xs) = Just (tag, (attrs, xs))
    decode (Text _)            = Nothing

    encode :: (String, ([Attribute], [XmlNode])) -> Identity XmlNode
    encode (tag, (attrs, xs)) = pure $ Node tag attrs xs

element'
    :: forall a b c x
     . XmlCodec String a
    -> XmlCodec [Attribute] b
    -> XmlCodec [XmlNode] c
    -> ((a, b, c) -> x)
    -> (x -> (a, b, c))
    -> XmlCodec XmlNode x
element' aCodec bCodec cCodec to from = element >>> go'
  where
    go :: XmlCodec (String, ([Attribute], [XmlNode])) (a, (b, c))
    go = aCodec ^^^ (bCodec ^^^ cCodec)

    to' :: (a, (b, c)) -> x
    to' (a, (b, c)) = to (a, b, c)

    from' :: x -> (a, (b, c))
    from' x =
        case from x of
            (a, b, c) -> (a, (b, c))

    go' :: XmlCodec (String, ([Attribute], [XmlNode])) x
    go' = invmap2 id id to' from' go

assertLeft
    :: forall a m n x
    .  Eq a
    => Alternative m
    => Applicative n
    => a
    -> Codec m n ( a, x ) x
assertLeft a = mkCodec decode encode
  where
    decode :: (a, x) -> m x
    decode (a', x) = if a == a' then pure x else empty

    encode :: x -> n (a, x)
    encode x = pure (a, x)

(#>>)
    :: forall y a m n x
    .  Eq a
    => Monad m
    => Alternative m
    => Monad n
    => Codec m n y (a, x)
    -> a
    -> Codec m n y x
(#>>) left a = left >>> assertLeft a

(>>#)
    :: forall y a m n x
    .  Eq a
    => Monad m
    => Alternative m
    => Monad n
    => Codec m n y (x, a)
    -> a
    -> Codec m n y x
(>>#) left a = left >>> assertRight a

assertRight
    :: forall a m n x
    .  Eq a
    => Alternative m
    => Applicative n
    => a
    -> Codec m n (x, a) x
assertRight a = mkCodec decode encode
  where
    decode :: (x, a) -> m x
    decode (x, a') = if a == a' then pure x else empty

    encode :: x -> n (x, a)
    encode x = pure (x, a)

assert
    :: forall m n a
     . Eq a
    => Alternative m
    => Applicative n
    => a
    -> Codec m n a ()
assert a = mkCodec decode encode
  where
    decode :: a -> m ()
    decode a' = if a == a' then pure () else empty

    encode :: () -> n a
    encode _ = pure a

attribute :: XmlCodec [Attribute] Attribute
attribute = mkCodec decode encode
  where
    decode :: [Attribute] -> Maybe Attribute
    decode [x] = Just x
    decode _   = Nothing

    encode :: Attribute -> Identity [Attribute]
    encode = pure . pure

attributes :: XmlCodec [Attribute] [(String, String)]
attributes = mkCodec decode encode
  where
    decode :: [Attribute] -> Maybe [(String, String)]
    decode = Just . fmap (\(Attribute k v) -> (k, v))

    encode :: [(String, String)] -> Identity [Attribute]
    encode = pure . fmap (uncurry Attribute)

attributeValue :: String -> XmlCodec Attribute String
attributeValue key = attributeText #>> key

listCons :: forall a. XmlCodec [a] (a, [a])
listCons = mkCodec decode encode
  where
    decode :: [a] -> Maybe (a, [a])
    decode = \case
        [] -> Nothing
        (a:as) -> Just (a, as)

    encode :: (a, [a]) -> Identity [a]
    encode (a, as) = pure (a:as)

assertMaybe :: forall a m n. Alternative m => Applicative n => Codec m n (Maybe a) a
assertMaybe = Codec (Star fwd) (Star bwd)
  where
    fwd :: Maybe a -> m a
    fwd = maybe empty pure
    bwd :: a -> n (Maybe a)
    bwd = pure . Just

attributeText :: XmlCodec Attribute (String, String)
attributeText = mkCodec decode encode
  where
    decode :: Attribute -> Maybe (String, String)
    decode (Attribute key value) = Just (key, value)

    encode :: (String, String) -> Identity Attribute
    encode (key, value) = pure $ Attribute key value

-------------------------------------------------------------------------------
-- Example

getKeyAttribute :: XmlCodec XmlNode (String, String)
getKeyAttribute =
    -- (firstAttribute, rest) <- tuple
    -- key1 <- left firstAttribute
    -- key2 <- right rest
    -- pure (key1, key2)
    tuple >>> (left ^^^ right)
  where
    tuple :: XmlCodec XmlNode (Attribute, [Attribute])
    tuple =
        element
            #>> "parent"
            >># []
            >>> listCons

    left :: XmlCodec Attribute String
    left = attributeText >>> assertLeft "key1"

    right :: XmlCodec [Attribute] String
    right =
        listCons
            >># []
            >>> attributeText
            #>> "key2"

data Thing = Thing String String
    deriving stock Show

thingCodec :: XmlCodec XmlNode Thing
thingCodec = invmap2 id id to from getKeyAttribute
  where
    to :: (String, String) -> Thing
    to (a, b)  = Thing a b

    from :: Thing -> (String, String)
    from (Thing a b) = (a, b)

(>:>) :: forall a b c. XmlCodec a b -> XmlCodec [a] c -> XmlCodec [a] (b, c)
(>:>) (Codec (Star mab) (Star nba)) (Codec (Star mac) (Star nca)) =
    mkCodec decode encode
  where
    decode :: [a] -> Maybe (b, c)
    decode = \case
        [] -> Nothing
        (a:as) -> (,) <$> mab a <*> mac as

    encode :: (b, c) -> Identity [a]
    encode (b, c) = (:) <$> nba b <*> nca c

(>:#) :: Eq a => XmlCodec a b -> XmlCodec a c -> XmlCodec [a] (b, c)
(>:#) bCodec cCodec = invmap2 id id to from $ bCodec >:> (cCodec >:> assert [])
  where
    to :: (b, (c, ())) -> (b, c)
    to (b, (c, _)) = (b, c)

    from :: (b, c) -> (b, (c, ()))
    from (b, c) = (b, (c, ()))

data Thing' = Thing' String String String
    deriving stock Show

thingCodec' :: XmlCodec XmlNode Thing'
thingCodec' =
    element'
        (assert "parent")
        (attributeValue "key1" >:> (attributeValue "key2" >:# attributeValue "key3"))
        (assert [])
        to
        from
  where
    to :: ((), (String, (String, String)), ()) -> Thing'
    to (_, (s1, (s2, s3)), _) = Thing' s1 s2 s3

    from :: Thing' -> ((), (String, (String, String)), ())
    from (Thing' s1 s2 s3) = ((), (s1, (s2, s3)), ())

runDecoder :: XmlCodec a b -> a -> Maybe b
runDecoder (Codec (Star decode) _) = decode

runEncoder ::  XmlCodec XmlNode a -> a -> XmlNode
runEncoder (Codec _ (Star encode)) = runIdentity . encode
