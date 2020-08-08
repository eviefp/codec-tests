{-# LANGUAGE ExplicitForAll #-}

module XmlCodecTake2 where

import Control.Applicative ( Alternative, empty )
import Data.Functor.Invariant ( Invariant2(invmap2) )
import Codec
import Prelude
import Data.Functor.Identity ( Identity )
import Control.Category ( (>>>) )
import Data.Profunctor ( Star(Star) )

type XmlCodec a b = Codec Maybe Identity a b

data XmlNode = Node String [ Attribute ] [ XmlNode ] | Text String
    deriving stock Eq

data Attribute = Attribute String String
    deriving stock Eq

example :: XmlNode
example =
    Node
        "parent"
        [ Attribute "key1" "value1"
        , Attribute "key2" "value2"
        ]
        []

element :: XmlCodec XmlNode ( String, ([ Attribute ], [XmlNode] ))
element = mkCodec decode encode
  where
    decode :: XmlNode -> Maybe ( String, ([ Attribute ], [XmlNode] ))
    decode (Node tag attrs xs) = Just ( tag, ((attrs), xs))
    decode (Text _) = Nothing

    encode :: ( String, ([ Attribute ], [XmlNode] )) -> Identity XmlNode
    encode ( tag, (attrs, xs)) = pure $ Node tag attrs xs

assertLeft :: forall a m n x.
    Eq a => Alternative m => Applicative n => a -> Codec m n ( a, x ) x
assertLeft a = mkCodec decode encode
  where
    decode :: ( a, x ) -> m x
    decode ( a', x ) = if a == a' then pure x else empty

    encode :: x -> n ( a, x )
    encode x = pure ( a, x )

assertRight :: forall a m n x.
    Eq a => Alternative m => Applicative n => a -> Codec m n ( x, a ) x
assertRight a = mkCodec decode encode
  where
    decode :: ( x, a ) -> m x
    decode ( x, a' ) = if a == a' then pure x else empty

    encode :: x -> n ( x, a )
    encode x = pure ( x, a )

attribute :: XmlCodec [ Attribute ] Attribute
attribute = mkCodec decode encode
  where
    decode :: [ Attribute ] -> Maybe Attribute
    decode [ x ] = Just x
    decode _ = Nothing

    encode :: Attribute -> Identity [ Attribute ]
    encode = pure . pure

attributes :: XmlCodec [Attribute] [(String, String)]
attributes = mkCodec decode encode
  where
    decode :: [ Attribute ] -> Maybe [(String, String)]
    decode = Just . fmap (\(Attribute k v) -> (k, v))

    encode :: [(String, String)] -> Identity [Attribute]
    encode = pure . fmap (uncurry Attribute)

listCons :: forall a. XmlCodec [a] (a, [a])
listCons = mkCodec decode encode
  where
    decode :: [a] -> Maybe (a, [a])
    decode = \case
        [] -> Nothing
        (a:as) -> Just (a, as)

    encode :: (a, [a]) -> Identity [a]
    encode (a, as) = pure (a:as)

assert :: forall a m n. Alternative m => Applicative n => Codec m n (Maybe a) a
assert = Codec (Star fwd) (Star bwd)
  where
    fwd :: Maybe a -> m a
    fwd = maybe empty pure
    bwd :: a -> n (Maybe a)
    bwd = pure . Just

attributeText :: XmlCodec Attribute ( String, String )
attributeText = mkCodec decode encode
  where
    decode :: Attribute -> Maybe ( String, String )
    decode (Attribute key value) = Just ( key, value )

    encode :: ( String, String ) -> Identity Attribute
    encode ( key, value ) = pure $ Attribute key value

data Thing = Thing String String
    deriving stock Show

getKeyAttribute :: XmlCodec XmlNode ( String, String  )
getKeyAttribute =
    tuple >>> (left ^^^ right)
  where
    tuple :: XmlCodec XmlNode (Attribute, [Attribute])
    tuple =
        element
            >>> assertLeft "parent"
            >>> assertRight []
            >>> listCons

    left :: XmlCodec Attribute String
    left = attributeText >>> assertLeft "key1"

    right :: XmlCodec [Attribute] String
    right =
        listCons
            >>> assertRight []
            >>> attributeText
            >>> assertLeft "key2"

thingCodec :: XmlCodec XmlNode Thing
thingCodec = invmap2 id id to from getKeyAttribute
  where
    to :: (  String, String  ) -> Thing
    to (  a, b )  = Thing a b

    from :: Thing -> (  String, String  )
    from (Thing a b) = ( a, b )

runDecoder :: XmlCodec a b -> a -> Maybe b
runDecoder (Codec (Star decode) _) a = decode a
