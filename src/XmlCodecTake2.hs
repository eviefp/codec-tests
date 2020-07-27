module XmlCodecTake2 where

import Control.Applicative (Alternative, empty)
import Data.Functor.Invariant (Invariant2(invmap2))
import Codec
import Prelude
import Data.Functor.Identity (Identity)
import Control.Category ((>>>))
import qualified Control.Category as C

type XmlCodec a b = Codec Maybe Identity a b

data XmlNode
    = Node String [Attribute] [XmlNode]
    | Text String

data Attribute = Attribute String String

example :: XmlNode
example =
    Node "parent"
        [ Attribute "key" "value" ]
        []

element :: XmlCodec XmlNode (String, [Attribute])
element = mkCodec decode encode
  where
    decode :: XmlNode -> Maybe (String, ([Attribute]))
    decode (Node tag attrs _) = Just (tag, (attrs))
    decode (Text _) = Nothing

    encode :: (String, ([Attribute])) -> Identity XmlNode
    encode (tag, attrs) = pure $ Node tag attrs []

assert
    :: forall a m n x
    .  Eq a
    => Alternative m
    => Applicative n
    => a
    -> Codec m n (a, x) x
assert a = mkCodec decode encode
  where
    decode :: (a, x) -> m x
    decode (a', x) = if a == a' then pure x else empty

    encode :: x -> n (a, x)
    encode x = pure (a, x)

attribute :: XmlCodec [Attribute] Attribute
attribute = mkCodec decode encode
  where
    decode :: [Attribute] -> Maybe Attribute
    decode [x] = Just x
    decode _ = Nothing

    encode :: Attribute -> Identity [Attribute]
    encode = pure . pure

attributeText :: XmlCodec Attribute (String, String)
attributeText = mkCodec decode encode
  where
    decode :: Attribute -> Maybe (String, String)
    decode (Attribute key value) = Just (key, value)

    encode :: (String, String) -> Identity Attribute
    encode (key, value) = pure $ Attribute key value

data Thing = Thing String String String

getKeyAttribute :: XmlCodec XmlNode (String, (String, String))
getKeyAttribute =
   element
      >>> (C.id ^^^ (attribute >>> attributeText))

thingCodec :: XmlCodec XmlNode Thing
thingCodec = invmap2 id id to from getKeyAttribute
  where
    to :: (String, (String, String)) -> Thing
    to (a, (b, c)) = Thing a b c

    from :: Thing -> (String, (String, String))
    from (Thing a b c) = (a, (b, c))
