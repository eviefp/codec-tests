module XmlCodec where

import Codec
import Control.Category
    ((>>>))
import Control.Monad
    (join)
import Data.Foldable
    (asum)
import Data.Functor.Identity
    (Identity (..))
import Data.Maybe
    (catMaybes, maybeToList)
import Data.Profunctor
    (Star (Star))
import Prelude

type XmlCodec a = Codec Maybe Identity XmlNode a

data XmlNode
   = Node String [XmlNode]
   | Attribute String String
   | Text String
   | Group [XmlNode]
   deriving stock (Eq, Show)

instance Semigroup XmlNode where
    xs <> ys = Group [xs, ys]

parent :: XmlNode
parent =
    Node "parent"
        [ Node "child1" [ Text "text" ]
        , Node "child2" [ Text "other" ]
        ]

parentWithAttribute :: XmlNode
parentWithAttribute =
    Node "parent"
        [ Node "child1" [ Attribute "hi" "there" ]
        , Node "child2" [ Attribute "whoops" ":(", Text "other" ]
        ]

runDecoder :: XmlNode -> XmlCodec a -> Maybe a
runDecoder node (Codec (Star xml2ma) _) = xml2ma node

normalize :: XmlNode -> Maybe XmlNode
normalize =
    \case
        Node t xs -> Just $ Node t (
            join
                . maybeToList
                . traverse normalize
                $ xs >>= go
                )
        Group _ -> Nothing
        attr@(Attribute _ _) -> Just attr
        t@(Text _) -> Just t
  where
    go :: XmlNode -> [XmlNode]
    go =
        \case
            Group xs -> join $ go <$> xs
            other -> [ other ]

roundTrip :: XmlNode -> XmlCodec a -> Maybe XmlNode
roundTrip node codec@(Codec _ (Star a2nxml)) =
    (runIdentity . a2nxml <$> runDecoder node codec) >>= normalize

element :: String -> XmlCodec [XmlNode]
element tag = mkCodec decode encode
  where
    decode :: XmlNode -> Maybe [XmlNode]
    decode (Node t c) = if t == tag then Just c else Nothing
    decode _          = Nothing

    encode :: [XmlNode] -> Identity XmlNode
    encode = pure . Node tag

attribute :: String -> Codec Maybe Identity [XmlNode] String
attribute name = mkCodec decode encode
  where
    decode :: [XmlNode] -> Maybe String
    decode = lookup name . catMaybes . fmap nodeToLookup

    nodeToLookup :: XmlNode -> Maybe (String, String)
    nodeToLookup (Attribute t v) = Just (t, v)
    nodeToLookup _               = Nothing

    encode :: String -> Identity [XmlNode]
    encode = pure . pure . Attribute name

single :: Codec Maybe Identity XmlNode XmlNode
single = mkCodec decode encode
  where
    decode :: XmlNode -> Maybe XmlNode
    decode (Node _ [x]) = Just x
    decode _            = Nothing

    encode :: XmlNode -> Identity XmlNode
    encode = pure

children :: Codec Maybe Identity XmlNode [XmlNode]
children  = mkCodec decode encode
  where
    decode :: XmlNode -> Maybe [XmlNode]
    decode (Node _ xs) = Just xs
    decode (Group  xs) = Just xs
    decode _           = Nothing

    encode :: [XmlNode] -> Identity XmlNode
    encode = pure . Group

child :: String -> Codec Maybe Identity [XmlNode] XmlNode
child tag = mkCodec decode encode
  where
    decode :: [XmlNode] -> Maybe XmlNode
    decode = lookup tag . catMaybes . fmap nodeToLookup

    nodeToLookup :: XmlNode -> Maybe (String, XmlNode)
    nodeToLookup n@(Node t _) = Just (t, n)
    nodeToLookup _            = Nothing

    encode :: XmlNode -> Identity [XmlNode]
    encode = pure . pure . Node tag . pure

text :: Codec Maybe Identity [XmlNode] String
text = mkCodec decode encode
  where
    decode :: [XmlNode] -> Maybe String
    decode = asum . fmap nodeToLookup

    nodeToLookup :: XmlNode -> Maybe String
    nodeToLookup (Text t) = Just t
    nodeToLookup _        = Nothing

    encode :: String -> Identity [XmlNode]
    encode = pure . pure . Group . pure . Text

parentCodec :: XmlCodec (String, String)
parentCodec =
    element "parent"
        >>> childCodec "child1" *** childCodec "child2"
  where
    childCodec :: String -> Codec Maybe Identity [XmlNode] String
    childCodec tag = child tag >>> children >>> text

attributeParentCodec :: XmlCodec (String, (String, String))
attributeParentCodec =
    element "parent"
        >>> (child "child1" >>> children >>> attribute "hi")
        *** (child "child2" >>> children >>> text *** attribute "whoops")

