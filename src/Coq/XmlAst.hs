-- | Simplified XML abstract syntax.
module Coq.XmlAst
  (Node (..),
   Attr (..), lookupAttr,
   Child (..), toNode, toText)
where

import Data.Text (Text)
import Data.List (find)
import Control.Applicative

-- | An XML element node.
data Node =
    Node {
      nodeName     :: Text,
      nodeAttrs    :: [Attr],
      nodeChildren :: [Child]
    }
  deriving (Eq, Show)


data Attr = (:=) { attrName, attrValue :: Text } deriving Eq

-- | Look for th evalue of an attribute by name.
lookupAttr :: Text -> [Attr] -> Maybe Text
lookupAttr name = fmap attrValue . find ((name ==) . attrName)

instance Show Attr where
    showsPrec d (n := v) = showParen (d > 9) $
        showsPrec 10 n . showString " := " . showsPrec 10 v


-- | A child of a node, either another node or a text element.
data Child = N Node | T Text deriving (Eq, Show)

toText :: Child -> Maybe Text
toText (T txt) = pure txt
toText _       = empty

toNode :: Child -> Maybe Node
toNode (N e) = pure e
toNode _     = empty
