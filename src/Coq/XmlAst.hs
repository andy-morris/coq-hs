-- | Simplified XML abstract syntax.
module Coq.XmlAst (Node (..), Attr (..), Child (..)) where

import Data.Text (Text)

-- | An XML element node.
data Node =
    Node {
      nodeName     :: Text,
      nodeAttrs    :: [Attr],
      nodeChildren :: [Child]
    }
  deriving (Eq, Show)

data Attr = (:=) { attrName, attrValue :: Text } deriving Eq

instance Show Attr where
    showsPrec d (n := v) = showParen (d > 9) $
        showsPrec 10 n . showString " := " . showsPrec 10 v

-- | A child of a node, either another node or a text element.
data Child = N Node | T Text deriving (Eq, Show)
