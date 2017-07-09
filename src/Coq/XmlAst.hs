{-# LANGUAGE OverloadedStrings #-}
-- | Simplified XML abstract syntax.
module Coq.XmlAst
  (Node (..), toXml,
   Attr (..), lookupAttr,
   Child (..), textChild, toNode, toText, strip)
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Data.Char (isAscii, isPrint, ord)
import Data.List (find)
import Data.Semigroup
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

-- | Look for the value of an attribute by name.
lookupAttr :: Text -> [Attr] -> Maybe Text
lookupAttr name = fmap attrValue . find ((name ==) . attrName)

instance Show Attr where
    showsPrec d (n := v) = showParen (d > 9) $
        showsPrec 10 n . showString " := " . showsPrec 10 v


-- | A child of a node, either another node or a text element. A text element
-- contains (1) the whole text, and (2) the text with leading and trailing
-- whitespace stripped.
data Child = N Node | T Text Text deriving (Eq, Show)

textChild :: Text -> Child
textChild txt = T txt (Text.strip txt)

toText :: Child -> Maybe Text
toText (T txt _) = pure txt
toText _         = empty

toNode :: Child -> Maybe Node
toNode (N e) = pure e
toNode _     = empty


toXml :: Node -> Text
toXml = runB . nodeB

runB :: Builder -> Text
runB = Lazy.toStrict . Builder.toLazyText

type MkB a = a -> Builder

nodeB :: MkB Node
nodeB (Node name attrs children) =
    "<" <> textB name <> attrsB attrs <> childrenCloseB name children

textB :: MkB Text
textB = Builder.fromText

attrsB :: MkB [Attr]
attrsB = foldMap ((" " <>) . attrB)

attrB :: MkB Attr
attrB (n := v) = textB n <> "='" <> pcdataB v <> "'"

pcdataB :: MkB Text
pcdataB txt | Text.null txt = mempty
pcdataB txt = textB this <> entityPcdataB rest
  where (this, rest) = Text.break special txt

special :: Char -> Bool
special c = c `elem` ("\"'<>&" :: String) || not (isAscii c && isPrint c)

entityPcdataB :: MkB Text
entityPcdataB txt'
  | Just (c, txt) <- Text.uncons txt' = entityB c <> pcdataB txt
entityPcdataB _    = mempty

entityB :: MkB Char
entityB '\xA0' = "&nbsp;"
entityB '"'    = "&quot;"
entityB '\''   = "&apos;"
entityB '<'    = "&lt;"
entityB '>'    = "&gt;"
entityB '&'    = "&amp;"
entityB c      = "&#" <> Builder.fromString (show (ord c)) <> ";"

childrenCloseB :: Text -> MkB [Child]
childrenCloseB _    []       = "/>"
childrenCloseB name children =
    ">" <> foldMap childB children <> "</" <> textB name <> ">"

childB :: MkB Child
childB (N node)  = nodeB node
childB (T txt _) = pcdataB txt


strip :: Node -> Text
strip = runB . go . N where
  go (N n)     = foldMap go $ nodeChildren n
  go (T txt _) = textB txt
