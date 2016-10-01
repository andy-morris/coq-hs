{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
-- | A simplified XML parser which is only expected to be able to handle
-- what Coq might throw at it.
module Coq.XmlParser
  (module Coq.XmlAst,
   node, openTag, closeTag, ident,
   attr, pcdata, stringLit, entity)
where

import Coq.XmlAst
import Prelude hiding (takeWhile)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (chr, isAlpha, isAlphaNum)
import Data.Attoparsec.Text
import Data.Functor
import Control.Applicative
import Numeric

-- | Parse a complete node.
node :: Parser Node
node = do
    (name, attrs, selfClose) <- openTag
    if selfClose then do
      pure (Node name attrs [])
    else do
      children <- many (N <$> node  <|>  T <$> pcdata)
      closeTag name
      pure (Node name attrs (filter nonEmpty children))
  where
    nonEmpty (T "") = False
    nonEmpty _      = True

-- | Parse an opening tag. The third element is 'True' if the tag is
-- self-closing.
openTag :: Parser (Text, [Attr], Bool)
openTag = do
    char '<' *> skipSpace
    name <- ident
    attrs <- many (skipSpace1 *> attr) <* skipSpace
    selfClose <- maybe False (const True) <$> optional (char '/')
    skipSpace <* char '>'
    pure (name, attrs, selfClose)

-- | Parse a closing tag of the given name.
closeTag :: Text -> Parser ()
closeTag name = do
    char '<' *> skipSpace
    char '/' *> skipSpace
    string name *> skipSpace <* char '>'

-- | Parse a simplified form of an XML identifier which only allows
-- letters, digits, and @_@.
ident :: Parser Text
ident = liftA2 Text.cons (satisfy isAlpha') (takeWhile isAlphaNum')
  where
    isAlpha'    c = c == '_' || isAlpha    c
    isAlphaNum' c = c == '_' || isAlphaNum c

-- | Parse an attribute.
attr :: Parser Attr
attr = do
    name <- ident
    void (char '=')
    value <- stringLit
    pure (name := value)

-- | Parse some text which may contain entities.
pcdata :: Parser Text
pcdata =
    Text.strip . Text.concat <$> many1 (entity <|> takeWhile1 nonSpecial)
  where nonSpecial c = c /= '&' && c /= '<'

-- | Parse a string literal which can be delimited by either @"@ or @'@ and
-- may contain entities.
stringLit :: Parser Text
stringLit = do
    c <- satisfy (\c -> c == '\'' || c == '"')
    content <- Text.concat <$>
      many (entity <|> takeWhile1 (nonSpecial c))
    void (char c)
    pure content
  where
    nonSpecial q c = c /= '&' && c /= '<' && c /= q

-- | Parse and interpret an entity. The forms understood are:
--
-- * @&#1234;@ (decimal codepoint),
-- * @&#x12AB;@ (hex codepoint),
-- * @&nbsp;@, @&quot;@, @&amp;@, @&apos;@, @&lt;@, and @&gt;@.
entity :: Parser Text
entity = Text.singleton <$> entity'

entity' :: Parser Char
entity' = (char '&' *> takeWhile (/= ';') <* char ';') >>= fromEntity
  where
    fromEntity "nbsp" = pure '\xA0'
    fromEntity "quot" = pure '"'
    fromEntity "amp"  = pure '&'
    fromEntity "apos" = pure '\''
    fromEntity "lt"   = pure '<'
    fromEntity "gt"   = pure '>'
    fromEntity txt
      | ("#x", hex) <- Text.splitAt 2 txt,
        [(n, "")] <- readHex (Text.unpack hex)
      = pure (chr n)
      | Just ('#', dec) <- Text.uncons txt,
        [(n, "")] <- readDec (Text.unpack dec)
      = pure (chr n)
      | otherwise = fail ("unrecognised entity: " ++ Text.unpack txt)

skipSpace1 :: Parser ()
skipSpace1 = space *> skipSpace
