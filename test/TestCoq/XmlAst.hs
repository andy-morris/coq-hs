{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}
module TestCoq.XmlAst (tests) where

import Coq.XmlAst
import Coq.XmlParser
import Data.Attoparsec.Text (parseOnly)
import Data.Char (isSpace)
import Data.Text (pack)
import Data.Semigroup
import Test.Tasty.TH
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Text.Arbitrary (Text)
import qualified Data.Text as Text


tests = $(testGroupGenerator)

prop_parseToXml n = parseOnly node (toXml n) == Right n

case_empty =
    toXml (Node "a" [] []) @?= "<a/>"
case_attr1 =
    toXml (Node "a" ["b" := "c"] []) @?= "<a b='c'/>"
case_attr2 =
    toXml (Node "a" ["b" := "c", "d" := "e"] []) @?=
    "<a b='c' d='e'/>"
case_attr_empty =
    toXml (Node "a" ["b" := ""] []) @?= "<a b=''/>"
case_attr_entity =
    toXml (Node "a" ["b" := "'<>"] []) @?=
    "<a b='&apos;&lt;&gt;'/>"
case_child_node1 =
    toXml (Node "a" [] [N (Node "b" [] [])]) @?= "<a><b/></a>"
case_child_node2 =
    toXml (Node "a" [] [N (Node "b" [] []), N (Node "c" [] [])]) @?=
    "<a><b/><c/></a>"
case_child_text1 =
    toXml (Node "a" [] [textChild "b"]) @?= "<a>b</a>"
case_child_text1_space =
    toXml (Node "a" [] [textChild "b   "]) @?= "<a>b   </a>"
case_child_text_entity =
    toXml (Node "a" [] [textChild "<>"]) @?= "<a>&lt;&gt;</a>"
case_child_text_node =
    toXml (Node "a" [] [textChild "b", N (Node "c" [] [])]) @?= "<a>b<c/></a>"

instance Arbitrary Node where
    arbitrary =
        Node <$> arbName
             <*> arbList
             <*> (mergeChildren <$> arbList)
    shrink (Node name attrs children) =
      [Node name attrs' children  | attrs'    <- shrink attrs] ++
      [Node name attrs  children' | children' <- shrink children]

instance Arbitrary Attr where
    arbitrary = (:=) <$> arbName <*> arbitrary
    shrink (n := v) = [n := v' | v' <- shrink v]

instance Arbitrary Child where
    arbitrary = oneof [N <$> arbitrary, textChild <$> arbText]
    shrink (N n)   = map N (shrink n)
    shrink (T t _) = map textChild (shrinkText t)

arbText = Text.strip . pack <$> (arbitrary `suchThat` any (not . isSpace))
shrinkText t = filter (Text.any (not . isSpace)) (shrink t)

mergeChildren (T t1 _ : T t2 _ : cs) = mergeChildren (textChild (t1 <> t2) : cs)
mergeChildren (c : cs)               = c : mergeChildren cs
mergeChildren []                     = []

arbList :: Arbitrary a => Gen [a]
arbList = do
    n <- arbitrary
    vectorOf n (scale (`div` n) arbitrary)

arbName :: Gen Text
arbName = do
    start <- startChar
    rest  <- listOf restChar
    pure (pack (start : rest))
  where
    startChar = oneof [choose ('a', 'z'), choose ('A', 'Z'), pure '_']
    restChar  = oneof [startChar, choose ('0', '9')]
