{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module TestCoq.XmlParser (tests) where

import Coq.XmlParser
import Data.Attoparsec.Text
import Data.String.QQ
import Test.Tasty.HUnit
import Test.Tasty.TH

p ~~> txt = either (const Nothing) Just (parseOnly (p <* endOfInput) txt)

case_entity_nbsp = entity ~~> "&nbsp;" @?= Just "\xA0"
case_entity_hex  = entity ~~> "&#xA0;" @?= Just "\xA0"
case_entity_dec  = entity ~~> "&#160;" @?= Just "\160"
case_entity_inv  = entity ~~> "&haha;" @?= Nothing

case_stringLit_apos_empty    = stringLit ~~> "''"       @?= Just ""
case_stringLit_apos_nonempty = stringLit ~~> "'abc'"    @?= Just "abc"
case_stringLit_apos_entity   = stringLit ~~> "'&apos;'" @?= Just "'"
case_stringLit_apos_fail     = stringLit ~~> "'''"      @?= Nothing

case_stringLit_quot_empty    = stringLit ~~> [s|""|]       @?= Just ""
case_stringLit_quot_nonempty = stringLit ~~> [s|"abc"|]    @?= Just "abc"
case_stringLit_quot_entity   = stringLit ~~> [s|"&quot;"|] @?= Just "\""
case_stringLit_quot_fail     = stringLit ~~> [s|"""|]      @?= Nothing

case_ident_empty = ident ~~> ""     @?= Nothing
case_ident_space = ident ~~> "a b"  @?= Nothing
case_ident_one   = ident ~~> "a"    @?= Just "a"
case_ident_many  = ident ~~> "abc"  @?= Just "abc"
case_ident_digit = ident ~~> "abc1" @?= Just "abc1"
case_ident_und   = ident ~~> "a_b"  @?= Just "a_b"
case_ident_dot   = ident ~~> "a.b"  @?= Just "a.b"

case_pcdata_nonempty     = pcdata ~~> "abc"      @?= Just ("abc", "abc")
case_pcdata_entity       = pcdata ~~> "a&lt;b"   @?= Just ("a<b", "a<b")
case_pcdata_space_before = pcdata ~~> " \n\tabc" @?= Just (" \n\tabc", "abc")
case_pcdata_space_after  = pcdata ~~> "abc \n\t" @?= Just ("abc \n\t", "abc")
case_pcdata_lt           = pcdata ~~> "a<b"      @?= Nothing

case_openTag_unterminated =
    openTag ~~> "<a" @?= Nothing
case_openTag_simple =
    openTag ~~> "<a>" @?= Just ("a", [], False)
case_openTag_nameless =
    openTag ~~> "<>" @?= Nothing
case_openTag_simple_space =
    openTag ~~> "< a >" @?= Just ("a", [], False)
case_openTag_simple_selfClose =
    openTag ~~> "<a/>" @?= Just ("a", [], True)
case_openTag_simple_selfClose_space =
    openTag ~~> "< a / >" @?= Just ("a", [], True)
case_openTag_attr1 =
    openTag ~~> "<a b='c'>" @?= Just ("a", ["b" := "c"], False)
case_openTag_attr2 =
    openTag ~~> "<a b='c' d='e'>" @?=
    Just ("a", ["b" := "c", "d" := "e"], False)
case_openTag_attr2' =
    openTag ~~> "<a b='c'd='e'>" @?= Nothing
case_openTag_attr_selfClose =
    openTag ~~> "<a b='c'/>" @?= Just ("a", ["b" := "c"], True)

case_closeTag =
    closeTag "a" ~~> "</a>" @?= Just ()
case_closeTag_space =
    closeTag "a" ~~> "< / a >" @?= Just ()
case_closeTag_wrong_name =
    closeTag "a" ~~> "</b>" @?= Nothing
case_closeTag_not_close =
    closeTag "a" ~~> "<a>" @?= Nothing
case_closeTag_empty =
    closeTag "a" ~~> "</>" @?= Nothing

case_node_selfClose =
    node ~~> "<a/>" @?= Just (Node "a" [] [])
case_node_empty_long =
    node ~~> "<a></a>" @?= Just (Node "a" [] [])
case_node_empty_long_space =
    node ~~> "<a> </a>" @?= Just (Node "a" [] [])
case_node_child_text =
    node ~~> "<a>b</a>" @?= Just (Node "a" [] [textChild "b"])
case_node_child_node =
    node ~~> "<a><b/></a>" @?= Just (Node "a" [] [N (Node "b" [] [])])
case_node_child_text_node =
    node ~~> "<a>b\n<c/></a>" @?=
    Just (Node "a" [] [textChild "b\n", N (Node "c" [] [])])
case_node_child_node_text =
    node ~~> "<a><c/>\nb</a>" @?=
    Just (Node "a" [] [N (Node "c" [] []), textChild "\nb"])

case_full_response =
    (skipSpace *> node <* skipSpace) ~~> [s|
      <option_state>
        <bool val="true"/>
        <bool val="false"/>
        <string>name1</string>
        <option_value val="intvalue">
          <option val="some">
            <int>37</int>
          </option>
        </option_value>
      </option_state>
    |] @?=
    Just (Node "option_state" [] [
      N (Node "bool" ["val" := "true"] []),
      N (Node "bool" ["val" := "false"] []),
      N (Node "string" [] [textChild "name1"]),
      N (Node "option_value" ["val" := "intvalue"] [
        N (Node "option" ["val" := "some"] [
          N (Node "int" [] [textChild "37"])
        ])
      ])
    ])

tests = $(testGroupGenerator)
