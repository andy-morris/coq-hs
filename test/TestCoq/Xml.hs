{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module TestCoq.Xml (tests, module Coq.Xml) where

import Coq.Protocol
import Coq.Xml
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import Data.Function (on)
import Data.Text.Arbitrary (Text)
import Data.String (IsString (..))
import Data.String.QQ
import Text.XML.Light


encodeDecode :: (XmlEncode a, XmlDecode a, Eq a) => a -> Bool
encodeDecode x = decode (encode x) == Just x

prop_encodeDecode_Unit   = encodeDecode @()
prop_encodeDecode_Bool   = encodeDecode @Bool
prop_encodeDecode_Int    = encodeDecode @Int
prop_encodeDecode_Text   = encodeDecode @Text
prop_encodeDecode_Maybe  = encodeDecode @(Maybe Int)
prop_encodeDecode_Pair   = encodeDecode @(Int, Text)
prop_encodeDecode_List   = encodeDecode @[Int]
prop_encodeDecode_Either = encodeDecode @(Either Int Text)

showEncode :: XmlEncode a => a -> String
showEncode = showElement . encode

case_encode_Unit =
    showEncode () @?= [s|<unit />|]
case_encode_True =
    showEncode True @?= [s|<bool val="true" />|]
case_encode_False =
    showEncode False @?= [s|<bool val="false" />|]
case_encode_42 =
    showEncode (42 :: Int) @?= [s|<int>42</int>|]
case_encode_str =
    showEncode ("hello" :: Text) @?= [s|<string>hello</string>|]
case_encode_entities =
    showEncode ("<hello>" :: Text) @?= [s|<string>&lt;hello&gt;</string>|]
case_encode_Nothing =
    showEncode (Nothing @()) @?= [s|<option val="none" />|]
case_encode_Just =
    showEncode (Just False) @?=
    [s|<option val="some"><bool val="false" /></option>|]
case_encode_Pair =
    showEncode (42 :: Int, "hello" :: Text) @?=
    [s|<pair><int>42</int><string>hello</string></pair>|]
case_encode_ListNil =
    showEncode ([] :: [Int]) @?= [s|<list />|]
case_encode_List3 =
    showEncode [True, True, False] @?=
    [s|<list><bool val="true" /><bool val="true" />|] ++
    [s|<bool val="false" /></list>|]
case_encode_Left =
    showEncode (Left 42 :: Either Int Int) @?=
    [s|<union val="in_l"><int>42</int></union>|]
case_encode_Right =
    showEncode (Right 42 :: Either Int Int) @?=
    [s|<union val="in_r"><int>42</int></union>|]


newtype PPXML = PPXML {unPPXML :: Element}
instance Eq PPXML where
    (==) = (==) `on` (showElement . removeSpace . unPPXML)
instance Show PPXML where
    show = ppElement . removeSpace . unPPXML
instance IsString PPXML where
    fromString = PPXML . fromJust . parseXMLDoc

encodeCall :: XmlMessage req resp => req -> PPXML
encodeCall = PPXML . makeCall

decodeStr :: XmlDecode resp => PPXML -> Maybe resp
decodeStr = decode . unPPXML

case_decode_entities1 =
    decodeStr [s|<string>&lt;hello&gt;</string>|] @?=
    Just ("<hello>" :: Text)
case_decode_entities2 =
    decodeStr [s|<string>&#9786;</string>|] @?=
    Just ("\9786" :: Text)

case_encode_Init =
    encodeCall (Init Nothing) @?=
    [s|<call val="Init"><option val="none" /></call>|]
case_decode_InitResp =
    decodeStr [s|<state_id val="42" />|] @?=
    Just (InitResp (StateId 42))

case_encode_About =
    encodeCall About @?= [s|<call val="About"><unit /></call>|]
case_decode_AboutResp =
    decodeStr
      [s|<coq_info>
           <string>8.5</string>
           <string>20140312</string>
           <string>June&nbsp;2016</string>
           <string>Jun&nbsp;9&nbsp;2016&nbsp;12:4:46</string>
         </coq_info>|]
    @?=
    Just (AboutResp {
      arCoqtopVersion = "8.5",
      arProtocolVersion = "20140312",
      arReleaseDate = "June\xA0\&2016",
      arCompileDate = "Jun\xA0\&9\xA0\&2016\xA0\&12:4:46"
    })

case_encode_Status =
    encodeCall (Status True) @?=
    [s|<call val="Status"><bool val="true" /></call>|]
case_decode_Status =
    decodeStr [s|
      <status>
        <list><string>Mod</string></list>
        <option val="some"><string>pf</string></option>
        <list />
        <int>2</int>
      </status>
    |] @?=
    Just (StatusResp {
      srProofPath = ["Mod"],
      srProofName = Just "pf",
      srAllProofs = [],
      srProofNum  = 2
    })

case_encode_Add =
    encodeCall (Add {
      aPhrase  = " intros.",
      aEditId  = 11, -- FIXME if this value diesn't actually make sense
      aStateId = StateId 0,
      aVerbose = False
    }) @?= [s|
      <call val="Add">
        <pair>
          <pair>
            <string> intros.</string>
            <int>11</int>
          </pair>
          <pair>
            <state_id val="0" />
            <bool val="false" />
          </pair>
        </pair>
      </call>
    |]
case_decode_Add =
    decodeStr [s|
      <pair>
        <state_id val="1" />
        <pair>
          <union val="in_l"> <unit /> </union>
          <string>a message</string>
        </pair>
      </pair>
    |] @?=
    Just (AddResp {
      arStateId   = StateId 1,
      arEditPoint = Nothing,
      arMessage   = "a message"
    })

case_encode_EditAt =
    encodeCall (EditAt (StateId 1)) @?=
    [s|<call val="EditAt"> <state_id val="1" /> </call>|]
case_decode_EditAt1 =
    decodeStr [s|<union val="in_l"> <unit /> </union>|] @?=
    Just EditAtNewTip
case_decode_EditAt2 =
    decodeStr [s|
      <union val="in_r">
        <pair>
          <state_id val="1" />
          <pair>
            <state_id val="2" />
            <state_id val="3" />
          </pair>
        </pair>
      </union>
    |] @?=
    Just (EditAtFocus {
      erStart = StateId 1,
      erStop  = StateId 2,
      erTip   = StateId 3
    })

case_encode_Query =
    encodeCall (Query "Print nat." (StateId 3))
    @?= [s|
      <call val="Query">
        <pair>
          <string>Print nat.</string>
          <state_id val="3" />
        </pair>
      </call>
    |]
case_decode_Query =
    decodeStr [s|<string>nat : Set</string>|] @?=
    Just (QueryResp "nat : Set")

case_encode_Goal =
    encodeCall Goal @?=
    [s|<call val="Goal"> <unit /> </call>|]
case_decode_Goal1 =
    decodeStr [s|<option val="none" />|] @?=
    Just GoalNotInProof
case_decode_Goal2 =
    decodeStr [s|
      <option val="some">
        <goals>
          <list />
          <pair> <list /> <list /> </pair>
          <list />
          <list>
            <goal>
              <string>goal1</string>
              <list> <string>H : True</string> </list>
              <string>False</string>
            </goal>
          </list>
        </goals>
      </option>
    |] @?=
    Just (GoalResp {
      grForeground = [],
      grBackground = ([], []),
      grShelved    = [],
      grGivenUp    = [GoalInfo {
        gGoalId     = "goal1",
        gHypotheses = ["H : True"],
        gConclusion = "False"
      }]
    })

case_encode_Evars =
    encodeCall Evars @?= [s|<call val="Evars"> <unit /> </call>|]
case_decode_Evars1 =
    decodeStr [s|<option val="none" />|] @?= Just EvarsNotInProof
case_decode_Evars2 =
    decodeStr [s|
      <option val="some">
        <list> <string>?a</string> <string>?b</string> </list>
      </option>
    |] @?=
    Just (EvarsResp [Evar "?a", Evar "?b"])

case_encode_Hints =
    encodeCall Hints @?= [s|<call val="Hints"> <unit /> </call>|]
case_decode_Hints1 =
    decodeStr [s|<option val="none" />|] @?= Just HintsNotInProof
case_decode_Hints2 =
    decodeStr [s|
      <option val="some">
        <pair>
          <list> <list /> </list>
          <list>
            <pair>
              <string>auto</string>
              <string>auto</string>
            </pair>
          </list>
        </pair>
      </option>
    |] @?=
    Just (HintsResp {
      hHypHints = [[]],
      hConcHint = [Hint {
        hTactic = "auto",
        hAppearance = "auto"
      }]
    })

case_encode_Search =
    encodeCall (Search [SearchFlag (TypePattern "Set") True,
                        SearchFlag (InModule ["M"]) False])
    @?= [s|
      <call val="Search">
        <list>
          <pair>
            <search_cst val="type_pattern">
              <string>Set</string>
            </search_cst>
            <bool val="true" />
          </pair>
          <pair>
            <search_cst val="in_module">
              <list> <string>M</string> </list>
            </search_cst>
            <bool val="false" />
          </pair>
        </list>
      </call>
    |]
case_decode_Search =
    decodeStr [s|
      <list>
        <coq_object>
          <list> <string>M1</string> </list>
          <list> <string>M2</string> <string>x</string> </list>
          <string>x : nat</string>
        </coq_object>
      </list>
    |] @?=
    Just (SearchResp [CoqObject ["M1"] ["M2", "x"] "x : nat"])

case_encode_GetOptions =
    encodeCall GetOptions @?= [s|<call val="GetOptions"> <unit /> </call>|]
case_decode_OptionState =
    decodeStr [s|
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
    Just (OptionState True False "name1" (IntValue (Just 37)))
case_decode_GetOptions =
    decodeStr [s|
      <list>
        <pair>
          <list>
            <string>Printing</string>
            <string>Universes</string>
          </list>
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
        </pair>
      </list>
    |] @?=
    Just (GetOptionsResp
      [Option {
        oName = ["Printing", "Universes"],
        oState = OptionState True False "name1" (IntValue (Just 37))
       }])

case_encode_SetOptions =
    encodeCall
      (SetOptions [SetOption ["Printing", "Universes"] (BoolValue True)])
    @?= [s|
      <call val="SetOptions">
        <list>
          <pair>
            <list>
              <string>Printing</string>
              <string>Universes</string>
            </list>
            <option_value val="boolvalue">
              <bool val="true" />
            </option_value>
          </pair>
        </list>
      </call>
    |]
case_decode_SetOptions =
    decodeStr [s|<unit />|] @?= Just SetOptionsResp

case_encode_MakeCases =
    encodeCall (MakeCases "nat") @?=
    [s|<call val="MakeCases"> <string>nat</string> </call>|]
case_decode_MakeCases =
    decodeStr [s|
      <list>
        <list> <string>O</string> </list>
        <list> <string>S</string> <string>n</string> </list>
      </list>
    |] @?=
    Just (MakeCasesResp [["O"], ["S", "n"]])

case_encode_Quit =
    encodeCall Quit @?= [s|<call val="Quit"> <unit /> </call>|]
case_decode_Quit =
    decodeStr [s|<unit />|] @?= Just QuitResp


case_fromResponse_invalid =
    fromResponse (fromJust (parseXMLDoc ("<whatever />" :: Text))) @?=
    (DecodeError :: Response QuitResp)

case_fromResponse_failure1 =
    fromResponse (fromJust (parseXMLDoc ([s|
      <value val="fail">
        <state_id val="1" />
        no, sorry
      </value>
    |] :: Text))) @?=
    (Failure Nothing (StateId 1) "no, sorry" :: Response QuitResp)

case_fromResponse_failure2 =
    fromResponse (fromJust (parseXMLDoc ([s|
      <value val="fail" loc_s="0" loc_e="1">
        <state_id val="1" />
        no, sorry
      </value>
    |] :: Text))) @?=
    (Failure (Just (0, 1)) (StateId 1) "no, sorry" :: Response QuitResp)


removeSpace' :: Content -> Content
removeSpace' (Elem e) = Elem (removeSpace e)
removeSpace' c = c

removeSpace :: Element -> Element
removeSpace (Element e as cs l) =
    let cs' = map removeSpace' (filter (not . isSpaceText) cs) in
    Element e as cs' l

isSpaceText :: Content -> Bool
isSpaceText (Text str) = all isSpace (cdData str)
isSpaceText _          = False

tests = $(testGroupGenerator)
