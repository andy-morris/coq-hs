{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module TestCoq.Xml (tests, module Coq.Xml) where

import Coq.Protocol
import Coq.Xml
import TestCoq.XmlQQ
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH
import Data.Text.Arbitrary (Text)


encodeDecode :: (Encode a, Decode a, Eq a) => a -> Bool
encodeDecode x = decode (encode x) == Just x

prop_encodeDecode_Unit   = encodeDecode @()
prop_encodeDecode_Bool   = encodeDecode @Bool
prop_encodeDecode_Int    = encodeDecode @Int
prop_encodeDecode_Text   = encodeDecode @Text
prop_encodeDecode_Maybe  = encodeDecode @(Maybe Int)
prop_encodeDecode_Pair   = encodeDecode @(Int, Text)
prop_encodeDecode_List   = encodeDecode @[Int]
prop_encodeDecode_Either = encodeDecode @(Either Int Text)

case_encode_Unit =
    encode () @?= [xml|<unit />|]
case_encode_True =
    encode True @?= [xml|<bool val="true" />|]
case_encode_False =
    encode False @?= [xml|<bool val="false" />|]
case_encode_42 =
    encode (42 :: Int) @?= [xml|<int>42</int>|]
case_encode_str =
    encode ("hello" :: Text) @?= [xml|<string>hello</string>|]
case_encode_entities =
    encode ("<hello>" :: Text) @?= [xml|<string>&lt;hello&gt;</string>|]
case_encode_Nothing =
    encode (Nothing @()) @?= [xml|<option val="none" />|]
case_encode_Just =
    encode (Just False) @?=
    [xml|<option val="some"><bool val="false" /></option>|]
case_encode_Pair =
    encode (42 :: Int, "hello" :: Text) @?=
    [xml|<pair><int>42</int><string>hello</string></pair>|]
case_encode_ListNil =
    encode ([] :: [Int]) @?= [xml|<list />|]
case_encode_List3 =
    encode [True, True, False] @?= [xml|
      <list>
        <bool val="true" />
        <bool val="true" />
        <bool val="false" />
      </list>
    |]
case_encode_Left =
    encode (Left 42 :: Either Int Int) @?=
    [xml|<union val="in_l"><int>42</int></union>|]
case_encode_Right =
    encode (Right 42 :: Either Int Int) @?=
    [xml|<union val="in_r"><int>42</int></union>|]


case_decode_entities1 =
    decode [xml|<string>&lt;hello&gt;</string>|] @?=
    Just ("<hello>" :: Text)
case_decode_entities2 =
    decode [xml|<string>&#9786;</string>|] @?=
    Just ("\9786" :: Text)



case_encode_Init =
    makeCall (Init Nothing) @?=
    [xml|<call val="Init"><option val="none" /></call>|]
case_decode_InitResp =
    decode [xml|<state_id val="42" />|] @?=
    Just (InitResp (StateId 42))

case_encode_About =
    makeCall About @?= [xml|<call val="About"><unit /></call>|]
case_decode_AboutResp =
    decode
      [xml|<coq_info>
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
    makeCall (Status True) @?=
    [xml|<call val="Status"><bool val="true" /></call>|]
case_decode_Status =
    decode [xml|
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
    makeCall (Add {
      aPhrase  = "intros.",
      aEditId  = 11, -- FIXME if this value diesn't actually make sense
      aStateId = StateId 0,
      aVerbose = False
    }) @?= [xml|
      <call val="Add">
        <pair>
          <pair>
            <string>intros.</string>
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
    decode [xml|
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
    makeCall (EditAt (StateId 1)) @?=
    [xml|<call val="EditAt"> <state_id val="1" /> </call>|]
case_decode_EditAt1 =
    decode [xml|<union val="in_l"> <unit /> </union>|] @?=
    Just EditAtNewTip
case_decode_EditAt2 =
    decode [xml|
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
    makeCall (Query "Print nat." (StateId 3))
    @?= [xml|
      <call val="Query">
        <pair>
          <string>Print nat.</string>
          <state_id val="3" />
        </pair>
      </call>
    |]
case_decode_Query =
    decode [xml|<string>nat : Set</string>|] @?=
    Just (QueryResp "nat : Set")

case_encode_Goal =
    makeCall Goal @?=
    [xml|<call val="Goal"> <unit /> </call>|]
case_decode_Goal1 =
    decode [xml|<option val="none" />|] @?=
    Just GoalNotInProof
case_decode_Goal2 =
    decode [xml|
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
    makeCall Evars @?= [xml|<call val="Evars"> <unit /> </call>|]
case_decode_Evars1 =
    decode [xml|<option val="none" />|] @?= Just EvarsNotInProof
case_decode_Evars2 =
    decode [xml|
      <option val="some">
        <list> <string>?a</string> <string>?b</string> </list>
      </option>
    |] @?=
    Just (EvarsResp [Evar "?a", Evar "?b"])

case_encode_Hints =
    makeCall Hints @?= [xml|<call val="Hints"> <unit /> </call>|]
case_decode_Hints1 =
    decode [xml|<option val="none" />|] @?= Just HintsNotInProof
case_decode_Hints2 =
    decode [xml|
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
    makeCall (Search [SearchFlag (TypePattern "Set") True,
                        SearchFlag (InModule ["M"]) False])
    @?= [xml|
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
    decode [xml|
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
    makeCall GetOptions @?= [xml|<call val="GetOptions"> <unit /> </call>|]
case_decode_OptionState =
    decode [xml|
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
    decode [xml|
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
    makeCall
      (SetOptions [SetOption ["Printing", "Universes"] (BoolValue True)])
    @?= [xml|
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
    decode [xml|<unit />|] @?= Just SetOptionsResp

case_encode_MakeCases =
    makeCall (MakeCases "nat") @?=
    [xml|<call val="MakeCases"> <string>nat</string> </call>|]
case_decode_MakeCases =
    decode [xml|
      <list>
        <list> <string>O</string> </list>
        <list> <string>S</string> <string>n</string> </list>
      </list>
    |] @?=
    Just (MakeCasesResp [["O"], ["S", "n"]])

case_encode_Quit =
    makeCall Quit @?= [xml|<call val="Quit"> <unit /> </call>|]
case_decode_Quit =
    decode [xml|<unit />|] @?= Just QuitResp


case_fromResponse_invalid =
    fromResponse [xml|<whatever/>|] @?=
    (DecodeError :: Response QuitResp)

case_fromResponse_failure1 =
    fromResponse [xml|
      <value val="fail">
        <state_id val="1" />
        no, sorry
      </value>
    |] @?=
    (Failure Nothing (StateId 1) "no, sorry" :: Response QuitResp)

case_fromResponse_failure2 =
    fromResponse [xml|
      <value val="fail" loc_s="0" loc_e="1">
        <state_id val="1" />
        no, sorry
      </value>
    |] @?=
    (Failure (Just (Loc 0 1)) (StateId 1) "no, sorry" :: Response QuitResp)

tests = $(testGroupGenerator)
