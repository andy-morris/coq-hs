{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | XML encoding/decoding for messages to/from @coqtop@.
module Coq.Xml
  (Encode (..),
   Decode (..), Decoder,
   Message (..),
   makeCall, fromResponse, Response (..), Location (..),
   Node (..), Attr (..), Child (..))
where

import Prelude hiding (negate, id, mod)
import Coq.Protocol
import Coq.XmlAst
import Text.Read (readMaybe)
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)



-- | Encode an outgoing message as XML.
class Encode a where
    -- | Encode a message.
    encode :: a -> Node

-- | Function for matching against an XML fragment.
type Decoder a = Node -> Maybe a

-- | Try to decode an XML document as an incoming message.
class Decode a where
    -- | Decode a message.
    decode :: Decoder a

-- | Class for messages which associates requests with responses and gives
-- their RPC call name.
class (Encode rq, Decode rs) => Message rq rs | rq -> rs, rs -> rq where
    -- | Call name, which is used in the @val@ attribute in a @call@
    -- element.
    callName :: rq -> Text


makeCall :: Message rq rs => rq -> Node
makeCall x = Node "call" ["val" := callName x] [N (encode x)]


-- | A response from @coqtop@.
data Response a =
    -- | The command failed
    Failure (Maybe Location) StateId RichPP
    -- | Couldn't understand @coqtop@'s response
  | DecodeError
    -- | The command was successful
  | Success a
  deriving (Eq, Show)


fromResponse :: Message rq rs => Node -> Response rs
fromResponse (Node "value" attrs elts) =
    fromMaybe DecodeError $ do
      decodeSuccess elts <|> decodeFailure elts
  where
    decodeSuccess [N e] = do
        "good" <- lookupAttr "val" attrs
        Success <$> decode e
    decodeSuccess _ = empty
    decodeFailure [N sid', N err] = do
        "fail" <- lookupAttr "val" attrs
        let loc = locFromAttrs "loc_s" "loc_e" attrs
        sid <- decode sid'
        rpp <- decode err
        pure (Failure loc sid rpp)
    decodeFailure _ = empty
fromResponse _ = DecodeError


instance Encode StateId where
    encode (StateId i) = encodeId "state_id" i

instance Decode StateId where
    decode = decodeUnion "state_id" $ \case
        (x, []) -> StateId <$> tread x
        _       -> empty

encodeId :: Text -> Int -> Node
encodeId typ i = Node typ ["val" := tshow i] []

decodeUnion' :: Text -> ((Text, [Child]) -> Maybe a) -> Decoder a
decodeUnion' typ fun (Node e ["val" := con] es) = do
    guard (e == typ)
    fun (con, es)
decodeUnion' _ _ _ = empty

decodeUnion :: Text -> ((Text, [Node]) -> Maybe a) -> Decoder a
decodeUnion typ fun = decodeUnion' typ $ \(con, es') -> do
    es <- traverse toNode es'
    fun (con, es)


instance Encode EditId where
    encode (EditId i) = encodeId "edit_id" i

instance Decode EditId where
    decode = decodeUnion "edit_id" $ \case
        (x, []) -> EditId <$> tread x
        _       -> empty


instance Decode Location where
    decode (Node "loc" attrs _) = locFromAttrs "start" "stop" attrs
    decode _ = empty

locFromAttrs :: Text -> Text -> [Attr] -> Maybe Location
locFromAttrs start stop attrs =
    liftA2 Location (readMaybe . Text.unpack =<< lookupAttr start attrs)
                    (readMaybe . Text.unpack =<< lookupAttr stop  attrs)


instance Encode Bool where
    encode True  = Node "bool" ["val" := "true"]  []
    encode False = Node "bool" ["val" := "false"] []

instance Decode Bool where
    decode = decodeUnion "bool" $ \case
        ("true",  []) -> pure True
        ("false", []) -> pure False
        _             -> empty


instance Encode Int where
    encode x = Node "int" [] [textChild (tshow x)]

instance Decode Int where
    decode (Node "int" _ [T _ txt]) = tread txt
    decode _                        = empty


instance Encode Text where
    encode txt = Node "string" [] [textChild txt]

instance Decode Text where
    decode (Node "string" _ txts) = concatTexts txts
    decode _                      = empty

concatTexts :: [Child] -> Maybe Text
concatTexts = fmap Text.concat . traverse toText

instance Encode a => Encode (Maybe a) where
    encode (Just x) = Node "option" ["val" := "some"] [N (encode x)]
    encode Nothing  = Node "option" ["val" := "none"] []

instance Decode a => Decode (Maybe a) where
    decode = decodeMaybe decode

decodeMaybe :: Decoder a -> Decoder (Maybe a)
decodeMaybe dec = decodeUnion "option" $ \case
    ("some", [e]) -> Just <$> dec e
    ("none", [])  -> pure Nothing
    _             -> empty


instance Encode () where encode () = Node "unit" [] []

instance Decode () where decode = decodeUnit

decodeUnit :: Decoder ()
decodeUnit (Node "unit" _ []) = pure ()
decodeUnit _                  = empty


instance (Encode a, Encode b) => Encode (a, b) where
    encode (a, b) = Node "pair" [] [N (encode a), N (encode b)]

instance (Decode a, Decode b) => Decode (a, b) where
    decode = decodePair decode decode

decodePair :: Decoder a -> Decoder b -> Decoder (a, b)
decodePair dec1 dec2 (Node "pair" _ [N a, N b]) =
    liftA2 (,) (dec1 a) (dec2 b)
decodePair _ _ _ = empty


instance Encode a => Encode [a] where
    encode = Node "list" [] . map (N . encode)

instance Decode a => Decode [a] where
    decode = decodeList decode

decodeList :: Decoder a -> Decoder [a]
decodeList dec (Node "list" _ es) = traverse (dec <=< toNode) es
decodeList _ _ = empty


instance (Encode a, Encode b) => Encode (Either a b) where
    encode (Left  x) = encodeConstructor "union" "in_l" x
    encode (Right x) = encodeConstructor "union" "in_r" x

instance (Decode a, Decode b) => Decode (Either a b) where
    decode = decodeUnion "union" $ \case
        ("in_l", [e]) -> Left  <$> decode e
        ("in_r", [e]) -> Right <$> decode e
        _             -> empty

decodeRecord :: Text -> Decoder [Node]
decodeRecord str (Node e [] es) = do
    guard (e == str)
    traverse toNode es
decodeRecord _ _ = empty


encodeConstructor :: Encode a => Text -> Text -> a -> Node
encodeConstructor con name arg =
    Node con ["val" := name] [N (encode arg)]


instance Encode Init where encode = encode . iFilename

instance Decode InitResp where decode = fmap InitResp . decode

instance Message Init InitResp where
    callName _ = "Init"


instance Encode About where encode _ = encode ()

instance Decode AboutResp where
    decode elt = do
        [coqtop, proto, rel, comp] <- decodeRecord "coq_info" elt
        liftA4 AboutResp (decode coqtop) (decode proto)
                         (decode rel)    (decode comp)

instance Message About AboutResp where
    callName _ = "About"


instance Encode Status where encode = encode . sForceEval

instance Decode StatusResp where
    decode elt = do
        [path, nm, proofs, num] <- decodeRecord "status" elt
        liftA4 StatusResp (decode path) (decode nm)
                          (decode proofs) (decode num)

instance Message Status StatusResp where
    callName _ = "Status"


instance Encode Add where
    encode (Add {..}) =
        encode ((aPhrase, unEditId aEditId), (aStateId, aVerbose))

instance Decode AddResp where
    decode elt = do
        (arStateId, (arEditPoint', arMessage)) <- decode elt
        pure (AddResp {arEditPoint = unEither arEditPoint', ..})

instance Message Add AddResp where
    callName _ = "Add"


instance Encode EditAt where encode (EditAt {..}) = encode eStateId

instance Decode EditAtResp where
    decode elt = do
        e <- decode elt
        case e of
            Left  ()                         -> pure EditAtNewTip
            Right (erStart, (erStop, erTip)) -> pure (EditAtFocus {..})

instance Message EditAt EditAtResp where
    callName _ = "Edit_at"


instance Encode Query where
    encode (Query {..}) = encode (qQuery, qStateId)

instance Decode QueryResp where decode = fmap QueryResp . decode

instance Message Query QueryResp where
    callName _ = "Query"


instance Encode Goal where encode _ = encode ()

instance Decode GoalResp where
    decode elt = do
        mb <- decodeMaybe (decodeRecord "goals") elt
        case mb of
            Nothing -> pure GoalNotInProof
            Just [fg, bg, sh, gu] ->
                liftA4 GoalResp (decode fg) (decode bg)
                                (decode sh) (decode gu)
            _ -> empty

instance Decode GoalInfo where
    decode elt = do
        [g, h, c] <- decodeRecord "goal" elt
        liftA3 GoalInfo (decode g) (decode h) (decode c)

instance Message Goal GoalResp where
    callName _ = "Goal"


instance Encode Evars where encode _ = encode ()

instance Decode EvarsResp where
    decode = fmap (maybe EvarsNotInProof EvarsResp) . decode

instance Decode Evar where decode = fmap Evar . decode

instance Message Evars EvarsResp where
    callName _ = "Evars"


instance Encode Hints where encode _ = encode ()

instance Decode HintsResp where
    decode = fmap (maybe HintsNotInProof (uncurry HintsResp)) . decode

instance Decode Hint where
    decode = fmap (uncurry Hint) . decode

instance Message Hints HintsResp where
    callName _ = "Hints"


instance Encode Search where encode = encode . sFlags

instance Encode SearchFlag where
    encode (SearchFlag {..}) = encode (sfConstraint, sfNegate)

instance Encode SearchConstraint where
    encode cst =
        case cst of
            NamePattern txt    -> encodeCst "name_pattern" txt
            TypePattern txt    -> encodeCst "type_pattern" txt
            SubTypePattern txt -> encodeCst "subtype_pattern" txt
            InModule txts      -> encodeCst "in_module" txts
            IncludeBlacklist   ->
                Node "search_cst" ["val" := "include_blacklist"] []
      where
        encodeCst x = encodeConstructor "search_cst" x


instance Decode SearchResp where decode = fmap SearchResp . decode

instance Decode a => Decode (CoqObject a) where
    decode elt = do
        [pre, qid, obj] <- decodeRecord "coq_object" elt
        liftA3 CoqObject (decode pre) (decode qid) (decode obj)

instance Message Search SearchResp where
    callName _ = "Search"


instance Encode GetOptions where encode _ = encode ()

instance Decode GetOptionsResp where
    decode = fmap GetOptionsResp . decode

instance Decode Option where decode = fmap (uncurry Option) . decode

instance Decode OptionState where
    decode elt = do
        [sync, dep, desc, val] <- decodeRecord "option_state" elt
        liftA4 OptionState (decode sync) (decode dep)
                           (decode desc) (decode val)

instance Decode OptionValue where
    decode = decodeUnion "option_value" $ \case
        ("boolvalue",      [e]) -> BoolValue      <$> decode e
        ("intvalue",       [e]) -> IntValue       <$> decode e
        ("stringvalue",    [e]) -> StringValue    <$> decode e
        ("stringoptvalue", [e]) -> StringOptValue <$> decode e
        _                       -> empty

instance Message GetOptions GetOptionsResp where
    callName _ = "GetOptions"


instance Encode SetOptions where
    encode (SetOptions opts) = encode opts

instance Encode SetOption where
    encode (SetOption {..}) = encode (soName, soValue)

instance Encode OptionValue where
    encode val =
        case val of
            BoolValue x      -> optionValue "boolvalue" x
            IntValue x       -> optionValue "intvalue" x
            StringValue x    -> optionValue "stringvalue" x
            StringOptValue x -> optionValue "stringoptvalue" x
      where
        optionValue x = encodeConstructor "option_value" x

instance Decode SetOptionsResp where
    decode elt = SetOptionsResp <$ decodeUnit elt

instance Message SetOptions SetOptionsResp where
    callName _ = "SetOptions"


instance Encode MakeCases where encode = encode . mcTypeName

instance Decode MakeCasesResp where decode = fmap MakeCasesResp . decode

instance Message MakeCases MakeCasesResp where
    callName _ = "MkCases"


instance Encode Quit where encode _ = encode ()

instance Decode QuitResp where
    decode elt = QuitResp <$ decodeUnit elt

instance Message Quit QuitResp where
    callName _ = "Quit"


instance Decode Feedback where
    decode (Node "feedback" attrs [N id', N content']) = do
        obj <- lookupAttr "object" attrs
        id  <- case obj of
            "edit"  -> Left  <$> decode id'
            "state" -> Right <$> decode id'
            _       -> empty
        route   <- readMaybe . Text.unpack =<< lookupAttr "route" attrs
        content <- decode content'
        pure (Feedback id content (RouteId route))
    decode (Node "message" _ [N lvl, N msg]) =
        liftA2 Message (decode lvl) (decode msg)
    decode _ = empty

instance Decode FeedbackContent where
    decode = decodeUnion "feedback_content" $ \case
        ("addedaxiom", _) -> pure AddedAxiom
        ("processed", _) -> pure Processed
        ("processingin", [loc]) -> ProcessingIn <$> decode loc
        ("incomplete", _) -> pure Incomplete
        ("complete", _) -> pure Complete
        ("globref", [loc, file, mod, id, ty]) ->
            liftA5 GlobRef (decode loc) (decode file) (decode mod)
                           (decode id)  (decode ty)
        ("globdef", [loc, id, sec, ty]) ->
            liftA4 GlobDef (decode loc) (decode id) (decode sec)
                           (decode ty)
        ("errormsg", [loc, txt]) ->
            liftA2 ErrorMsg (decode loc) (decode txt)
        ("inprogress", [n]) -> InProgress <$> decode n
        ("workerstatus", [pair]) -> uncurry WorkerStatus <$> decode pair
        ("goals", [loc, txt]) -> liftA2 Goals (decode loc) (decode txt)
        ("custom", [loc, name, x]) ->
            liftA3 Custom (decode loc) (decode name) (pure x)
        ("filedependency", [from, dep]) ->
            liftA2 FileDependency (decode from) (decode dep)
        ("fileloaded", [dir, file]) ->
            liftA2 FileLoaded (decode dir) (decode file)
        _ -> empty

instance Decode MessageLevel where
    decode = decodeUnion' "message_level" $ \case
        ("debug", [T _ txt]) -> pure (LDebug txt)
        ("info", _)          -> pure LInfo
        ("notice", _)        -> pure LNotice
        ("warning", _)       -> pure LWarning
        ("error", _)         -> pure LError
        _                    -> empty


instance Encode RichPP where
    encode (RPP fs) =
        Node "richpp" [] [N (Node "_" [] $ map encodeFrag fs)]

encodeFrag :: RPPFragment -> Child
encodeFrag (RText txt)     = textChild txt
encodeFrag (RFormat cs fs) = N $ Node name [] (map encodeFrag fs)
  where name = Text.intercalate "." cs

instance Decode RichPP where
    decode (Node "richpp" [] [N (Node "_" [] fs)]) =
      RPP <$> traverse decodeFrag fs
    decode _ = empty

decodeFrag :: Child -> Maybe RPPFragment
decodeFrag (T txt _)           = pure (RText txt)
decodeFrag (N (Node cs [] fs)) = RFormat cs' <$> traverse decodeFrag fs
  where cs' = Text.splitOn "." cs
decodeFrag _ = empty



unEither :: Either () a -> Maybe a
unEither = either (const Nothing) Just

liftA4 :: Applicative f => (a -> b -> c -> d -> e)
       -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> x)
       -> f a -> f b -> f c -> f d -> f e -> f x
liftA5 f a b c d e = liftA4 f a b c d <*> e

tread :: Read a => Text -> Maybe a
tread = readMaybe . Text.unpack

tshow :: Show a => a -> Text
tshow = Text.pack . show
