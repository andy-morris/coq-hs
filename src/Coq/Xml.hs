{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- | XML encoding/decoding for messages to/from @coqtop@.
module Coq.Xml
  (XmlEncode (..),
   XmlDecode (..), XmlDecoder,
   XmlMessage (..))
where

import Prelude hiding (negate)
import Coq.Protocol
import Text.Read (readMaybe)
import Text.XML.Light
import Control.Applicative
import Control.Monad
import Data.Text (Text, pack, unpack)


-- | Encode an outgoing message as XML.
class XmlEncode a where
    -- | Encode a message.
    encode :: a -> Element

-- | Function for matching against an XML fragment.
type XmlDecoder a = Element -> Maybe a

-- | Try to decode an XML document as an incoming message.
class XmlDecode a where
    -- | Decode a message.
    decode :: XmlDecoder a

-- | Class for (outgoing) messages which associates them with their
-- response and their RPC name.
class (XmlEncode a, XmlDecode (Resp a)) => XmlMessage a where
    -- | Response type
    type Resp a = r | r -> a
    -- | Call name, which is used in the @val@ attribute in a @call@
    -- element.
    callName :: a -> String




instance XmlEncode StateId where
    encode (StateId i) = unode "state_id" (uattr "val" (show i))

instance XmlDecode StateId where
    decode = decodeUnion "state_id" $ \case
        (x, []) -> StateId <$> readMaybe x
        _       -> mzero

decodeUnion :: String -> ((String, [Element]) -> Maybe a) -> XmlDecoder a
decodeUnion typ fun elt = do
    Element e [Attr a con] es' _ <- pure elt
    guard (and [e ==. typ, a ==. "val"])
    es <- traverse unElem es'
    fun (con, es)

instance XmlEncode Bool where
    encode True  = unode "bool" (uattr "val" "true")
    encode False = unode "bool" (uattr "val" "false")

instance XmlDecode Bool where
    decode = decodeUnion "bool" $ \case
        ("true",  []) -> pure True
        ("false", []) -> pure False
        _             -> mzero


instance XmlEncode Int where encode = unode "int" . show

instance XmlDecode Int where
    decode elt = do
        Element e [] [Text txt] _ <- pure elt
        guard (e ==. "int")
        readMaybe (cdData txt)


instance XmlEncode Text where encode = unode "string" . unpack

instance XmlDecode Text where
    decode elt = do
        Element e [] [Text txt] _ <- pure elt
        guard (e ==. "string")
        pure (pack (cdData txt))


instance XmlEncode a => XmlEncode (Maybe a) where
    encode (Just x) = unode "option" (uattr "val" "some", encode x)
    encode Nothing  = unode "option" (uattr "val" "none")

instance XmlDecode a => XmlDecode (Maybe a) where
    decode = decodeMaybe decode

decodeMaybe :: XmlDecoder a -> XmlDecoder (Maybe a)
decodeMaybe dec = decodeUnion "option" $ \case
    ("some", [e]) -> Just <$> dec e
    ("none", [])  -> pure Nothing
    _             -> mzero


instance XmlEncode () where encode () = unode "unit" ()

instance XmlDecode () where
    decode = decodeUnit

decodeUnit :: XmlDecoder ()
decodeUnit elt = do
    Element e [] [] _ <- pure elt
    guard (e ==. "unit")


instance (XmlEncode a, XmlEncode b) => XmlEncode (a, b) where
    encode (a, b) = unode "pair" [encode a, encode b]

instance (XmlDecode a, XmlDecode b) => XmlDecode (a, b) where
    decode = decodePair decode decode

decodePair :: XmlDecoder a -> XmlDecoder b -> XmlDecoder (a, b)
decodePair dec1 dec2 elt = do
    Element e [] [Elem a, Elem b] _ <- pure elt
    guard (e ==. "pair")
    liftA2 (,) (dec1 a) (dec2 b)


instance XmlEncode a => XmlEncode [a] where
    encode = unode "list" . map encode

instance XmlDecode a => XmlDecode [a] where
    decode = decodeList decode

decodeList :: XmlDecoder a -> XmlDecoder [a]
decodeList dec elt = do
    Element e [] es _ <- pure elt
    guard (e ==. "list")
    traverse (dec <=< unElem) es


instance (XmlEncode a, XmlEncode b) => XmlEncode (Either a b) where
    encode (Left  x) = encodeConstructor "union" "in_l" x
    encode (Right x) = encodeConstructor "union" "in_r" x

instance (XmlDecode a, XmlDecode b) => XmlDecode (Either a b) where
    decode = decodeUnion "union" $ \case
        ("in_l", [e]) -> Left  <$> decode e
        ("in_r", [e]) -> Right <$> decode e
        _             -> mzero

decodeRecord :: String -> XmlDecoder [Element]
decodeRecord str elt = do
    Element e [] es _ <- pure elt
    guard (e ==. str)
    traverse unElem es

encodeConstructor :: XmlEncode a => String -> String -> a -> Element
encodeConstructor con name arg =
    unode con (uattr "val" name, encode arg)


instance XmlEncode Init where encode = encode . iFilename

instance XmlDecode InitResp where decode = fmap InitResp . decode

instance XmlMessage Init where
    type Resp Init = InitResp
    callName _ = "Init"


instance XmlEncode About where encode _ = encode ()

instance XmlDecode AboutResp where
    decode elt = do
        [coqtop, proto, rel, comp] <- decodeRecord "coq_info" elt
        liftA4 AboutResp (decode coqtop) (decode proto)
                         (decode rel)    (decode comp)

instance XmlMessage About where
    type Resp About = AboutResp
    callName _ = "About"


instance XmlEncode Status where encode = encode . sForceEval

instance XmlDecode StatusResp where
    decode elt = do
        [path, nm, proofs, num] <- decodeRecord "status" elt
        liftA4 StatusResp (decode path) (decode nm)
                          (decode proofs) (decode num)

instance XmlMessage Status where
    type Resp Status = StatusResp
    callName _ = "Status"


instance XmlEncode Add where
    encode (Add {..}) = encode ((aPhrase, aEditId), (aStateId, aVerbose))

instance XmlDecode AddResp where
    decode elt = do
        (arStateId, (arEditPoint', arMessage)) <- decode elt
        pure (AddResp {arEditPoint = unEither arEditPoint', ..})

instance XmlMessage Add where
    type Resp Add = AddResp
    callName _ = "Add"


instance XmlEncode EditAt where encode (EditAt {..}) = encode eStateId

instance XmlDecode EditAtResp where
    decode elt = do
        e <- decode elt
        case e of
            Left  ()                         -> pure EditAtNewTip
            Right (erStart, (erStop, erTip)) -> pure (EditAtFocus {..})

instance XmlMessage EditAt where
    type Resp EditAt = EditAtResp
    callName _ = "EditAt"


instance XmlEncode Query where
    encode (Query {..}) = encode (qQuery, qStateId)

instance XmlDecode QueryResp where decode = fmap QueryResp . decode

instance XmlMessage Query where
    type Resp Query = QueryResp
    callName _ = "Query"


instance XmlEncode Goal where encode _ = encode ()

instance XmlDecode GoalResp where
    decode elt = do
        mb <- decodeMaybe (decodeRecord "goals") elt
        case mb of
            Nothing -> pure GoalNotInProof
            Just [fg, bg, sh, gu] ->
                liftA4 GoalResp (decode fg) (decode bg)
                                (decode sh) (decode gu)
            _ -> mzero

instance XmlDecode GoalInfo where
    decode elt = do
        [g, h, c] <- decodeRecord "goal" elt
        liftA3 GoalInfo (decode g) (decode h) (decode c)

instance XmlMessage Goal where
    type Resp Goal = GoalResp
    callName _ = "Goal"


instance XmlEncode Evars where encode _ = encode ()

instance XmlDecode EvarsResp where
    decode = fmap (maybe EvarsNotInProof EvarsResp) . decode

instance XmlDecode Evar where decode = fmap Evar . decode

instance XmlMessage Evars where
    type Resp Evars = EvarsResp
    callName _ = "Evars"


instance XmlEncode Hints where encode _ = encode ()

instance XmlDecode HintsResp where
    decode = fmap (maybe HintsNotInProof (uncurry HintsResp)) . decode

instance XmlDecode Hint where
    decode = fmap (uncurry Hint) . decode

instance XmlMessage Hints where
    type Resp Hints = HintsResp
    callName _ = "Hints"


instance XmlEncode Search where encode = encode . sFlags

instance XmlEncode SearchFlag where
    encode (SearchFlag {..}) = encode (sfConstraint, sfNegate)

instance XmlEncode SearchConstraint where
    encode cst =
        case cst of
            NamePattern txt    -> encodeCst "name_pattern" txt
            TypePattern txt    -> encodeCst "type_pattern" txt
            SubTypePattern txt -> encodeCst "subtype_pattern" txt
            InModule txts      -> encodeCst "in_module" txts
            IncludeBlacklist   ->
                unode "search_cst" (uattr "val" "include_blacklist")
      where
        encodeCst x = encodeConstructor "search_cst" x


instance XmlDecode SearchResp where decode = fmap SearchResp . decode

instance XmlDecode a => XmlDecode (CoqObject a) where
    decode elt = do
        [pre, qid, obj] <- decodeRecord "coq_object" elt
        liftA3 CoqObject (decode pre) (decode qid) (decode obj)

instance XmlMessage Search where
    type Resp Search = SearchResp
    callName _ = "Search"


instance XmlEncode GetOptions where encode _ = encode ()

instance XmlDecode GetOptionsResp where
    decode = fmap GetOptionsResp . decode

instance XmlDecode Option where decode = fmap (uncurry Option) . decode

instance XmlDecode OptionState where
    decode elt = do
        [sync, dep, nm, val] <- decodeRecord "option_state" elt
        liftA4 OptionState (decode sync) (decode dep)
                           (decode nm)   (decode val)

instance XmlDecode OptionValue where
    decode elt = do
        Element e [Attr a x] [Elem y] _ <- pure elt
        guard (and [e ==. "option_value", a ==. "val"])
        case x of
            "boolvalue"      -> BoolValue      <$> decode y
            "intvalue"       -> IntValue       <$> decode y
            "stringvalue"    -> StringValue    <$> decode y
            "stringoptvalue" -> StringOptValue <$> decode y
            _                -> mzero

instance XmlMessage GetOptions where
    type Resp GetOptions = GetOptionsResp
    callName _ = "GetOptions"


instance XmlEncode SetOptions where
    encode (SetOptions opts) = encode opts

instance XmlEncode SetOption where
    encode (SetOption {..}) = encode (soName, soValue)

instance XmlEncode OptionValue where
    encode val =
        case val of
            BoolValue x      -> optionValue "boolvalue" x
            IntValue x       -> optionValue "intvalue" x
            StringValue x    -> optionValue "stringvalue" x
            StringOptValue x -> optionValue "stringoptvalue" x
      where
        optionValue x = encodeConstructor "option_value" x

instance XmlDecode SetOptionsResp where
    decode elt = SetOptionsResp <$ decodeUnit elt

instance XmlMessage SetOptions where
    type Resp SetOptions = SetOptionsResp
    callName _ = "SetOptions"


instance XmlEncode MakeCases where encode = encode . mcTypeName

instance XmlDecode MakeCasesResp where decode = fmap MakeCasesResp . decode

instance XmlMessage MakeCases where
    type Resp MakeCases = MakeCasesResp
    callName _ = "MakeCases"


instance XmlEncode Quit where encode _ = encode ()

instance XmlDecode QuitResp where
    decode elt = QuitResp <$ decodeUnit elt

instance XmlMessage Quit where
    type Resp Quit = QuitResp
    callName _ = "Quit"


unEither :: Either () a -> Maybe a
unEither = either (const Nothing) Just

liftA4 :: Applicative f => (a -> b -> c -> d -> e)
       -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

uattr :: String -> String -> Attr
uattr = Attr . unqual

(==.) :: QName -> String -> Bool
q ==. s = q == unqual s

unElem :: Content -> Maybe Element
unElem (Elem e) = pure e
unElem _        = mzero