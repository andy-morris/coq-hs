{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-fields #-}
module TestCoq.XmlQQ (xml) where

import Coq.XmlParser
import Data.Text
import Data.Attoparsec.Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

instance Lift Text where lift txt = [|pack $(lift (unpack txt))|]
deriving instance Lift Node
deriving instance Lift Attr
deriving instance Lift Child

xml :: QuasiQuoter
xml = QuasiQuoter { quoteExp = quoteXml }

quoteXml :: String -> ExpQ
quoteXml str =
    case parseOnly (skipSpace *> node <* skipSpace) (pack str) of
      Left err -> fail err
      Right x  -> lift x
