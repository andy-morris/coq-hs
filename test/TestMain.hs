module Main (main) where

import qualified TestCoq.XmlParser as XmlParser
import qualified TestCoq.Xml as Xml
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "tests" $
    [XmlParser.tests,
     Xml.tests]
