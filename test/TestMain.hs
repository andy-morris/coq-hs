module Main (main) where

import qualified TestCoq.Xml as Xml
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "tests" $
    [Xml.tests]
