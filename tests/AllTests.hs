module Main where

import Test.HUnit
import qualified InventoryTest
import qualified GeometryTest

main :: IO ()
main = runTestTTAndExit $ TestList
    [ InventoryTest.tests
    , GeometryTest.tests
    ]
