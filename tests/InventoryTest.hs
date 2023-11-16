module Main where

import Inventory (FlattenInventory, permutations', pick, toForest')
import Data.Char (toUpper)
import Data.List (singleton)
import Data.Tree (Tree(..))
import Test.HUnit
import qualified System.Exit as Exit

inventory :: FlattenInventory Char Int
inventory = [('a', 1), ('b', 2)]

testPick :: Test
testPick = TestCase (assertEqual "should pick one element and leave the rest of the inventory intact" [('a', [('b', 2)]), ('b', [('b', 1), ('a', 1)])] (pick inventory))

testToForest :: Test
testToForest = TestCase (assertEqual "should transform to forest" [Node 'a' [(Node 'b' [(Node 'b' [])])], Node 'b' [(Node 'b' [Node 'a' []]), (Node 'a' [Node 'b' []])]] (toForest' inventory))

testPermutations :: Test
testPermutations = TestCase (assertEqual "should list all unique permutations" ["abb", "bba", "bab"] (permutations' singleton inventory))

testPermutations2 :: Test
testPermutations2 = TestCase (assertEqual "should list all unique permutations"
    ["abb","abB","aBb","aBB","Abb","AbB","ABb","ABB","bba","bbA","bBa","bBA","bab","baB","bAb","bAB","Bba","BbA","BBa","BBA","Bab","BaB","BAb","BAB"] (permutations' (\c -> c:[toUpper c]) inventory))

tests :: Test
tests = TestList
    [ TestLabel "testPick" testPick
    , TestLabel "testToForest" testToForest
    , TestLabel "testPermutations" testPermutations
    , TestLabel "testPermutations2" testPermutations2
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
