module InventoryTest where

import Test.HUnit
import Inventory
import Data.Char (toUpper, isUpper)

inventory :: Inventory Char
inventory = fromList [('a', 1), ('b', 2)]

evalPicker' = flip evalPicker inventory
execPicker' = fmap toList . flip execPicker inventory

pickEachPermutation :: InventoryPicker Char Char
pickEachPermutation = pickEach >>= (\p -> lift $ [p, toUpper p])

haveDifferentLetterCase :: Char -> Char -> Bool
haveDifferentLetterCase c c' = isUpper c /= isUpper c'

testPickEach :: Test
testPickEach = TestLabel "pickEach" $ TestList
    [ TestCase $
        assertEqual "should pick the first element of each category"
        ['a', 'b'] $
        evalPicker' pickEach
    , TestCase $
        assertEqual "should leave the inventory in the right state"
        [[('b', 2)], [('a', 1), ('b', 1)]] $
        execPicker' pickEach
    ]

testBindAndLift :: Test
testBindAndLift = TestLabel "bind and lift" $ TestList
    [ TestCase $
        assertEqual "should pick all permutations of the first element of each category"
        ['a', 'A', 'b', 'B']
        (evalPicker' pickEachPermutation)
    , TestCase $
        assertEqual "should leave the inventory in the right state"
        [[('b', 2)], [('b', 2)], [('a', 1), ('b', 1)], [('a', 1), ('b', 1)]] $
        execPicker' pickEachPermutation
    ]

testRepeatedlyPick :: Test
testRepeatedlyPick = TestLabel "repeatedlyPick" $ TestList
    [ TestCase $
        assertEqual "should pick all"
        ["abb", "bba", "bab"] $
        evalPicker' $ repeatedlyPick pickEach
    , TestCase $
        assertEqual "should pick all (with permutations)"
        ["abb","abB","aBb","aBB","Abb","AbB","ABb","ABB","bba","bbA","bBa","bBA","bab","baB","bAb","bAB","Bba","BbA","BBa","BBA","Bab","BaB","BAb","BAB"] $
        evalPicker' $ repeatedlyPick pickEachPermutation
    , TestCase $
        assertEqual "should pick all (with permutations and filter)"
        ["aBb","AbB","bBa","bAb","BbA","BaB"] $
        evalPicker' $ repeatedlyPickWithFilter haveDifferentLetterCase pickEachPermutation
    ]

tests :: Test
tests = TestList
    [ testPickEach
    , testBindAndLift
    , testRepeatedlyPick
    ]

main :: IO ()
main = runTestTTAndExit tests
