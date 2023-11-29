module GeometryTest where

import Data.Bifunctor (bimap)
import Data.Function (on)
import Test.HUnit
import Geometry

assertCloseTo :: String -> (Float, Float) -> (Float, Float) -> Assertion
assertCloseTo msg = assertEqual msg `on` bimap Geometry.round Geometry.round

assertCloseTo' :: String -> Position -> Position -> Assertion
assertCloseTo' msg = assertEqual msg `on` canonical

testFromPolar :: Test
testFromPolar = TestLabel "fromPolar" $ TestList
    [ TestCase $ assertCloseTo "should return (1, 0) for (1, 0)" (1, 0) $ fromPolar 1 0
    , TestCase $ assertCloseTo "should return (0, 1) for (1, π/2)" (0, 1) $ fromPolar 1 (pi / 2)
    , TestCase $ assertCloseTo "should return (-1, 0) for (1, π)" (-1, 0) $ fromPolar 1 pi
    , TestCase $ assertCloseTo "should return (0, -1) for (1, 3π/2)" (0, -1) $ fromPolar 1 (3 * pi / 2)
    , TestCase $ assertCloseTo "should return (1/√2, 1/√2) for (1, π/4)" (1 / sqrt 2, 1 / sqrt 2) $ fromPolar 1 (pi / 4)
    ]

testToPolar :: Test
testToPolar = TestLabel "toPolar" $ TestList
    [ TestCase $ assertCloseTo "should return (1, 0) for (1, 0)" (1, 0) $ toPolar 1 0
    , TestCase $ assertCloseTo "should return (1, π/2) for (0, 1)" (1, pi / 2) $ toPolar 0 1
    , TestCase $ assertCloseTo "should return (1, π) for (-1, 0)" (1, pi) $ toPolar (-1) 0
    , TestCase $ assertCloseTo "should return (1, -π/2) for (0, -1)" (1,-pi / 2) $ toPolar 0 (-1)
    , TestCase $ assertCloseTo "should return (1, π/4) for (1/√2, 1/√2)" (1, pi / 4) $ toPolar (1 / sqrt 2) (1 / sqrt 2)
    ]

testRotate :: Test
testRotate = TestLabel "rotate" $ TestList
    [ TestCase $ assertCloseTo "should give same input if rotating by 0" (3, 4) $ rotate 3 4 0
    , TestCase $ assertCloseTo "should give same input if rotating by 0 (even negative numbers)" (3, -4) $ rotate 3 (-4) 0
    , TestCase $ assertCloseTo "should give same input if rotating by 2π" (3, 4) $ rotate 3 4 (2 * pi)
    , TestCase $ assertCloseTo "should give same input if rotating by 4π" (3, 4) $ rotate 3 4 (4 * pi)
    , TestCase $ assertCloseTo "should give opposite if rotating by 3π" (-3, -4) $ rotate 3 4 (3 * pi)
    , TestCase $ assertCloseTo "should swap and negate if rotating by π/2" (-4, 3) $ rotate 3 4 (pi / 2)
    ]

testCanonical :: Test
testCanonical = TestLabel "canonical" $ TestList
    [ TestCase $ assertEqual "should treat (√2, √2, π) as the same as (1.4142, 1.4142, 3.1416)" (Position 1.4142 1.4142 3.1416) $ canonical (Position (sqrt 2) (sqrt 2) (3 * pi))
    ]

testTransform :: Test
testTransform = TestLabel "transform" $ TestList
    [ TestCase $ assertCloseTo' "given mempty, should return the input" (Position 5 3 pi) $ Position 5 3 pi ~> mempty
    , TestCase $ assertCloseTo' "P(1,0,0) ~> T(2,0,π) should give P(3,0,π)" (Position 3 0 pi) $ Position 1 0 0 ~> Transformation 2 0 pi
    , TestCase $ assertCloseTo' "P(1,0,π/2) ~> T(2,0,π) should give P(1,2,3π/2)" (Position 1 2 (3 * pi / 2)) $ Position 1 0 (pi / 2) ~> Transformation 2 0 pi
    , TestCase $ assertCloseTo' "P(1,1,π/4) ~> T(1,0,0) should give P(1+√2,1+√2,π/4)" (Position (1 + sqrt (1/2)) (1 + sqrt (1/2)) (pi / 4)) $ Position 1 1 (pi / 4) ~> Transformation 1 0 0
    , TestCase $ assertCloseTo' "P(1,1,π/4) ~> T(1,1,π/4) should give P(1,1+√2,π/2)" (Position 1 (1 + sqrt 2) (pi / 2)) $ Position 1 1 (pi / 4) ~> Transformation 1 1 (pi / 4)
    , TestCase $ assertCloseTo' "P(0,0,0) ~> T(1,-1,π/4) should give P(1,-1,π/4)" (Position 1 (-1) (pi / 4)) $ Position 0 0 0 ~> Transformation 1 (-1) (pi / 4)
    ]

tests :: Test
tests = TestList
    [ testRotate
    , testToPolar
    , testFromPolar
    , testCanonical
    , testTransform
    ]
