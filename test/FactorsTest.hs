module FactorsTest where

import Test.HUnit (Test(..), assertEqual) 

import Factors

testFactors :: Test
testFactors = TestCase $ do 
    assertEqual "factors 10" [1, 2, 5, 10] (factors 10)
    assertEqual "factors 16" [1, 2, 4, 8, 16] (factors 16)
    assertEqual "factors 0" [] (factors 0)
    assertEqual "factors 25" [1, 5, 25] (factors 25)


testFactorial :: Test 
testFactorial = TestCase $ do 
    assertEqual "factorial 1" 1 (factorial 1) 
    assertEqual "factorial 3" 6 (factorial 3) 
    assertEqual "factorial 4" 24 (factorial 4) 


testPrimeFactors :: Test
testPrimeFactors = TestCase $ do 
    assertEqual "primeFactors 1" [] (primeFactors 1) 
    assertEqual "primeFactors 10" [2, 5] (primeFactors 10) 
    assertEqual "primeFactors 20" [2, 2, 5] (primeFactors 20) 


factorsTest :: Test
factorsTest = TestList 
    [ TestLabel "testFActors" testFactors 
    , TestLabel "testFactorial" testFactorial
    , TestLabel "testPrimeFactors" testPrimeFactors
    ]
