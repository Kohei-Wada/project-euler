import Test.HUnit (Test(..), runTestTTAndExit)
import FactorsTest (factorsTest)


allTests = TestList [ TestLabel "factorTest" factorsTest ]
   

main :: IO ()
main = runTestTTAndExit allTests 


