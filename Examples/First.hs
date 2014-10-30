module Examples.First where

import Language.Parser
import Test.HUnit hiding (test)
import Test.HUnit.Diff
import Text.ParserCombinators.Parsec
import Text.Parsec.Error

import Language.Pollstr_syntax
import Examples.SimpleSurvey
import Examples.Sections

test = runTestTT tests

tests = TestList[ TestLabel "SimpleSurvey" simple_test,
               --   TestLabel "Sections" sections_test
                ]

-- Testing the simplest survey
simple_expected = parse survey "" simpleSurveyText 
simple_result = Right simpleSurveyAST
simple_test = mkTestCase simple_expected simple_result

--sections_expected = parse survey "" sectionsText 
--sections_result = Right sectionsAST
--sections_test = mkTestCase sections_expected sections_result

instance Eq ParseError where
  (==) e1 e2 = (errorPos e1 == errorPos e2) &&
               (errorMessages e1 == errorMessages e2)

mkTestCase expected seen = TestCase(expected @=? seen)