{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.First where

import Language.Parser
import Test.HUnit hiding (test)
import Test.HUnit.Diff
import Text.ParserCombinators.Parsec
import Text.Parsec.Error

import Language.Syntax
import Language.Quote
import Examples.SimpleSurvey
import Examples.Sections
import Examples.Flow

{- Pollstr testing suite -}

test = runTestTT tests

tests = TestList[ TestLabel "SimpleSurvey" simple_test
                , TestLabel "Sections" sections_test
                , TestLabel "Flow" flow_test
                ]

-- Testing the simplest survey
simple_result = surveySimple
simple_expected = simpleSurveyAST
simple_test = mkTestCase simple_expected simple_result

-- Testing sections, including nested sections
sections_result = surveySections
sections_expected = sectionsAST
sections_test = mkTestCase sections_expected sections_result

-- Testing flow control with skips
flow_result = surveyFlow
flow_expected = flowAST
flow_test = mkTestCase flow_expected flow_result


{-
    testing infrastructure (borrowed from Will and Andrew)
-}
instance Eq ParseError where
  (==) e1 e2 = (errorPos e1 == errorPos e2) &&
               (errorMessages e1 == errorMessages e2)

mkTestCase expected seen = TestCase(expected @=? seen)