module Examples.First where

import Language.Pollstr_syntax
import Language.Parser
import Test.HUnit

first = "survey First \
\\
\ response bool = {\"Yes\", \"No\"} \
\\
\ Qducks: \"Have you ever seen the movie 'The Mighty Ducks'?\" {\"Yes\", \"No\"} \
\\
\ question movie = \"What is your favorite movie?\" \
\\
\ end First"

--test = runTestTT tests
--tests = TestList[ TestLabel "QLiteral" qLiteral_test
--                , TestLabel "QVar" qVar_test
--                ]


--qLiteral_result = 
--    parseQ "\"Have you ever seen the movie 'The Mighty Ducks'?\""
--qLiteral_expects = Right(Question "Have you ever seen the movie 'The Mighty Ducks'?")
--qLiteral_test = TestCase(assertEqual "QLiteral" qLiteral_expects qLiteral_result)

--qVar_result = parseQ "myQuestion"
--qVar_expects = Qvar "myQuestion"
--qVar_test = TestCase(assertEqual "QVar" qVar_expects qVar_result)



--declarations = [RespDecl("howFrequent", Response ["Never", "Sometimes", "Often", "Always"])]
--items = [Item("teeth", Question "How often do you brush your teeth?", Response ["Never", "Sometimes", "Often", "Always"] )]
--test = Survey("mySurvey", declarations, items)
