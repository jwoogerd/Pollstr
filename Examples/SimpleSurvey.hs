module Examples.SimpleSurvey where

import Language.Pollstr_syntax

{- 
    This is the simplest example of a complete survey. Declarations and what 
    I'm calling 'items' can b mixed in any order at top level. The most notable
    change is in naming items -- items must now have an identifier starting 
    with a capital 'Q'. For right now, responses can only be sets of strings --
    in the future, I plan to expand the response types available.

-}
simpleSurveyText = unlines [
    "survey Simple",
    "", 
    "    response bool = {\"Yes\", \"No\"}", -- response declaration
    "",
    "    Qducks: \"Have you ever seen the movie 'The Mighty Ducks'?\" {\"Yes\", \"No\"}",
    "",
    "    question movie = \"What is your favorite movie?\"", -- question declaration
    "",
    "end Simple"]

simpleSurveyAST = Survey ("Simple", [RespDecl ("bool",Response ["Yes","No"]),
    QuestDecl ("movie",Question "What is your favorite movie?")],
    [Item ("ducks",Question "Have you ever seen the movie 'The Mighty Ducks'?",
    Response ["Yes","No"], None)],[])