module Examples.SimpleSurvey where

import Language.Pollstr_syntax

simpleSurveyText = unlines [
    "survey Simple",
    "", 
    "    response bool = {\"Yes\", \"No\"}",
    "",
    "    Qducks: \"Have you ever seen the movie 'The Mighty Ducks'?\" {\"Yes\", \"No\"}",
    "",
    "    question movie = \"What is your favorite movie?\"",
    "",
    "end Simple"]

simpleSurveyAST = Survey ("Simple", [RespDecl ("bool",Response ["Yes","No"]),
    QuestDecl ("movie",Question "What is your favorite movie?")],
    [Item ("ducks",Question "Have you ever seen the movie 'The Mighty Ducks'?",
    Response ["Yes","No"])],[])