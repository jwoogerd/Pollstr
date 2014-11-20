{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.SimpleSurvey where

import Language.Syntax
import Language.Quote

{- 
    This is the simplest example of a complete survey. Declarations and what 
    I'm calling 'items' can b mixed in any order at top level. The most notable
    change is in naming items -- items must now have an identifier starting 
    with a capital 'Q'. For right now, responses can only be sets of strings --
    in the future, I plan to expand the response types available.

-}

[pollstr| 
    Response bool = ["Yes", "No"]
    Question movie = "What is your favorite movie?"

    Survey Simple: "My first survey"
        Qducks: "Have you ever seen the movie 'The Mighty Ducks'?" ["Yes", "No"]
|]

simpleSurveyText = unlines [
    "Response bool = [\"Yes\", \"No\"]", -- response declaration
    "Question movie = \"What is your favorite movie?\"", -- question declaration

    "Survey Simple: \"My first survey\"",
    "",
    "    Qducks: \"Have you ever seen the movie 'The Mighty Ducks'?\" [\"Yes\", \"No\"]"
    ]

simpleSurveyAST = Survey "Simple" "My first survey" [RespDecl "bool" (Response ["Yes","No"]),
    QuestDecl "movie" (Question "What is your favorite movie?")]
    [Section "Bare" "" [Item "ducks" (Question "Have you ever seen the movie 'The Mighty Ducks'?")
    (Response ["Yes","No"]) None]]