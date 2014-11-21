{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.SimpleSurvey where

import Language.Syntax
import Language.Quote

{- 
    This is the simplest example of a complete survey. This program produces 
    two declarations: surveySimple has type Survey and is the survey represented 
    in Pollstr abstract syntax. The value printSimple is has type String -> IO()
    and produces a LaTeX at the given path, which produces this survey 
    rendered in print.
-}

[pollstr| 
    Response bool = ["Yes", "No"]
    Question movie = "What is your favorite movie?"

    Survey Simple: "My first survey"
        Qducks: "Have you ever seen the movie 'The Mighty Ducks'?" ["Yes", "No"]
|]

{- Expected Pollstr abstract syntax -}

simpleSurveyAST = Survey "Simple" "My first survey" [RespDecl "bool" (Response ["Yes","No"]),
    QuestDecl "movie" (Question "What is your favorite movie?")]
    [Section "Bare" "" [Item "Qducks" (Question "Have you ever seen the movie 'The Mighty Ducks'?")
    (Response ["Yes","No"]) None]]