{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.SimpleSurvey where

import Language.Syntax
import Language.Quote

{- 
    This is the simplest example of a complete survey. This program produces 
    two declarations: surveySimple has type Survey and is the survey represented 
    in Pollstr abstract syntax. The value printSimple is has type String -> IO()
    and produces a LaTeX at the given path, which produces this survey 
    rendered in print. A change in the grammar: a survey can either have sections
    OR top-level 'bare' items, but no longer can have both (see Flow.hs and 
    Sections.hs for and examples of sectioning).

    E.g in ghci: printSimple "Examples/Latex/simple.tex"
-}

[pollstr| 
    Response bool = ["Yes", "No"]
    Question movie = "What is your favorite movie?"

    Survey Simple: "My first survey"
        Q1: "Have you ever seen the movie 'The Mighty Ducks'?" ["Yes", "No"]
|]
