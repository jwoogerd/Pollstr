{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.SimpleSurvey where

import Language.Syntax
import Language.Quote

{- 
    This is the simplest example of a complete survey. This program produces 
    three declarations: printSimple, toJSONSimple, and toJSONSimple'.
    The value printSimple is has type FilePath -> IO() and produces a LaTeX file
    at the given path, which produces this survey rendered in print. Similarly, 
    toJSONSimple takes a filepath and prints a JSON structure representing
    the survey at that path. Finally, toJSONSimple' returns the JSON structure
    as a ByteString, which can be used for testing.

    A change in the grammar: a survey can either have sections
    OR top-level 'bare' items, but no longer can have both (see Flow.hs and 
    Sections.hs for and examples of sectioning).

    E.g in ghci: printSimple "Examples/Latex/simple.tex"
    E.g in ghci: toJSONSimple "Examples/Latex/simple.json"
-}

[pollstr| 
    Response bool = ["Yes", "No"]
    Question movie = "What is your favorite movie?"

    Survey Simple: 
        Title: "My first survey"
        Q1: "Have you ever seen the movie 'The Mighty Ducks'?" ["Yes", "No"]
|]
