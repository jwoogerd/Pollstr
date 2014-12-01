{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Demo where

import Language.Syntax
import Language.Quote

{-
    This example shows multiple sections and declarations, as well as skips. 
    This program generates three declarations: printDemo, toJSONDemo,
    and toJSONDemo'.
-}

[pollstr|
    Response howFrequent = ["Never", "Sometimes", "Often", "Always"]
    Response bool        = ["Yes", "No"]
    Response ff          = ["Fact", "Fiction"]

    Survey Demo:
        Title: "An Example Survey"
        Author: "Jayme Woogerd"
        Description: "This Pollstr program demonstrates key features, such " ++
                     "as bound variables, sections, and skip logic. Enjoy!"

        Section First: "Would You Rather?"
            Q1: "Would you rather be stuck in a closure with Ming Chow or " ++
                "Norman Ramsey?"
                ["\"Have anyone ever heard of a thing called...\"",
                 "\"I guess I misjudged how long that homework would take.\""]
                skipTo(Q3, ["\"Have anyone ever heard of a thing called...\""])

            Q2: "Would you rather have to wait for everything in real life to " ++
                "compile or have an interpreter follow your around all the time, " ++
                "throwing errors at you?"
                ["Compile", "Interpreter"]

            Q3: "Would you rather only be able to speak in Perl or only be " ++
                "able to talk when others are talking about Perl? " 
                ["Speak in Perl", "Talk when others are talking about Perl"]

            Q4: "Would you rather be a garbage collector or a nomad monad?"
                ["Garbage collector!", "Nomad monad!"]

        Section Second: "Fact or Fiction"
            Q5: "Fact or Fiction: Bruce Molay has a black belt in karate." ff

            Q11: "Fact or Fiction: Sam Guyer is is a licensed airplane pilot." ff
|]






