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
            Q1: "Would you rather become a cockroach for the rest of your life " ++
                "OR only be able to eat cockroaches for the rest of your life?"
                ["Be a cockroach", "Eat cockroaches"]
                skipTo(Q3, ["Be a cockroach"])

            Q2: "Sautéed, fried or dipped in chocolate?" 
                ["Sautéed", "Fried", "Dipped in chocolate"]

            Q3: "Would you rather only be able to speak in Perl or only be " ++
                "able to talk when others are talking about Perl? " 
                ["Speak in Perl", "Talk when others are talking about Perl"]

        Section Second: "Fact or Fiction"
            Q5: "Fact or Fiction: Every US president with a beard has been a Democrat." ff

            Q11: "Fact or Fiction: The world's biggest island is Greenland." ff
|]

