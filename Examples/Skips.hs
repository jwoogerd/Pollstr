{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Skips where

import Language.Syntax
import Language.Quote

{- 
    This is an example of multiple skip options for one single-choice
    response.
-}

[pollstr| 
    Survey Skips: 
        Title: "An example of a survey with more skip logic"

        Q1: "Who is your favorite president?"
            ["Abraham Lincoln", "George Washington", "FDR"] 
            skipTo(Q3, ["Abraham Lincoln"])
            skipTo(Q4, ["FDR"])

        Q2: "What movies have you seen?" 
            Multi ["The Mighty Ducks", "Fight Club", "Love Actually", "Finding Nemo"]

        Q3: "Tell a story about a time you got lost." Free 10

        Q4: "Another question...." ["Why?", "How?", "When?"]
|]
