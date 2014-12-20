{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Multi where

import Language.Syntax
import Language.Quote

{- 

-}

[pollstr| 
    Survey Multi: 
        Title: "An example of a survey with multiple selection"
        Q1: "What movies have you seen?" 
            Multi ["The Mighty Ducks", "Fight Club", "Love Actually", "Finding Nemo"]
|]
