{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Free where

import Language.Syntax
import Language.Quote

{- 

-}

[pollstr| 
    Survey Free: 
        Title: "An example of a survey with a free response type"
        Q1: "Explain why a monad is called a monad." Free 5
|]
