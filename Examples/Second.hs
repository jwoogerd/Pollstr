{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Second where

import Language.Parser

import Language.Syntax
import Language.Quote

{- 
    These Pollstr programs should all produce parse-time errors.
-}


{- Test duplicate question and response bindings -}
[pollstr|
    Response a = Single ["Never", "Sometimes", "Often", "Always"]
    Question a = "This should be an error"
    Survey Duplicate:
        Q1: "This is a question?" a
|]

{- Test undefined variable -}
[pollstr|
    Question quest = "What happens with an unbound variable?"
    Survey Unbound:
        Q1: quest undefined_response
|]