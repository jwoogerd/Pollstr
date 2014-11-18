{-# LANGUAGE OverloadedStrings #-}

module Language.Latex_test where

import Text.LaTeX

import Language.Parser
import Language.Syntax

preamble :: Monad m => String -> String -> LaTeXT_ m
preamble _title _author = 
    documentclass [] article
    <> title (fromString _title)
    <> author (fromString _author)

body :: Monad m => Question -> LaTeXT_ m
body (Question q) = document (maketitle <> (fromString q))

surveyL :: Monad m => String -> String -> LaTeXT_ m
surveyL title author = preamble (fromString title) (fromString author) <> body (Question "Does this work?")

main :: IO ()
main = execLaTeXT (surveyL "Title" "Author") >>= renderFile "survey.tex"
