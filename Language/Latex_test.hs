{-# LANGUAGE OverloadedStrings #-}

module Language.Latex_test where

import Text.LaTeX

import Examples.SimpleSurvey

import Language.Parser
import Language.Syntax

--preamble :: Monad m => String -> String -> LaTeXT_ m
--preamble _title _author = 
--    documentclass [] article
--    <> title (fromString _title)
--    <> author (fromString _author)

--body :: Monad m => Question -> LaTeXT_ m
--body (Question q) = document (maketitle <> (fromString q))

preamble :: Monad m => LaTeXT_ m
preamble = documentclass [] article

surveyL :: Monad m => Survey -> LaTeXT_ m
surveyL (Survey (id, decls, items, sections)) = 
    preamble
    <> title (fromString id)
    <> itemsL decls items

itemsL :: Monad m => [Decl] -> [Item] -> LaTeXT_ m
itemsL decls items = 
    map mkItem items
    where mkItem

itemsL :: Monad m => Question -> LaTeXT_ m
questionL Question q = fromString q
--questionL Qvar    qv = case qv `elem` 

sectionsL :: Monad m => [Section] -> LaTeXT_ m
sectionsL = undefined

main :: IO ()
main = execLaTeXT (surveyL simpleSurveyAST) >>= renderFile "survey.tex"
