{-# LANGUAGE OverloadedStrings #-}

module Language.Latex_test where

import Text.LaTeX hiding (item)
import qualified Text.LaTeX as LT (item)

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
surveyL (Survey id decls items sections) = 
    preamble
    <> title (fromString id)
    <> document (itemsL decls items)

itemsL :: Monad m => [Decl] -> [Item] -> LaTeXT_ m
itemsL decls items = 
    let mkItem (Item id quest resp skp) = 
            (questionL quest decls)
            <> responseL resp decls
            <> skipL skp decls
    in mconcat $ map mkItem items

questionL :: Monad m => Question -> [Decl] -> LaTeXT_ m
questionL (Question q) _  = fromString q
questionL (Qvar qv) decls = case lookupQ decls of Just q  -> questionL q []
                                                  Nothing -> error "Not found"
    where lookupQ []                    = Nothing
          lookupQ (QuestDecl id q:rest) = if qv == id then Just q else lookupQ rest
          lookupQ (_:rest)              = lookupQ rest

responseL :: Monad m => Response -> [Decl] -> LaTeXT_ m
responseL (Response rs) _  = mconcat $ fmap fromString rs
responseL (Rvar rv) decls = case lookup decls of Just r  -> responseL r []
                                                 Nothing -> error "Not found"
    where lookup []                   = Nothing                                            
          lookup (RespDecl id r:rest) = if rv == id then Just r else lookup rest
          lookup (_:rest)             = lookup rest

skipL :: Monad m => Skip -> [Decl] -> LaTeXT_ m
skipL None _ = fromString ""
skipL (Skip id r) decls = fromString "Not implemented"

sectionsL :: Monad m => [Section] -> LaTeXT_ m
sectionsL = undefined

main :: IO ()
main = execLaTeXT (surveyL simpleSurveyAST) >>= renderFile "survey.tex"
