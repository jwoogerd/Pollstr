{-# LANGUAGE OverloadedStrings #-}

module Language.PrintLatex (printLatex) where

import Text.LaTeX hiding (title, item, section)
import qualified Text.LaTeX as LT (title, item, section)
import qualified Data.Map.Strict as Map
import Data.List(intersperse)

import Language.Syntax
import Language.Environment

{-
    This module exports one function, printLatex that, given a Survey and a
    path, produces a LaTeX file to render the survey in print. Right now 
    variable lookup is horrendous -- I plan to implement a variable environment,
    which will also allow for multiple declared Surveys to share bound 
    variables (as of now, you can only define one Survey at a time).

-}
printLatex :: Survey -> String -> IO ()
printLatex s dest = execLaTeXT (surveyL s) >>= renderFile dest

preamble :: Monad m => LaTeXT_ m
preamble = documentclass [] article

surveyL :: Monad m => Survey -> LaTeXT_ m
surveyL (Survey id title decls sections) = 
    let env = makeEnv decls
    in preamble <> LT.title (fromString title)
       <> document (maketitle <> sectionsL env sections)

sectionsL :: Monad m => Env -> [Section] -> LaTeXT_ m
sectionsL env [] = fromString ""
sectionsL env [Section "Bare" "" items] = itemsL env items
sectionsL env (section:rest) = sectionL section <> sectionsL env rest
    where sectionL (Section id title items) =
            LT.section (fromString title) <> itemsL env items

itemsL :: Monad m => Env -> [Item] -> LaTeXT_ m
itemsL env items = 
    let mkItem (Item id quest resp skp) = 
            subsection $ textnormal (questionL quest env 
                <> vspace (Mm 2) <> newline  
                <> responseL resp skp env)
            <> label (fromString id)
    in mconcat $ map mkItem items

questionL :: Monad m => Question -> Env -> LaTeXT_ m
questionL (Question q) _  = fromString q
questionL (Qvar qv) env = 
    case (Map.lookup qv (qs env)) of Just q  -> questionL q env
                                     Nothing -> error "Not found"

responseL :: Monad m => Response -> Skip -> Env -> LaTeXT_ m
responseL (Response rs) skip env = let 
    checkbox = fromString "[" <> hspace (Mm 5) 
            <> fromString "]" <> hspace (Mm 4)
    naturals  = iterate (+ 1) 1
    countBy n resp = resp <> hspace (Mm 3) <> (fromString $ "(" ++ show n ++ ")")
    responses =  zipWith countBy naturals $ fmap (skipL skip env) rs
    in mconcat $ checkbox :(intersperse (hspace (Mm 5) <> newline <> checkbox) responses)
responseL (Rvar rv) skip env = case Map.lookup rv (rs env) of Just r  -> responseL r skip env 
                                                              Nothing -> error "Not found"

skipL :: Monad m => Skip -> Env -> String -> LaTeXT_ m
skipL None _ resp = fromString resp
skipL (Skip id (Response skips)) env resp =
    fromString resp <>
    if resp `elem` skips 
    then hspace (Mm 3) <> textit (fromString "(skip to question "
         <> ref (fromString id) <> fromString ")")
    else fromString ""
skipL (Skip id (Rvar rv)) env resp = 
    case Map.lookup rv (rs env) of 
        Just (Response skips) -> 
            fromString resp <>
            if resp `elem` skips
            then hspace (Mm 3) <> textit (fromString "(skip to question "
                 <> ref (fromString id) <> fromString ")")
            else fromString ""
        Just (Rvar r) -> skipL (Skip id (Rvar r)) env resp
