{-# LANGUAGE OverloadedStrings #-}

module Language.PrintLatex (printLatex) where

import Text.LaTeX hiding (title, item, section)
import qualified Text.LaTeX as LT (title, author, institute, paragraph, item, section)
import Data.List(intersperse)

import Language.Syntax

{-
    This module exports one function, printLatex that, given a Survey and a
    path, produces a LaTeX file to render the survey in print.
-}

printLatex :: Survey -> FilePath -> IO ()
printLatex s dest = execLaTeXT (surveyL s) >>= renderFile dest

preamble :: Monad m => LaTeXT_ m
preamble = documentclass [] article

surveyL :: Monad m => Survey -> LaTeXT_ m
surveyL (Survey id (Meta title author desc) sections) = 
    let mkString s = case s of (Just s) -> fromString s
                               Nothing  -> fromString ""
    in preamble 
       <> LT.title (mkString title) 
       <> LT.author (mkString author)
       <> document (maketitle <> LT.paragraph (mkString desc) <> sectionsL sections)

sectionsL :: Monad m => [Section] -> LaTeXT_ m
sectionsL [] = fromString ""
sectionsL [Section "Bare" "" items] = itemsL items
sectionsL (section:rest) = sectionL section <> sectionsL rest
    where sectionL (Section id title items) =
            LT.section (fromString title) <> itemsL items

itemsL :: Monad m => [Item] -> LaTeXT_ m
itemsL items = 
    let mkItem (Item id quest resp skips) = 
            subsection $ textnormal (questionL quest
                <> vspace (Mm 2) <> newline  
                <> responseL resp skips)
            <> label (fromString id)
    in mconcat $ map mkItem items

questionL :: Monad m => Question -> LaTeXT_ m
questionL (Question q)  = fromString q

responseL :: Monad m => Response -> [Skip] -> LaTeXT_ m
responseL (Single rs) skips = 
    let checkbox = fromString "[" <> hspace (Mm 5) <> fromString "]" <> hspace (Mm 4)
        naturals  = iterate (+ 1) 1
        countBy n resp = resp <> hspace (Mm 3) <> (fromString $ "(" ++ show n ++ ")")
        responses =  zipWith countBy naturals $ fmap (skipL skips) rs
    in (mconcat $ checkbox :(intersperse (hspace (Mm 5) <> newline <> checkbox) responses))
responseL (Multi rs) _ = 
    let checkbox = fromString "[" <> hspace (Mm 5) <> fromString "]" <> hspace (Mm 4)
        responses =  fmap fromString rs
    in vspace (Mm 2) <> (textit $ fromString "(Check all that apply)") <> newline 
       <> (textnormal $ mconcat $ checkbox :(intersperse (hspace (Mm 5) <> newline <> checkbox) responses))
responseL (Free lines) _ = newline 
                         <> (mconcat $ intersperse (vspace (Mm 3.5) <> newline) 
                             $ take lines $ repeat (underline $ hspace (In 4)))

skipL :: Monad m => [Skip] -> String -> LaTeXT_ m
skipL skips resp = 
    let mkSkip (Skip id opts) rs =
            rs <>
            if resp `elem` opts
            then hspace (Mm 3) <> textit (fromString "(skip to question "
                 <> ref (fromString id) <> fromString ")")
            else fromString ""
    in foldr mkSkip (fromString resp) skips 
