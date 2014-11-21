{-# LANGUAGE OverloadedStrings #-}

module Language.PrintLatex (printLatex) where

import Text.LaTeX hiding (title, item, section)
import qualified Text.LaTeX as LT (title, item, section)
import Data.List(intersperse)

import Language.Syntax

printLatex :: Survey -> String -> IO ()
printLatex s dest = execLaTeXT (surveyL s) >>= renderFile dest

preamble :: Monad m => LaTeXT_ m
preamble = documentclass [] article

surveyL :: Monad m => Survey -> LaTeXT_ m
surveyL (Survey id title decls sections) = 
    preamble
    <> LT.title (fromString title)
    <> document (maketitle <> sectionsL decls sections)

sectionsL :: Monad m => [Decl] -> [Section] -> LaTeXT_ m
sectionsL decls [] = fromString ""
sectionsL decls [Section "Bare" "" items] = itemsL decls items
sectionsL decls (section:rest) = sectionL section <> sectionsL decls rest
    where sectionL (Section id title items) =
            LT.section (fromString title) <> itemsL decls items

itemsL :: Monad m => [Decl] -> [Item] -> LaTeXT_ m
itemsL decls items = 
    let mkItem (Item id quest resp skp) = 
            subsection $ textnormal (questionL quest decls
                <> vspace (Mm 2) <> newline  
                <> responseL resp skp decls)
            <> label (fromString id)
    in mconcat $ map mkItem items

questionL :: Monad m => Question -> [Decl] -> LaTeXT_ m
questionL (Question q) _  = fromString q
questionL (Qvar qv) decls = case lookupQ decls of Just q  -> questionL q []
                                                  Nothing -> error "Not found"
    where lookupQ []                    = Nothing
          lookupQ (QuestDecl id q:rest) = if qv == id then Just q else lookupQ rest
          lookupQ (_:rest)              = lookupQ rest

responseL :: Monad m => Response -> Skip -> [Decl] -> LaTeXT_ m
responseL (Response rs) skip decls = let 
    checkbox = fromString "[" <> hspace (Mm 5) 
            <> fromString "]" <> hspace (Mm 4)
    naturals  = iterate (+ 1) 1
    countBy n resp = resp <> hspace (Mm 3) <> (fromString $ "(" ++ show n ++ ")")
    responses =  zipWith countBy naturals $ fmap (skipL skip decls) rs
    in mconcat $ checkbox :(intersperse (hspace (Mm 5) <> newline <> checkbox) responses)
responseL (Rvar rv) skip decls = case lookup decls of Just r  -> responseL r skip []
                                                      Nothing -> error "Not found"
    where lookup []                   = Nothing                                            
          lookup (RespDecl id r:rest) = if rv == id then Just r else lookup rest
          lookup (_:rest)             = lookup rest

skipL :: Monad m => Skip -> [Decl] -> String -> LaTeXT_ m
skipL None _ rs = fromString rs
skipL (Skip id (Response skips)) decls rs =
    fromString rs <>
    if rs `elem` skips 
    then hspace (Mm 3) <> textit (fromString "(skip to question "
         <> ref (fromString id) <> fromString ")")
    else fromString ""
skipL (Skip id (Rvar rv)) decls rs = 
    case lookup decls of 
        Just (Response skips) -> 
            if rs `elem` skips
            then hspace (Mm 3) <> textit (fromString "(skip to question "
                 <> ref (fromString id) <> fromString ")")
            else fromString ""
        Nothing -> error "Not found"
        where lookup []                   = Nothing                                            
              lookup (RespDecl id r:rest) = if rv == id then Just r else lookup rest
              lookup (_:rest)             = lookup rest
