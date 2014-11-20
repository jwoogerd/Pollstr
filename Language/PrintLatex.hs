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
sectionsL decls sections = enumerate (mconcat $ map sectionL sections)
    where sectionL (Section id title items) =
            LT.item Nothing <> textbf (fromString title) <> itemsL decls items

itemsL :: Monad m => [Decl] -> [Item] -> LaTeXT_ m
itemsL decls items = 
    let mkItem (Item id quest resp skp) = 
            LT.item Nothing <> questionL quest decls
            <> vspace (Mm 2) <> newline  
            <> responseL resp decls
            <> skipL skp decls
    in enumerate (mconcat $ map mkItem items)

questionL :: Monad m => Question -> [Decl] -> LaTeXT_ m
questionL (Question q) _  = fromString q
questionL (Qvar qv) decls = case lookupQ decls of Just q  -> questionL q []
                                                  Nothing -> error "Not found"
    where lookupQ []                    = Nothing
          lookupQ (QuestDecl id q:rest) = if qv == id then Just q else lookupQ rest
          lookupQ (_:rest)              = lookupQ rest

responseL :: Monad m => Response -> [Decl] -> LaTeXT_ m
-- TODO: add numbers at end
responseL (Response rs) _  = let 
    checkbox = fromString "[" <> hspace (Mm 5) 
            <> fromString "]" <> hspace (Mm 4)
    in mconcat $ checkbox :(intersperse (hspace (Mm 5) <> newline <> checkbox) $ fmap fromString rs)
responseL (Rvar rv) decls = case lookup decls of Just r  -> responseL r []
                                                 Nothing -> error "Not found"
    where lookup []                   = Nothing                                            
          lookup (RespDecl id r:rest) = if rv == id then Just r else lookup rest
          lookup (_:rest)             = lookup rest

skipL :: Monad m => Skip -> [Decl] -> LaTeXT_ m
skipL None _ = fromString ""
skipL (Skip id r) decls = fromString "Not implemented"
