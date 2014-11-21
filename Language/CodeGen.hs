{-# LANGUAGE TemplateHaskell  #-}

module Language.CodeGen where

import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import qualified Data.Map as Map

import Language.Syntax as PS
import Language.Parser
import Language.PrintLatex

makeSurveyDecs :: Survey -> Q [Dec]
makeSurveyDecs survey@(Survey id _ _ _) = do
    s              <- [| survey |]
    printLatexDecl <- genPrintLatexDecl survey
    surveyDecl     <- genSurveyDecl id s
    return [surveyDecl, printLatexDecl]

genSurveyDecl :: ID -> Exp -> Q Dec
genSurveyDecl id exp = do 
    return $ ValD (VarP (mkName $ "survey" ++ id)) (NormalB exp) []

genPrintLatexDecl :: Survey -> Q Dec
genPrintLatexDecl survey@(Survey id _ _ _) = do
    body <- [| printLatex survey |]
    return $ ValD (VarP (mkName $ "print" ++ id)) (NormalB body) []
