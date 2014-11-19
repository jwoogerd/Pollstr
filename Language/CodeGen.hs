{-# LANGUAGE TemplateHaskell  #-}

module Language.CodeGen where

import Language.Haskell.TH 
import Language.Haskell.TH.Quote

import Language.Syntax as PS
import Language.Parser
import Language.PrintLatex
import Data.Char (toLower)

import Examples.SimpleSurvey

makeSurveyDecs :: Survey -> Q [Dec]
makeSurveyDecs survey@(Survey id _ _ _) = do
    s <- [| survey |]
    let surveyDecl     = genSurveyDecl id s
        --jprintLatexDecl = genPrintLatexDecl id s
    return [surveyDecl]

idToLower :: ID -> String
idToLower (x:xs) = (toLower x):xs

genSurveyDecl :: ID -> Exp -> Dec
genSurveyDecl id exp = ValD (VarP (mkName $ idToLower id)) (NormalB exp) []

--genPrintLatexDecl :: ID -> Exp -> Dec
--genPrintLatexDecl id exp = ValD (VarP (mkName $ idToLower id)) (NormalB exp) []