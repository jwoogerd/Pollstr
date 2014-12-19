{-# LANGUAGE TemplateHaskell  #-}

module Language.CodeGen where

import Language.Haskell.TH 
import Language.Haskell.TH.Quote

import Language.Syntax as PS
import Language.Parser
import Language.PrintLatex
import Language.ToJSON

makeSurveyDecs :: Survey -> Q [Dec]
makeSurveyDecs survey@(Survey id _ _) = do
    s              <- [| survey |]
  --  surveyDecl     <- genSurveyDecl id s
    printLatexDecl <- genPrintLatexDecl survey
    jsonDecl       <- genJSONDecl survey
    jsonDecl'      <- genJSONDecl' survey
    return [printLatexDecl, jsonDecl, jsonDecl']

--genSurveyDecl :: ID -> Exp -> Q Dec
--genSurveyDecl id exp = do 
--    return $ ValD (VarP (mkName $ "survey" ++ id)) (NormalB exp) []

genPrintLatexDecl :: Survey -> Q Dec
genPrintLatexDecl survey@(Survey id _ _) = do
    body <- [| printLatex survey |]
    return $ ValD (VarP (mkName $ "print" ++ id)) (NormalB body) []

genJSONDecl :: Survey -> Q Dec
genJSONDecl survey@(Survey id _ _) = do
    body <- [| surveyToJSON survey |]
    return $ ValD (VarP (mkName $ "toJSON" ++ id)) (NormalB body) []

genJSONDecl' :: Survey -> Q Dec
genJSONDecl' survey@(Survey id _ _) = do
    body <- [| surveyToJSON' survey |]
    return $ ValD (VarP (mkName $ "toJSON" ++ id ++ "'")) (NormalB body) []
