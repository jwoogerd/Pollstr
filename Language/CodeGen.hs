{-# LANGUAGE TemplateHaskell  #-}

module Language.CodeGen where

import Language.Haskell.TH 
import Language.Haskell.TH.Quote

import Language.Syntax as PS
import Language.Parser

makeSurveyDecs :: Survey -> Q [Dec]
makeSurveyDecs s = do
    return [ValD (LitP (StringL "survey")) (NormalB (LitE (IntegerL 5))) []]