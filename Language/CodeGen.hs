{-# LANGUAGE TemplateHaskell  #-}

module Language.CodeGen where

import Language.Haskell.TH 
import Language.Haskell.TH.Quote

import Language.Pollstr_syntax as PS
import Language.Parser

make_survey :: Survey -> Q Exp
make_survey s = dataToExpQ (\x -> Nothing) s