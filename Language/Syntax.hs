{-# LANGUAGE DeriveDataTypeable #-}

module Language.Syntax where

import Data.Data
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

{- Pollstr abstract syntax -}

type ID         = String

data Survey     = Survey ID [Decl] [Item] [Section]
                  deriving (Show, Eq, Typeable, Data)

data Section    = Section ID [Decl] [Item] [Section]
                  deriving (Show, Eq, Typeable, Data)

data Decl       = QuestDecl ID Question
                | RespDecl ID Response deriving (Show, Eq, Typeable, Data)

data Item       = Item ID Question Response Skip
                  deriving (Show, Eq, Typeable, Data)

data Question   = Question String | Qvar ID deriving (Show, Eq, Typeable, Data)

data Response   = Response [String] | Rvar ID
                  deriving (Show, Eq, Typeable, Data)

data Skip       = Skip ID Response | None deriving (Show, Eq, Typeable, Data)


instance Lift Survey where
    lift s = dataToExpQ (\x -> Nothing) s
