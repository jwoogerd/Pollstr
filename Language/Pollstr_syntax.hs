module Language.Pollstr_syntax where

type ID         = String

data Survey     = Survey (ID, [Decl], [Item], [Section]) 
                  deriving (Show, Eq)

data Section    = Section (ID, [Decl], [Item], [Section]) 
                  deriving (Show, Eq)

data Decl       = QuestDecl (ID, Question) 
                | RespDecl (ID, Response) deriving (Show, Eq)

data Item       = Item (ID, Question, Response) deriving (Show, Eq)

data Question   = Question String | Qvar ID deriving (Show, Eq)

data Response   = Response [String] | Rvar ID deriving (Show, Eq)

