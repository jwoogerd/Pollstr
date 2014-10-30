module Language.Pollstr_syntax where

type ID         = String

data Survey     = Survey (ID, [Statement], [Section]) deriving Show

data Section    = Section (ID, [Statement], [Section]) deriving Show

data Statement  = DeclS Decl | ItemS Item deriving Show

data Decl       = QuestDecl (ID, Question) 
                | RespDecl (ID, Response) deriving Show

data Item       = Item (ID, Question, Response) deriving Show

data Question   = Question String | Qvar ID deriving Show

data Response   = Response [String] | Rvar ID deriving Show

