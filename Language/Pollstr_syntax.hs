module Language.Pollstr_syntax where

type ID         = String

data Var   = Var ID deriving Show

data Survey     = Survey (ID, [Decl], [SurveyItem]) deriving Show

data Question   = Qlit String | Qvar Var deriving Show

data RespTy = S String | N Integer | B Bool deriving Show

data Response   = RLit [RespTy] | Rvar Var deriving Show

data Decl       = QuestDecl (ID, Question) 
                | RespDecl (ID, Response) deriving Show

data SurveyItem = SurveyItem (ID, Question, Response) deriving Show

--declarations = [RespDecl("howFrequent", Response ["Never", "Sometimes", "Often", "Always"])]
--items = [SurveyItem("teeth", Question "How often do you brush your teeth?", Response ["Never", "Sometimes", "Often", "Always"] )]
--test = Survey("mySurvey", declarations, items)