module Language.Pollstr_syntax where

type ID         = String

data Survey     = Survey (ID, [Decl], [SurveyItem]) deriving Show

data Question   = Question String deriving Show

data Response   = Response [String] deriving Show

data Decl       = QuestDelc (ID, Question) 
                | RespDecl (ID, Response) deriving Show

data SurveyItem = SurveyItem (ID, Question, Response) deriving Show

declarations = [RespDecl("howFrequent", Response ["Never", "Sometimes", "Often", "Always"])]
items = [SurveyItem("teeth", Question "How often do you brush your teeth?", Response ["Never", "Sometimes", "Often", "Always"] )]
test = Survey("mySurvey", declarations, items)