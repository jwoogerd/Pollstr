
import Language.Pollstr_syntax

import Text.Parsec
import Text.ParserCombinators.Parsec

import qualified Text.Parsec.String as PS
import qualified Text.Parsec.Prim as PP
import qualified Text.Parsec.Token as PT

import Text.ParserCombinators.Parsec.Language (haskellStyle, reservedOpNames, reservedNames)
import Control.Monad

parseQ :: String -> Either ParseError Question
parseQ = parse question "(unknown)"

question :: PS.Parser Question 
question = do 
    q <- stringLiteral
    return $ Question q
    <?> "question"

parseR :: String -> Either ParseError Response
parseR = parse response "(unknown)"

response :: PS.Parser Response
response = do 
    rs <- braces $ commaSep stringLiteral
    return $ Response rs
    <?> "response"

parseS :: String -> Either ParseError SurveyItem 
parseS = parse surveyItem "(unknown)"

surveyItem :: PS.Parser SurveyItem
surveyItem = do 
    reserved "question"
    whiteSpace
    id <- identifier
    whiteSpace
    quest <- question
    whiteSpace
    resp <- response
    return $ SurveyItem (id, quest, resp)
    <?> "survey item"

parseRS:: String -> Either ParseError Decl
parseRS = parse respDecl "(unknown)"

respDecl :: PS.Parser Decl
respDecl = do
    reserved "response"
    whiteSpace
    id <- identifier
    whiteSpace
    reserved "="
    whiteSpace
    resp <- response
    return $ RespDecl(id, resp)
    <?> "response declaration"


parseQS:: String -> Either ParseError Decl
parseQS = parse questDecl "(unknown)"

questDecl :: PS.Parser Decl
questDecl = do
    reserved "question"
    whiteSpace
    id <- identifier
    whiteSpace
    reserved "="
    whiteSpace
    quest <- question 
    return $ QuestDecl(id, quest)
    <?> "question declaration"

decl :: PS.Parser Decl
decl = questDecl <|> respDecl

parseSurvey :: String -> Either ParseError Survey
parseSurvey = parse survey "(unknown)"

survey = do
    reserved "survey"
    whiteSpace
    id <- identifier
    whiteSpace
    decls <- many decl
    items <- many surveyItem
    reserved "end"
    return $ Survey (id, decls, items) 
    <?> "survey"


{- Borrowed from PADS parser -}
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle 
  { reservedOpNames = ["{", "}", ","],
    reservedNames   = ["question", "response", "survey", "end"]})

whiteSpace    = PT.whiteSpace    lexer
identifier    = PT.identifier    lexer
reserved      = PT.reserved      lexer
colon         = PT.colon         lexer
stringLiteral = PT.stringLiteral lexer
reservedOp    = PT.reservedOp    lexer
commaSep      = PT.commaSep      lexer
braces        = PT.braces        lexer

brackets      = PT.brackets    lexer
operator      = PT.operator    lexer
charLiteral   = PT.charLiteral lexer
integer       = PT.integer     lexer
parens        = PT.parens      lexer

