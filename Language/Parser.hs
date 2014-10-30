
module Language.Parser where

import Language.Pollstr_syntax

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.String as PS
import qualified Text.Parsec.Token as PT
import Data.Monoid(mconcat)

import Text.ParserCombinators.Parsec.Language (haskellStyle, reservedOpNames, reservedNames)

question :: PS.Parser Question 
question = qlit <|> qvar

qvar :: PS.Parser Question 
qvar = do 
    id <- identifier 
    return $ Qvar id
    <?> "question variable"

qlit :: PS.Parser Question 
qlit = do 
    q <- stringLiteral
    return $ Question q
    <?> "literal question (string)"

parseR :: String -> Either ParseError Response
parseR = parse response "(unknown)"

response :: PS.Parser Response 
response = rlit <|> rvar

rvar :: PS.Parser Response
rvar = do 
    id <- identifier 
    return $ Rvar id
    <?> "response variable"

rlit :: PS.Parser Response
rlit = do 
    rs <- braces $ commaSep stringLiteral
    return $ Response rs
    <?> "literal response"

parseI :: String -> Either ParseError Item 
parseI = parse item "(unknown)"

itemID :: PS.Parser ID
itemID = do
    oneOf "Q"
    id <- identifier
    return id
    <?> "item identifier to start with 'Q'"

item :: PS.Parser Item
item = do 
    id <- itemID
    reserved ":"
    whiteSpace
    quest <- question
    whiteSpace
    resp <- response
    return $ Item (id, quest, resp)
    <?> "item statement"

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

-- See http://stackoverflow.com/questions/8758460
decl', item' :: Parser ([Decl], [Item])
decl' = fmap (\x -> ([x], [])) decl
item' = fmap (\x -> ([], [x])) item

statements :: PS.Parser ([Decl], [Item])
statements = fmap mconcat . many . choice $ [decl', item']

parseSurvey :: String -> Either ParseError Survey
parseSurvey = parse survey "(unknown)"

survey = do
    reserved "survey"
    whiteSpace
    idStart <- identifier
    whiteSpace
    (decls, items) <- statements
    reserved "end"
    whiteSpace
    idEnd <- identifier
    return $ Survey (idStart, decls, items, []) 
    <?> "survey"


{- lexer borrowed from PADS parser -}

lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle 
  { reservedOpNames = ["{", "}", ",", ":"],
    reservedNames   = ["question", "response", "survey", "section", "end"]})

whiteSpace    = PT.whiteSpace    lexer
identifier    = PT.identifier    lexer
reserved      = PT.reserved      lexer
colon         = PT.colon         lexer
stringLiteral = PT.stringLiteral lexer
reservedOp    = PT.reservedOp    lexer
commaSep      = PT.commaSep      lexer
braces        = PT.braces        lexer
