
module Language.Parser where

import Language.Pollstr_syntax

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.String as PS
import qualified Text.Parsec.Token as PT
import Data.Monoid(mconcat)

import Text.ParserCombinators.Parsec.Language (
    haskellStyle, reservedOpNames, reservedNames)

{- Pollstr parsing -}

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
decl', item', section' :: Parser ([Decl], [Item], [Section])
decl' = fmap (\x -> ([x], [], [])) decl
item' = fmap (\x -> ([], [x], [])) item
section' = fmap (\x -> ([], [], [x])) section

statements :: PS.Parser ([Decl], [Item], [Section])
statements = fmap mconcat . many . choice $ [decl', item', section']

insides :: PS.Parser (String, [Decl], [Item], [Section])
insides = do
    whiteSpace
    idStart <- identifier
    (decls, items, sections) <- statements
    reserved "end"
    whiteSpace
    idEnd <- identifier
    return (idStart, decls, items, sections)

section :: PS.Parser Section
section = do
    reserved "section"
    (id, decls, items, sections) <- insides
    return $ Section (id, decls, items, sections)
    <?> "section"

survey = do
    reserved "survey"
    (id, decls, items, sections) <- insides
    return $ Survey (id, decls, items, sections) 
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
