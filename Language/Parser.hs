module Language.Parser where

import Language.Syntax

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.String as PS
import qualified Text.Parsec.Prim   as PP
import qualified Text.Parsec.Token  as PT

import Text.ParserCombinators.Parsec.Language (
    haskellStyle, reservedOpNames, reservedNames)
import Text.ParserCombinators.Parsec.Pos (newPos)
import Data.Monoid (mconcat)

parsePollstrSurvey :: (SourceName, Line, Column) -> String -> Either ParseError Survey
parsePollstrSurvey (filename, line, column) input =
    PP.parse s filename input
    where s = do 
            setPosition (newPos filename line column)
            whiteSpace
            s <- survey
            whiteSpace
            eof <|> errorParse
            return s

-- This function consumes input until the eof marker.
-- (Borrowed from PADS implementation)
errorParse = do
    rest <- manyTill anyToken eof
    unexpected rest

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

skipTo :: PS.Parser Skip
skipTo = do
    reserved "skipTo"
    (id, resp) <- parens (do 
        id <- itemID
        reserved ","
        resp <- response
        return (id, resp)
        )
    return $ Skip id resp
    <?> "skipTo application"

item :: PS.Parser Item
item = do 
    id <- itemID
    reserved ":"
    whiteSpace
    quest <- question
    whiteSpace
    resp <- response
    skip <- option None skipTo
    return $ Item id quest resp skip
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
    return $ RespDecl id resp
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
    return $ QuestDecl id quest
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

insides :: PS.Parser ([Decl], [Item], [Section])
insides = do
    (decls, items, sections) <- statements
    reserved "end"
    whiteSpace
    return (decls, items, sections)

upperID :: PS.Parser String
upperID = do 
    first <- upper
    rest <- identifier
    return (first:rest)

lowerID :: PS.Parser String
lowerID = do 
    first <- lower
    rest <- identifier
    return (first:rest)

section :: PS.Parser Section
section = do
    reserved "section"
    whiteSpace
    id <- upperID
    (decls, items, sections) <- insides
    idEnd <- upperID
    return $ Section id decls items sections
    <?> "section"

survey :: PS.Parser Survey
survey = do
    reserved "survey"
    id <- upperID 
    whiteSpace
    (decls, items, sections) <- insides
    idEnd <- upperID 
    return $ Survey id decls items sections
    <?> "survey"

{- lexer borrowed from PADS parser -}

lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle 
  { reservedOpNames = ["{", "}", "(", ")", ",", ":"],
    reservedNames   = ["question", "response", "skipTo", "survey", "section", "end"]})

whiteSpace    = PT.whiteSpace    lexer
identifier    = PT.identifier    lexer
reserved      = PT.reserved      lexer
colon         = PT.colon         lexer
stringLiteral = PT.stringLiteral lexer
reservedOp    = PT.reservedOp    lexer
commaSep      = PT.commaSep      lexer
braces        = PT.braces        lexer
parens        = PT.parens        lexer
