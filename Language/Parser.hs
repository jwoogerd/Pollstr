module Language.Parser where

import Language.Syntax
import Language.Environment

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

question :: Env -> PS.Parser Question 
question env = qlit <|> qvar env

qvar :: Env -> PS.Parser Question 
qvar env = do 
    id <- identifier 
    return $ lookupQ env id
    <?> "question variable"

qlit :: PS.Parser Question 
qlit = do 
    q <- multiline
    return $ Question q
    <?> "literal question (string)"

response :: Env -> PS.Parser Response 
response env = single <|> multi <|> free <|> rvar env

rvar :: Env -> PS.Parser Response
rvar env = do 
    id <- identifier 
    return $ lookupR env id
    <?> "response variable"

single :: PS.Parser Response
single = do 
    rs <- brackets $ commaSep stringLiteral
    return $ Single rs
    <?> "single response"

multi :: PS.Parser Response
multi = do 
    reserved "Multi"
    whiteSpace
    rs <- brackets $ commaSep stringLiteral
    return $ Multi rs
    <?> "multi response"

free :: PS.Parser Response
free = do
    reserved "Free"
    whiteSpace
    lines <- natural
    return $ Free (fromIntegral lines)
    <?> "free response"

itemID :: PS.Parser ID
itemID = do
    oneOf "Q"
    id <- many alphaNum
    return ("Q" ++ id)
    <?> "item identifier to start with 'Q'"

skipTo :: Env -> PS.Parser Skip
skipTo env = do
    reserved "skipTo"
    (id, resp) <- parens (do 
        id <- itemID
        reserved ","
        resp <- (single <|> rvar env)
        return (id, resp)
        )
    return $ case resp of (Single opts) -> Skip id opts 
                          _             -> error "Cannot make skip from response type"
    <?> "skipTo application"

item :: Env -> PS.Parser Item
item env = do 
    id <- itemID
    reserved ":"
    whiteSpace
    quest <- (question env)
    whiteSpace
    resp <- (response env)
    skips <- many (skipTo env)
    return $ Item id quest resp skips
    <?> "item statement"

respDecl :: PS.Parser Decl
respDecl = do
    reserved "Response"
    whiteSpace
    id <- identifier
    whiteSpace
    reserved "="
    whiteSpace
    resp <- single 
    return $ RespDecl id resp 
    <?> "response declaration"

questDecl :: PS.Parser Decl
questDecl = do
    reserved "Question"
    whiteSpace
    id <- identifier
    whiteSpace
    reserved "="
    whiteSpace
    quest <- qlit
    return $ QuestDecl id quest
    <?> "question declaration"

decl :: PS.Parser Decl
decl = questDecl <|> respDecl

decls :: PS.Parser [Decl]
decls = many decl

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

titleP :: PS.Parser String
titleP = do
    reserved "Title"
    whiteSpace
    reserved ":" 
    title <- stringLiteral 
    return title

authorP :: PS.Parser String
authorP = do
    reserved "Author"
    whiteSpace
    reserved ":" 
    author <- stringLiteral 
    return author

multiline' :: PS.Parser String
multiline' = do
    string <- stringLiteral
    optional $ reserved "++"
    return string 

multiline :: PS.Parser String
multiline = do
    strings <- many1 multiline'
    return $ mconcat strings
    
descriptionP :: PS.Parser String
descriptionP = do
    reserved "Description"
    whiteSpace
    reserved ":" 
    description <- multiline
    return $ description

meta :: PS.Parser Meta
meta = do
    title <- optionMaybe titleP
    author <- optionMaybe authorP
    description <- optionMaybe descriptionP
    return (Meta title author description)

section :: Env -> PS.Parser Section
section env = do
    reserved "Section"
    whiteSpace
    id <- upperID
    whiteSpace
    reserved ":" 
    whiteSpace
    title <- stringLiteral
    items <- manyTill (item env) (lookAhead $ reserved "Section" <|> eof)
    return $ Section id title items
    <?> "section"

sections :: Env -> PS.Parser [Section]
sections env = do
    items <- many (item env)
    case items of [] -> many (section env)
                  is -> return $ [Section "Bare" "" items]
                  

survey :: PS.Parser Survey
survey = do
    ds <- decls
    let env = makeEnv ds
    reserved "Survey"
    whiteSpace
    id <- upperID 
    whiteSpace
    reserved ":"
    whiteSpace
    metaData <- meta
    sects <- sections env
    return $ Survey id metaData sects
    <?> "survey"

lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser (haskellStyle 
  { reservedOpNames = ["(", ")", ",", ":", "++"],
    reservedNames   = ["Question", "Response", "skipTo", "Survey", "Section",
                       "Title", "Author", "Description", "Single", "Multi",
                       "Free"]})

whiteSpace    = PT.whiteSpace    lexer
identifier    = PT.identifier    lexer
reserved      = PT.reserved      lexer
colon         = PT.colon         lexer
stringLiteral = PT.stringLiteral lexer
natural       = PT.natural       lexer
reservedOp    = PT.reservedOp    lexer
commaSep      = PT.commaSep      lexer
parens        = PT.parens        lexer
brackets      = PT.brackets      lexer
