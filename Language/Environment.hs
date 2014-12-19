
module Language.Environment where

import qualified Data.Map.Strict as Map

import Language.Syntax

data Env = Env { rs :: Map.Map ID Response
               , qs :: Map.Map ID Question
               } deriving Show

lookupQ :: Env -> ID -> Question
lookupQ env id = 
    case (Map.lookup id (qs env)) of Just q  -> q
                                     Nothing -> error "Question not defined"

lookupR :: Env -> ID -> Response
lookupR env id = 
    case (Map.lookup id (rs env)) of Just r  -> r
                                     Nothing -> error "Question not defined"

makeEnv :: [Decl] -> Env
makeEnv decls = 
    let emptyEnv = Env Map.empty Map.empty
        f (QuestDecl id q) (Env rs qs) = Env rs (Map.insert id q qs)
        f (RespDecl id r) (Env rs qs)  = Env (Map.insert id r rs) qs
    in foldr f emptyEnv decls 