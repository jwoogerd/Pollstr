
module Language.Environment where

import qualified Data.Map.Strict as Map

import Language.Syntax

data Env = Env { rs :: Map.Map ID Response
               , qs :: Map.Map ID Question
               } deriving Show

lookupQ :: Env -> ID -> Question
lookupQ env id = 
    case (Map.lookup id (qs env)) of Just q  -> q
                                     Nothing -> error ("Question not defined " ++ show id)

lookupR :: Env -> ID -> Response
lookupR env id = 
    case (Map.lookup id (rs env)) of Just r  -> r
                                     Nothing -> error ("Response not defined " ++ show id)

makeEnv :: [Decl] -> Env
makeEnv decls = 
    let emptyEnv = Env Map.empty Map.empty
        f (QuestDecl id q) (Env rs qs) = if Map.notMember id rs
                                         then Env rs (Map.insert id q qs)
                                         else error ("Duplicate variable " ++ show id)
        f (RespDecl id r) (Env rs qs)  = if Map.notMember id qs
                                         then Env (Map.insert id r rs) qs
                                         else error ("Duplicate variable " ++ show id)
    in foldr f emptyEnv decls 