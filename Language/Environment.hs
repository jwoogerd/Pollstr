{-# LANGUAGE NamedFieldPuns #-}

module Language.Environment where

import qualified Data.Map as Map

import Language.Syntax


data Env = Env { rs :: Map.Map ID Response
               , qs :: Map.Map ID Question
               } deriving Show

makeEnv :: Survey -> Env
makeEnv (Survey _ _ decls _) = 
    let emptyEnv = Env { rs = Map.empty
                       , qs = Map.empty}
        f (QuestDecl id q) Env{rs, qs} = Env{rs, qs = Map.insert id q qs}
        f (RespDecl id r) Env{rs, qs}  = Env{rs = (Map.insert id r rs), qs}
    in foldr f emptyEnv decls 