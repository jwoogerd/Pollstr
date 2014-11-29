module Language.ToJSON (surveyToJSON) where

import Prelude hiding (writeFile)
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy (writeFile)

import Language.Syntax
import Language.Environment

surveyToJSON :: Survey -> Maybe FilePath -> IO()
surveyToJSON s dest =
    let s' = resolveVars s
        json = encodePretty s'
    in case dest of Just dest -> writeFile dest json
                    Nothing   -> print json

resolveVars :: Survey -> Survey
resolveVars (Survey id meta decls sections) =
    let env = makeEnv decls
        resolveResponse (Rvar id) = 
            case Map.lookup id (rs env) of Just r  -> resolveResponse r 
                                           Nothing -> error "Response not defined"
        resolveResponse response = response
        resolveQuestion (Qvar id) = 
            case Map.lookup id (qs env) of Just q -> resolveQuestion q 
                                           Nothing -> error "Question not defined"
        resolveQuestion question = question
        resolveSkip (Skip id resp) = Skip id (resolveResponse resp)
        resolveSkip None = None
        resolveItem (Item id q r s) = Item id (resolveQuestion q) (resolveResponse r) (resolveSkip s)
        resolveSection (Section id title items) = Section id title (map resolveItem items)
    in Survey id meta decls (map resolveSection sections)
