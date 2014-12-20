module Language.ToJSON (surveyToJSON, surveyToJSON') where

import Prelude hiding (writeFile)
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy (writeFile)
import Data.ByteString.Lazy hiding (map)

import Language.Syntax

{- 
    This module exports two functions for generating JSON structures from
    Pollstr AST: surveyToJSON pretty-prints the JSON string to a given filepath
    and surveyToJSON' returns the raw ByteString for testing purposes.
-}

-- Pretty-print a survey at the given filepath
surveyToJSON :: Survey -> FilePath -> IO()
surveyToJSON s dest = writeFile dest $ encodePretty s

-- Return a JSON representation of a survey as a ByteString
surveyToJSON' :: Survey -> Data.ByteString.Lazy.ByteString 
surveyToJSON' = encode
