module Language.Quote (pollstr) where

import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Language.CodeGen (makeSurveyDecs)
import qualified Language.Parser as P


pollstr :: QuasiQuoter
pollstr = QuasiQuoter (error "parse expression")
                      (error "parse pattern")
                      (error "parse type")
                      quoteSurveyDecl

quoteSurveyDecl :: String -> TH.Q [TH.Dec]
quoteSurveyDecl input = do
    loc <- TH.location
    let filename    = TH.loc_filename loc
    let (line, col) = TH.loc_start loc
    case P.parsePollstrSurvey (filename, line, col) input of
        Left err -> unsafePerformIO $ fail $ show err
        Right x  -> makeSurveyDecs x