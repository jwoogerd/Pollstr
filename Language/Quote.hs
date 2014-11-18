module Language.Quote
    (pollstr)
    where

import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Language.CodeGen (make_survey)
import qualified Language.Parser as P


pollstr :: QuasiQuoter
pollstr = QuasiQuoter quoteSurveyExpr
                      (error "parse pattern")
                      (error "parse type")
                      (error "parse declaration")

quoteSurveyExpr :: String -> TH.Q TH.Exp
quoteSurveyExpr input = do
    loc <- TH.location
    let filename    = TH.loc_filename loc
    let (line, col) = TH.loc_start loc
    case P.parsePollstrSurvey (filename, line, col) input of
        Left err -> unsafePerformIO $ fail $ show err
        Right x  -> make_survey x