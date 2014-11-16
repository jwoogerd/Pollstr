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
pollstr = QuasiQuoter (error "parse expression")
                      (error "parse pattern")
                      (error "parse type")
                      pparse

pparse :: String -> TH.Q [TH.Dec]
pparse input = do
    case P.survey input of
      Left err -> unsafePerformIO $ fail $ show err
      Right x  -> make_survey x