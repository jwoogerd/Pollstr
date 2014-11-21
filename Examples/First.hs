{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.First where

import Language.Parser
import Test.HUnit hiding (test)
import Test.HUnit.Diff
import Text.ParserCombinators.Parsec
import Text.Parsec.Error

import System.Process

import Language.Syntax
import Language.Quote
import Examples.SimpleSurvey
import Examples.Sections
import Examples.Flow

{- Pollstr testing suite -}

test :: IO()
test = do
    print "Invoking printSimple and saving output at 'Examples/simple.tex'"
    printSimple "Examples/simple.tex"

    print "Invoking printFlow and saving output at 'Examples/flow.tex'"
    printFlow "Examples/flow.tex"

    print "Invoking printSections and saving output at 'Examples/sections.tex'"
    printSections "Examples/sections.tex"

    print "Generating simple.pdf, flow.pdf, sections.pdf files...and cleaning up"
    createProcess (shell $ "latexmk -pdf Examples/simple.tex &&" ++
                           "latexmk -pdf Examples/flow.tex &&" ++ 
                           "latexmk -pdf Examples/sections.tex && " ++
                           "mv *.pdf Examples/ && rm *.log *.aux *.fls *.fdb_latexmk .temp*")
    print "Reference tex and pdf files are located in Examples/Reference\n"
    return ()

clean :: IO()
clean = do
    print "Removing test-generated files."
    createProcess (shell $ "rm Examples/*.tex Examples/*.pdf")
    return ()
