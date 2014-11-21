{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Flow where

import Language.Syntax
import Language.Quote

{- 
    This example shows what a simple flow control construct looks like -- 
    skipping to an item or section based on the response. This program produces
    the declarations surveyFlow and printFlow. Note that since skipTo references
    item IDs, one may reorder the questions and the numbering and referencing
    will still be correct when rendered in print. Right now there is no guard
    for skipping back to a question you have already seen.

    Also, this program highlights some changes I've made to the grammar. All
    Response/Question variable binding now happens outside the Survey. Also, 
    Sections can no longer be nested -- in practice, most surveys have only one
    level of sectioning. In addition to identifiers, Surveys and Sections are
    also given a string title.
-}

[pollstr|
    Response howFrequent = ["Never", "Sometimes", "Often", "Always"]
    Response redundant = howFrequent

    Survey Flow: "A short survey about hygiene"
        Section Hygiene: "Hygiene"

            Qteeth: "How often do you brush your teeth?" howFrequent
                    skipTo(Qbrand, ["Often", "Always"])

            Qhair: "How often do you brush your hair?" redundant 

            Qbrand: "What brand of toothpaste do you use?"
                    ["Colgate", "Crest", "Other"]
|]

{- Expected Pollstr abstract syntax -}

topLevelDecls = [RespDecl "howFrequent" (Response ["Never","Sometimes","Often","Always"]),
                 RespDecl "redundant" (Rvar "howFrequent")]

hygieneItems = [
    Item "Qteeth" (Question "How often do you brush your teeth?")
         (Rvar "howFrequent") (Skip "Qbrand" (Response ["Often", "Always"])),
    Item "Qhair" (Question "How often do you brush your hair?")
         (Rvar "redundant") None,
    Item "Qbrand" (Question "What brand of toothpaste do you use?")
        (Response ["Colgate","Crest","Other"]) None]

flowAST = Survey "Flow" "A short survey about hygiene" 
         topLevelDecls [Section "Hygiene" "Hygiene" hygieneItems]

