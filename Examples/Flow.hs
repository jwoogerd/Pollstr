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
    also given string titles, which are printed.
-}

[pollstr|
    Response howFrequent = ["Never", "Sometimes", "Often", "Always"]
    Response redundant = howFrequent

    Survey Flow: 
        Title: "A short survey about hygiene"
        
        Section Hygiene: "Hygiene"

            Qteeth: "How often do you brush your teeth?" howFrequent
                    skipTo(Qbrand, ["Often", "Always"])

            Qhair: "How often do you brush your hair?" redundant 

            Qbrand: "What brand of toothpaste do you use?"
                    ["Colgate", "Crest", "Other"]
|]
