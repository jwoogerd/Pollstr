{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Flow where

import Language.Syntax
import Language.Quote

{- 
    This example shows what a simple flow control construct looks like -- 
    skipping to an item or section based on the response. Right now, you 
    pass to the function 'skipTo' the identifier of the item or section to 
    go to and the responses for which the skip should be executed. This is
    still a work in progress. 
-}

[pollstr|
    Response howFrequent = ["Never", "Sometimes", "Often", "Always"]

    Survey Flow: "A short survey about hygiene"
        Section Hygiene: "Hygiene"

            Qteeth: "How often do you brush your teeth?" howFrequent
                    skipTo(Qbrand, ["Often", "Always"])

            Qhair: "How often do you brush your hair?" howFrequent

            Qbrand: "What brand of toothpaste do you use?"
                    ["Colgate", "Crest", "Other"]
|]

flowText = unlines [
    "Response howFrequent = [\"Never\", \"Sometimes\", \"Often\", \"Always\"]",
    "",
    "Survey Flow: \"A short survey about hygiene\"",
    "   Section Hygiene: \"Hygiene\"",
    "",
    "       Qteeth: \"How often do you brush your teeth?\" howFrequent",
    "           skipTo(Qbrand, [\"Often\", \"Always\"])",
    "",
    "       Qhair: \"How often do you brush your hair?\" howFrequent",
    "",
    "       Qbrand: \"What brand of toothpaste do you use?\"",
    "               [\"Colgate\", \"Crest\", \"Other\"]"
    ]

topLevelDecls = [RespDecl "howFrequent" (Response ["Never","Sometimes","Often","Always"])]

hygieneItems = [
    Item "teeth" (Question "How often do you brush your teeth?")
         (Rvar "howFrequent") (Skip "brand" (Response ["Often", "Always"])),
    Item "hair" (Question "How often do you brush your hair?")
         (Rvar "howFrequent") None,
    Item "brand" (Question "What brand of toothpaste do you use?")
        (Response ["Colgate","Crest","Other"]) None]

flowAST = Survey "Flow" "A short survey about hygiene" 
         topLevelDecls [Section "Hygiene" "Hygiene" hygieneItems]

