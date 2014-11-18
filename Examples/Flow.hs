module Examples.Flow where

import Language.Syntax

{- 
    This example shows what a simple flow control construct looks like -- 
    skipping to an item or section based on the response. Right now, you 
    pass to the function 'skipTo' the identifier of the item or section to 
    go to and the responses for which the skip should be executed. This is
    still a work in progress. 
-}

flowText = unlines [
    "survey Flow",
    "",
    "   response howFrequent = {\"Never\", \"Sometimes\", \"Often\", \"Always\"}",
    "",
    "   section Hygiene",
    "",
    "       Qteeth: \"How often do you brush your teeth?\" howFrequent",
    "           skipTo(Qbrand, {\"Often\", \"Always\"})",
    "",
    "       Qhair: \"How often do you brush your hair?\" howFrequent",
    "",
    "       Qbrand: \"What brand of toothpaste do you use?\"",
    "               {\"Colgate\", \"Crest\", \"Other\"}",
    "",
    "   end Hygiene",
    "end Flow"]


topLevelDecls = 
    [RespDecl "howFrequent" (Response ["Never","Sometimes","Often","Always"])]

hygieneItems = [
    Item "teeth" (Question "How often do you brush your teeth?")
         (Rvar "howFrequent") (Skip "brand" (Response ["Often", "Always"])),
    Item "hair" (Question "How often do you brush your hair?")
         (Rvar "howFrequent") None,
    Item "brand" (Question "What brand of toothpaste do you use?")
        (Response ["Colgate","Crest","Other"]) None]

flowAST = Survey "Flow" topLevelDecls [] [Section "Hygiene" [] hygieneItems []]

