module Examples.Flow where

import Language.Pollstr_syntax

flowText = unlines [
    "survey Flow",
    "",
    "   response howFrequent = {\"Never\", \"Sometimes\", \"Often\", \"Always\"}",
    "",
    "   section hygiene",
    "",
    "       Qteeth: \"How often do you brush your teeth?\" howFrequent",
    "           skipTo(Qbrand, {\"Often\", \"Always\"})",
    "",
    "       Qhair: \"How often do you brush your hair?\" howFrequent",
    "",
    "       Qbrand: \"What brand of toothpaste do you use?\"",
    "               {\"Colgate\", \"Crest\", \"Other\"}",
    "",
    "   end hygiene",
    "end Flow"]


topLevelDecls = 
    [RespDecl ("howFrequent", Response ["Never","Sometimes","Often","Always"])]

hygieneItems = [
    Item ("teeth", Question "How often do you brush your teeth?", 
         Rvar "howFrequent", Skip ("brand", Response ["Often", "Always"])),
    Item ("hair", Question "How often do you brush your hair?", 
         Rvar "howFrequent",None),
    Item ("brand", Question "What brand of toothpaste do you use?", 
        Response ["Colgate","Crest","Other"], None)]

flowAST = Survey ("Flow", topLevelDecls, [],
                 [Section ("hygiene", [], hygieneItems, [])])