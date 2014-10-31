
module Examples.Sections where

import Language.Pollstr_syntax

---

sectionsText = unlines [ 
    "survey Sections",
    "",
    "   response howFrequent = {\"Never\", \"Sometimes\", \"Often\", \"Always\"}",
    "   question train = \"How often do you ride the T?\"",
    "",
    "   section hygiene",
    "",
    "       Qteeth: \"How often do you brush your teeth?\" howFrequent",
    "",
    "   end hygiene",
    "",
    "   section transportation",
    "",
    "       Qtrain: train howFrequent",
    "",
    "       section nested",
    "",
    "           question dogcat = \"Do your prefer dogs or cats?\"",
    "",
    "       end nested",
    "",
    "   end transportation",
    "",
    "end Sections"]

{- Expected Pollstr abstract syntax -}

topLevelDecls = [
    RespDecl ("howFrequent", Response ["Never","Sometimes","Often","Always"]),
    QuestDecl ("train", Question "How often do you ride the T?")]

sections = [
    Section ("hygiene", [], [Item ("teeth",
        Question "How often do you brush your teeth?", Rvar "howFrequent", None)], []),
    Section ("transportation", [], [Item ("train", 
        Qvar "train", Rvar "howFrequent", None)], nested)]

nested = [Section ("nested", [QuestDecl ("dogcat", 
    Question "Do your prefer dogs or cats?")],[],[])]

sectionsAST = Survey ("Sections", topLevelDecls, [], sections) 
