module Examples.Sections where

import Language.Pollstr_syntax

{-
    This example shows how survey sections will work. There can be any number
    of sections, and sections can be nested. Right now the indentation is 
    not enforced by the parser, but I am considering making the indentation
    mandatory. Section and survey identifiers must start with a capital letter.

    Right now Survey and Section have the exact same abstract syntax and
    constructions. This seems repetitive, but for some reason it seems 
    prudent to keep these two entities distinct.
-}

sectionsText = unlines [ 
    "survey Sections",
    "",
    "   response howFrequent = {\"Never\", \"Sometimes\", \"Often\", \"Always\"}",
    "   question train = \"How often do you ride the T?\"",
    "",
    "   section Hygiene",
    "",
    "       Qteeth: \"How often do you brush your teeth?\" howFrequent",
    "",
    "   end Hygiene",
    "",
    "   section Transportation",
    "",
    "       Qtrain: train howFrequent",
    "",
    "       section Nested",
    "",
    "           question dogcat = \"Do your prefer dogs or cats?\"",
    "",
    "       end Nested",
    "",
    "   end Transportation",
    "",
    "end Sections"]

{- Expected Pollstr abstract syntax -}

topLevelDecls = [
    RespDecl ("howFrequent", Response ["Never","Sometimes","Often","Always"]),
    QuestDecl ("train", Question "How often do you ride the T?")]

sections = [
    Section ("Hygiene", [], [Item ("teeth",
        Question "How often do you brush your teeth?", Rvar "howFrequent", None)], []),
    Section ("Transportation", [], [Item ("train", 
        Qvar "train", Rvar "howFrequent", None)], nested)]

nested = [Section ("Nested", [QuestDecl ("dogcat", 
    Question "Do your prefer dogs or cats?")],[],[])]

sectionsAST = Survey ("Sections", topLevelDecls, [], sections) 
