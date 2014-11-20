{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Sections where

import Language.Syntax
import Language.Quote

{-
    This example shows how survey sections will work. There can be any number
    of sections, and sections can be nested. Right now the indentation is 
    not enforced by the parser, but I am considering making the indentation
    mandatory. Section and survey identifiers must start with a capital letter.

    Right now Survey and Section have the exact same abstract syntax and
    constructions. This seems repetitive, but for some reason it seems 
    prudent to keep these two entities distinct.
-}

[pollstr|
   Response howFrequent = ["Never", "Sometimes", "Often", "Always"]
   Question train = "How often do you ride the T?"
   Question dogcat = "Do your prefer dogs or cats?"

    Survey Sections: "This is an example of sections"

       Section Hygiene: "Hygiene"
           Qteeth: "How often do you brush your teeth?" howFrequent
                   skipTo(Qschool, ["Never"])
           Qhair: "How often do you brush your hair?" howFrequent

       Section Transportation: "Transportation"
           Qtrain: train howFrequent
       
       Section School: "School"
           Qschool: "How often do you go to school?" howFrequent
           Qlunch: "What do you eat for lunch?" ["candy", "kale", "other"]
 |]

sectionsText = unlines [ 
    "Response howFrequent = [\"Never\", \"Sometimes\", \"Often\", \"Always\"]",
    "Question train = \"How often do you ride the T?\"",
    "Question dogcat = \"Do your prefer dogs or cats?\"",
    "",
    "Survey Sections: \"This is an example of sections\"",
    "   Section Hygiene: \"Hygiene\"",
    "       Qteeth: \"How often do you brush your teeth?\" howFrequent",
    "           skipTo(Qschool, [\"Never\"])",
    "       Qhair: \"How often do you brush your hair?\" howFrequent",
    "",
    "   Section Transportation: \"Transportation\"",
    "       Qtrain: train howFrequent",
    "",
    "   Section School: \"School\"",
    "       Qschool: \"How often do you go to school?\" howFrequent",
    "       Qlunch: \"What do you eat for lunch?\" [\"candy\", \"kale\", \"other\"]"
    ]

{- Expected Pollstr abstract syntax -}

topLevelDecls = [
    RespDecl "howFrequent" (Response ["Never","Sometimes","Often","Always"]),
    QuestDecl "train" (Question "How often do you ride the T?"),
    QuestDecl "dogcat" (Question "Do your prefer dogs or cats?")]

section1 = Section "Hygiene" "Hygiene" 
    [Item "teeth" (Question "How often do you brush your teeth?")
        (Rvar "howFrequent") (Skip "school" (Response ["Never"])),
    Item "hair" (Question "How often do you brush your hair?")
    (Rvar "howFrequent") None]

section2 = Section "Transportation" "Transportation" 
    [Item "train" (Qvar "train") (Rvar "howFrequent") None]

section3 = Section "School" "School"
    [Item "school" (Question "How often do you go to school?") (Rvar "howFrequent") None,
    Item "lunch"(Question "What do you eat for lunch?") (Response ["candy","kale","other"]) None]

sectionsAST = Survey "Sections" "This is an example of sections"
    topLevelDecls [section1, section2, section3]
