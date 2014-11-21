{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Sections where

import Language.Syntax
import Language.Quote

{-
    This example shows multiple sections and declarations, as well as skips. 
    This program generates two declarations: surveySections and printSections.
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

{- Expected Pollstr abstract syntax -}

topLevelDecls = [
    RespDecl "howFrequent" (Response ["Never","Sometimes","Often","Always"]),
    QuestDecl "train" (Question "How often do you ride the T?"),
    QuestDecl "dogcat" (Question "Do your prefer dogs or cats?")]

section1 = Section "Hygiene" "Hygiene" 
    [Item "Qteeth" (Question "How often do you brush your teeth?")
        (Rvar "howFrequent") (Skip "Qschool" (Response ["Never"])),
    Item "Qhair" (Question "How often do you brush your hair?")
    (Rvar "howFrequent") None]

section2 = Section "Transportation" "Transportation" 
    [Item "Qtrain" (Qvar "train") (Rvar "howFrequent") None]

section3 = Section "School" "School"
    [Item "Qschool" (Question "How often do you go to school?") (Rvar "howFrequent") None,
    Item "Qlunch"(Question "What do you eat for lunch?") (Response ["candy","kale","other"]) None]

sectionsAST = Survey "Sections" "This is an example of sections"
    topLevelDecls [section1, section2, section3]
