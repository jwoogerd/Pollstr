{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Sections where

import Language.Syntax
import Language.Quote

{-
    This example shows multiple sections and declarations, as well as skips. 
    This program generates three declarations: printSections, toJSONSections,
    and toJSONSections'.
-}

[pollstr|
   Response howFrequent = ["Never", "Sometimes", "Often", "Always"]
   Question train = "How often do you ride the T?"
   Question dogcat = "Do your prefer dogs or cats?"

    Survey Sections:
      Title: "Example Survey with Sections"
      Author: "Jayme Woogerd"
      Description: "This is an example of sections"

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
