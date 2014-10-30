
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
    "   end transportation"
    "",
    "end Sections"]