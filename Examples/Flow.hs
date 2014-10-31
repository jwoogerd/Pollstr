module Examples.Flow where

import Language.Pollstr_syntax

flowText = unlines [
    "survey Flow",
    "",
    "   response howFrequent = {\"Never\", \"Sometimes\", \"Often\", \"Always\"}"
    "",
    "   section hygiene",
    "",
    "       Qteeth: \"How often do you brush your teeth?\" howFrequent",
    "           skipTo(Qbrand, {\"Often\", \"Always\"})",
    "",
    "       Qhair: \"How often do you brush your hair?\" howFrequent"
    "",
    "       Qbrand: \"What brand of toothpaste do you use?\"",
    "               {\"Colgate\", \"Crest\", \"Other\"}"
    "",
    "   end hygiene"
    "end Flow"]