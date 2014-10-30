module Examples.Third where

import Language.Pollstr_syntax


third = "survey Third \
\\
    \ response howFrequent = {\"Never\", \"Sometimes\", \"Often\", \"Always\"} \
\\
    \ section hygiene \
\\
        \ Qteeth: \"How often do you brush your teeth?\" howFrequent \
        \ skipTo(Qbrand, {\"Often\", \"Always\"}) \
\\
        \ Qhair: \"How often do you brush your hair?\" howFrequent \
\\
        \ Qbrand: \"What brand of toothpaste do you use?\" {\"Colgage\", \"Crest\", \"Other\"} \
\\
    \ end hygiene \
\\
\ end Third"