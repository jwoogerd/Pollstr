--module Examples.First where
--import Language.Pollstr_syntax

---

"survey mySurvey

    response howFrequent = {"Never", "Sometimes", "Often", "Always"}

    question teeth: "How often do you brush your teeth?" howFrequent

    question train: "How often do you ride the T?" howFrequent

    question ducks: "Have you ever seen the movie 'The Mighty Ducks'?" 
                    {"Yes", "No"}

end mySurvey"