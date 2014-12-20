##Pollstr

Pollstr is a domain-specific language embedded in Haskell for specifying and 
generating surveys.   


After running `cabal install`, run the tests by loading Examples/First.hs in
ghci and invoke the `testLatex` and `testJSON` functions. `latexmk` is required to build the pdf files from tex.

---

Things that have been updated:

1. Variable binding/environment management improvements: Variable mapping for
  responses and questions have been moved into the parser. It is now an error
  at parse time to reference an unbound variable or to use a duplicate name for
  both a question and a response. Variables of the same type can still be 
  rebound, however.   

1. More response types: In addition to single-selection, I've added a 
multiple-selection and a free response type. For multiple selection, the 
respondent may check all options that apply. For free response, space is 
provided for the respondent to write-in his response.   

1. Addition skip options: For single-selection response types, users may now
specify any number of (non-overlapping) skips. This is still rudimentary, and
it would be nice to clean up the concrete syntax for this. Additionally, there
is no check for overlapping options.  

1. Tests have been added for the new features and grammar has been updated.