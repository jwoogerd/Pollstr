##Pollstr
---

Pollstr is a domain-specific language embedded in Haskell for specifying and 
generating surveys.

---

After running `cabal install`, run the tests by loading Examples/First.hs in
ghci and invoke the `testLatex` and `testJSON` functions. `latexmk` is required to build the pdf files from tex.

---
To do:

- Allow adding instructions to a survey or to specific sections
- Allow for multiple Surveys sharing same variable environment and fixup
environment 
- Extend response types to include short answer/blank space
- Multiple language support?
