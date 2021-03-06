ApaTypeAnalysis
===============

Students:
Frank Dedden (3705269)
Wilco Kusee (3800296)

Assignment for the master course `Automatic Program Analysis`.

Project structure
-----------------

- analysis: contains the analysis itself:
  - examples: contains runnable examples
  - src: contains the code itself
    - Expr.hs: contains the data types used to parse and analyse input
    - Lexer.hs: defines the lexer
    - Parser.hs: uses the result of the lexer, and parses haskell expressions
    - Typechecker.hs: typechecking
    - Main.hs: glues everything together, and contains the unification algorithm
- reports: contains additional documentation:
  - plan: contains the original proposal of the analysis
  - rules: contains the type rules
  - report: contains the final report

Building/running
----------------

    $ cd analysis
    $ cabal configure
    $ cabal build
    $ ./run examples/Example1.hs

run is a symlink. If it does not work, you have to call:

    $ dist/build/ApaTypeAnalysis/ApaTypeAnalysis examples/Example1.hs
