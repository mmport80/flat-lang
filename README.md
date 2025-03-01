# Flat Lang

- no nesting
- only number operations at first
- only compute using a fold (finite ops)
- last column represents output (only one output allowed)
    - first columns are inputs
- function composition: visually chain tables together somehow
- use fat ops throughout
    - visualise somehow.. convert errors to eithers..
- for now don’t include product types or lists
    - try to avoid nesting as long as possible..



- all num operations use fat ops
- for bools, just keep bools (everything is total)
- lists are just columns with multiple rows
    - avoid lists of lists for now (maybe forever)
- Tuples?
- Maps?
actually using combinations


# Commands

Open development environment:

`nix develop`

Run property tests:

`ghcid --command="ghci Main.hs" --test=":run runParserTests"`

Interactively parse:

```
ghci Parse.hs

ghci> parseProgram "x = 1 + 2"
Right [NamedValue "x" (BinOp Add (Lit (1.0 :+ 0.0)) (Lit (2.0 :+ 0.0)))]
```

Parse a file:

`ghcid --command="ghci Main.hs" --test=":run main examples.flat"`

Run ops tests:

`ghcid --command="ghci Ops.hs" --test=":run test"`

Run operations:

```
ghci Ops.hs

ghci> show $ from $ pythC (to 1) (to 1)
"Right 1.4142135623730951"
```