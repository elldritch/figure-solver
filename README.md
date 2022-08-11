# figure-solver

`figure-solver` is a solver for [Figure](https://figure.game/) puzzles.

To use `figure-solver`, create a `.figure` file and pass it to `--puzzle`:

```bash
$ cat ./puzzles/45.figure
GYGPP
PYPWP
YYGYP
WPWYY
PYWGY

$ cabal run figure-solver -- --puzzle ./puzzles/45.figure
# TODO: document example of output.
```
