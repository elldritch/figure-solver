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

$ cabal run figure-solver -- --puzzle ./puzzles/45.figure --moves 8
Solving puzzle:
+-----+
|GYGPP|
|PYPWP|
|YYGYP|
|WPWYY|
|PYWGY|
+-----+

Solution:
0: P
+-----+
| YGPP|
|GYPWP|
|PYGYP|
|YPWYY|
|WYWGY|
+-----+
 ^

0: W
+-----+
| YGPP|
| YPWP|
|GYGYP|
|PPWYY|
|YYWGY|
+-----+
 ^

4: Y
+-----+
| YG  |
| YP  |
|GYGPP|
|PPWWP|
|YYWGP|
+-----+
     ^

2: W
+-----+
| Y   |
| Y   |
|GYG P|
|PPPPP|
|YYGGP|
+-----+
   ^

4: P
+-----+
|     |
| Y   |
| Y   |
|GYG  |
|YYGG |
+-----+
     ^

0: Y
+-----+
|     |
|     |
|     |
|  G  |
|G GG |
+-----+
 ^

0: G
+-----+
|     |
|     |
|     |
|  G  |
|  GG |
+-----+
 ^

2: G
+-----+
|     |
|     |
|     |
|     |
|     |
+-----+
   ^
```
