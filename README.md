# figure-solver

`figure-solver` is a solver for [Figure](https://figure.game/) puzzles.

To use `figure-solver`, create a `.figure` file and pass it to `--puzzle`:

```bash
$ cabal run figure-solver -- --current
Solving puzzle in 10 moves:
+-----+
|PPWWG|
|PWWGG|
|YGWPP|
|PWGWW|
|PGPGP|
+-----+

Solution:
0: P
+-----+
| PWWG|
| WWGG|
|PGWPP|
|PWGWW|
|YGPGP|
+-----+
 ^

0: Y
+-----+
| PWWG|
| WWGG|
| GWPP|
|PWGWW|
|PGPGP|
+-----+
 ^

3: G
+-----+
| PW G|
| WWWG|
| GWGP|
|PWGPW|
|PGPWP|
+-----+
    ^

4: P
+-----+
| PW  |
| WWWG|
| GWGG|
|PWGPP|
|PGPWW|
+-----+
     ^

3: W
+-----+
| PW  |
| WW  |
| GWWG|
|PWGGG|
|PGPPP|
+-----+
    ^

2: P
+-----+
| P   |
| WW  |
| GW  |
|PWWWG|
|PGGGG|
+-----+
   ^

1: G
+-----+
|     |
| P   |
| WW  |
|PGW  |
|PWWW |
+-----+
  ^

1: W
+-----+
|     |
|     |
|     |
|PP   |
|PG   |
+-----+
  ^

0: P
+-----+
|     |
|     |
|     |
|     |
| G   |
+-----+
 ^

1: G
+-----+
|     |
|     |
|     |
|     |
|     |
+-----+
  ^
```
