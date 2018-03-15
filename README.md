# mu-kanren

Haskell implementation of
[μKanren](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf), a
minimalist language in the miniKanren family of relational (logic) programming
languages.

It is not meant to be an actual runnable program, but rather a core library to
experiment with on the repl.

# Build & use

```
$ cabal new-repl
```

# Examples

Some examples taken from the
[repository](https://github.com/jasonhemann/microKanren) of the original Scheme
implementation.

```
λ> takeS 2 aAndB
([(#1, 5), (#0, 7)], 2)
([(#1, 6), (#0, 7)], 2)

λ> takeS 1 (fives (num 5))
([], 0)

-- limit number of recursive calls
-- to avoid divergence.
λ> takeS' 1 10 (fives (num 3))
λ>

λ> takeS 2 callAppendo
([(#0, (#1 #2 #3)), (#2, #3), (#1, ())], 4)
([(#0, (#1 #2 #3)), (#2, #6), (#5, ()), (#3, (#4 . #6)), (#1, (#4 . #5))], 7)

λ> run 2 callAppendo
(() _.0 _.0)
((_.0) _.1 (_.0 . _.1))

λ> runAll appendoo
(() (1 2 3 4 5))
((1) (2 3 4 5))
((1 2) (3 4 5))
((1 2 3) (4 5))
((1 2 3 4) (5))
((1 2 3 4 5) ())



```