# wordle-solver-haskell

[![Haskell CI](https://github.com/siadat/wordle-solver-haskell/actions/workflows/haskell.yml/badge.svg)](https://github.com/siadat/wordle-solver-haskell/actions/workflows/haskell.yml)

A Wordle solver written in Haskell. This tool suggests words (eg `boxer`) and then you provide the colors for that word (eg `02022`).
No heuristics are used. The tool only suggests the first word that complies with previous seen colors and guesses.

I am learning Haskell and I would highly appreciate your feedback!
The [main logic](https://github.com/siadat/wordle-solver-haskell/blob/main/src/Lib.hs#L131-L142) and its [tests](https://github.com/siadat/wordle-solver-haskell/blob/main/test/Spec.hs#L30-L34) might be interesting.

## Usage

Open a Wordle game (eg [Wordle](https://www.powerlanguage.co.uk/wordle/) or [Absurdle](https://qntm.org/files/wordle/index.html)), then run:
```
make run
```

```
Guide:
0     = black
1     = yellow
2     = green
Example:
22001 = 2 greens, 2 blacks, 1 yellow

Enter colors for 'showy': 00000
Enter colors for 'angle': 00001
Enter colors for 'tepid': 01010
Enter colors for 'fixer': 11021
Enter colors for 'brief': 22222
Found!
Bye
```

## Run the tests

```
make test
```
