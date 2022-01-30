# wordle-solver-haskell

A Wordle solver written in Haskell. This tool suggests words (eg `boxer`) and then you provide the colors for that word (eg `02022`).

No heuristics are used. It only suggests the first word that complies with previous seen colors and guesses.

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
22001 = 2 greens, 2 blacks, 1 yellow

Try this word: nurls
Enter colors here: 00100
Try this word: frati
Enter colors here: 01000
Try this word: reeve
Enter colors here: 11000
Try this word: hyper
Enter colors here: 00022
Try this word: cower
Enter colors here: 02022
Try this word: joker
Enter colors here: 02022
Try this word: moder
Enter colors here: 02022
Try this word: boxer
Enter colors here: 22222
Found!
Bye
```

## Run the tests

```
make test
```
