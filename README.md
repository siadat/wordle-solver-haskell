# wordle-solver-haskell

A Wordle solver written in Haskell. This tool suggests words (eg `boxer`), you give provide the colors for that word (eg `02022`).

This tool doesn't use any heuristics. It only suggests the first word that complies with the previous seen colors and guesses.

I am learning Haskell and I would highly appreciate your feedback!

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
