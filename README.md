# The Train Game in Haskell!

## What's the train game?

The train game is a game you play with the id number of a train/bus. For example:

![Train Carriage Number](https://goughlui.com/wp-content/uploads/2013/09/20130904_075855.jpg)

(Credit to [this website](https://goughlui.com/2013/09/08/weekend-soup-pot-post-politics-rail-observations-etc/) for the photo!)

Using those 4 numbers and the operators `+ - *` and `/` to create the number `10`.

For example, using `1234`:

`1 + 2 + 3 + 4 = 10`

`(2 * 3) + (4 * 1) = 6 + 4 = 10`

And there may be many more solutions.

## What does this project do?

This project finds all the solutions to a particular train game problem :)

The assumptions it makes are:
* Only clean division occurs (i.e. if we have `a/b`, then this division is accepted iff `a mod b = 0`)
* No division by zero
* There are only 4 numbers as input (unless I make this better...)

## How do I run it?

Make sure you have stack at least version `1.9`. Older versions may work too!

To build the project:
```
stack build
```
To run the project:
```
stack exec train-game-exe -- *Intert 4 numbers here*
```