# A makefile as a stack alternative.
# Using stack takes up a lot of memory - Up to 3gb for a project such as this one.
# GHC uses a lot less if invoked standalone!
# However it's only about half (about 1GB).

CC=ghc
SOURCE=app/Main.hs src/TrainGame.hs

all: train-game-exe

train-game-exe: $(SOURCE)
	$(CC) -o train-game-exe $(SOURCE)

clean:
	rm -f app/Main.o
	rm -f app/Main.hi
	rm -f src/TrainGame.o
	rm -f src/TrainGame.hi
	rm -f train-game-exe
