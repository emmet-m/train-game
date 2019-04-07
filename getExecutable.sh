#!/bin/bash

stack build || exit
stack test  || exit

STACKPATH=`stack path --local-install-root`

cp "$STACKPATH/bin/train-game-exe" .
