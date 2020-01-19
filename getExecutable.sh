#!/bin/bash

stack build || exit

STACKPATH=`stack path --local-install-root`

cp "$STACKPATH/bin/train-game-exe" .
