#!/bin/bash
set -e

clear

cabal install
echo "** COMPILED **"

.cabal-sandbox/bin/base-haskell

echo "** DONE **"
