#!/bin/bash
rm -rf cabal-dev
cabal-dev add-source ~/projects/highNineP/
cabal-dev install-deps
cabal-dev configure
cabal-dev build
