#!/usr/bin/bash

mkdir -p out
rm out/*

if [[ $1 == debug ]]; then
  fpc src/deash.pp -FE"out/" -Fu"inc/" -O4 -Xs -XX -dDEBUG
else
  fpc src/deash.pp -FE"out/" -Fu"inc/" -O4 -Xs -XX
fi
