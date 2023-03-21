#!/usr/bin/bash

function clean_out {
  mkdir -p out
  rm out/*
}

if [[ $1 == debug ]]; then
  clean_out
  fpc src/deash.pp -FE"out/" -Fu"inc/" -g -dDEBUG
elif [[ $1 == docs ]]; then
  cd docs
  mkdir -p html
  rm -f html/*
  for file in *.sad; do
    sadv "$file" -x
    mv "$file".html html/"$file".html
  done
  cd ..
else  
  clean_out
  fpc src/deash.pp -FE"out/" -Fu"inc/" -O4 -Xs -XX
fi
