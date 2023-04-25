#!/usr/bin/bash

function clean_out {
  mkdir -p out
  rm out/*
}

if [[ $1 == debug ]]; then
  clean_out
  if [[ uname == Linux ]]; then
    fpc src/deash.pp -FE"out/" -Fu"inc/" -g -dDEBUG -n @fpc_linux.cfg
  else
    fpc src/deash.pp -FE"out/" -Fu"inc/" -g -dDEBUG
  fielse  
  clean_out

  if [[ uname == Linux ]]; then
    fpc src/deash.pp -FE"out/" -Fu"inc/" -O4 -Xs -XX -n @fpc_linux.cfg
  else
    fpc src/deash.pp -FE"out/" -Fu"inc/" -O4 -Xs -XX
  fi
fi
