#!/usr/bin/bash

rm out/*
fpc src/deash.pp -FE"out/" -Fu"inc/" -O4 -Xs -XX
