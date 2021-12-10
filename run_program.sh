#!/bin/bash

make
./lilc $1 -C | gcc -xc -
./a.out
rm -f a.out
make clean
