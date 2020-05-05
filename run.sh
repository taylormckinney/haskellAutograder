#!/bin/bash

# simple bash script to get each .hs file in a given directory (here "tests/"), compile it
# and run ioTest with it as the command line argument. 


for f in ./tests/*.hs; do
    test -f "$f" || continue
    echo "Testing $f..." 
    ghc ${f%.hs}
    ./ioTest $f
done
