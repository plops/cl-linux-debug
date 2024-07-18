#!/bin/bash

# Recursively find all *.asd *.lisp and *.h files in ../
# For each of them print a header // start of <file> 
# and a footer // end of <file>

for file in `find .. -name "*.asd" -o -name "*.lisp" -o -name "*.h"`
do
    echo "// start of $file"
    cat $file
    echo "// end of $file"
done

# Output is 568kB of text

