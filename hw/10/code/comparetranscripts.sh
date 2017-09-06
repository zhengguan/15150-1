#!/bin/bash

## almost all systems will have diff here, but you may need to change this
DIFF=/usr/bin/diff

for file in *.auto.txt
do
    if [[ -e $file ]]
    then
	echo "checking $file against tests/$file"
	diff $file tests/$file
    else
	echo "tests/$file doesn't exist! this is bad"
    fi
done
