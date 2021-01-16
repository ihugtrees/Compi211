#!/bin/bash
filename="$1"
make -f ./compiler/Makefile $filename
echo "------ Our Output ------"
./$filename
o1=`scheme -q < $filename.scm`
o2=`./$filename`
echo "(equal? '($o1) '($o2))" > test.scm
echo "------ Compare to Chez ------"
scheme -q < test.scm
rm ./compiler/$filename.*
rm $filename
