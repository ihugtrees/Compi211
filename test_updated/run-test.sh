#!/bin/bash

for file_name in {1..169}
do
make -s -f ./test/Makefile test$file_name;\
o1=`scheme -q < test/test$file_name.scm`; o2=`./test$file_name`;\
echo "(equal? '($o1) '($o2))" > test.scm;\
ans=($(scheme -q < test.scm))


if [ $ans = "#t" ] ; then
echo "test$file_name:--true--"

else
echo
echo "test$file_name:"
echo
echo "--false--"
echo
echo "$(cat test.scm)"
echo

# echo test.scm
fi

rm -f compiler/test$file_name.s compiler/test$file_name.o test$file_name test.scm
	
done
