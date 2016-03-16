#!/bin/bash

ghc dka-2-mka.hs

rm -rf test/diffs/*/*

for input in $(ls ./test/*.in) ; do
	diff <(./dka-2-mka -i $input | sort) <(cat $input | sort) > test/diffs/i/$(basename $input .in).diff
	if [ $? -eq 0 ] ; then
		st="\033[1;32mPASS\033[0m"
	else
		st="\033[1;31mFAIL\033[0m"
	fi

	diff <(./dka-2-mka -t $input | sort) <(cat $(dirname $input)/$(basename $input .in).out | sort) > test/diffs/t/$(basename $input .in).diff
	if [ $? -eq 0 ] ; then
		nd="\033[1;32mPASS\033[0m"
	else
		nd="\033[1;31mFAIL\033[0m"
	fi

	echo -e "-i $st -t $nd\t$input"
done

find test -empty -exec rm {} \;
