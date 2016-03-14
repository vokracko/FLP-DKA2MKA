#!/bin/bash

for input in $(ls ./test/*.in) ; do
	diff -q <(./dka-2-mka -i $input | sort) <(cat $input | sort) 2>/dev/null >&2
	if [ $? -eq 0 ] ; then
		st="\033[1;32mPASS\033[0m"
	else
		st="\033[1;31mFAIL\033[0m"
	fi

	diff -q <(./dka-2-mka -t $input | sort) <(cat $(dirname $input)/$(basename $input .in).out | sort) 2>/dev/null >&2
	if [ $? -eq 0 ] ; then
		nd="\033[1;32mPASS\033[0m"
	else
		nd="\033[1;31mFAIL\033[0m"
	fi

	echo -e "-i $st -t $nd\t$input"
done
