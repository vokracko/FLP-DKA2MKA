#!/bin/bash

function compareFsm
{
	input=$1
	minimal=${input/plain/minimal/}
	res=${input/plain/res/}

	../dka-2-mka -i $input > $res.plain
	./fstcompile.sh $res.plain $res.plain.fst
	fstequivalent $res.plain.fst $minimal # minimal is always equivalent with coresponding non minimal

	if [ $? -eq 0 ] ; then
		st="\033[1;32mPASS\033[0m"
	else
		st="\033[1;31mFAIL\033[0m"
	fi

	../dka-2-mka -t $input > $res.minimal
	./fstcompile.sh $res.minimal $res.minimal.fst
	fstequivalent $res.minimal.fst $minimal

	if [ $? -eq 0 ] ; then
		nd="\033[1;32mPASS\033[0m"
	else
		nd="\033[1;31mFAIL\033[0m"
	fi

	echo -e "-i $st -t $nd\t$input"
}

rm res/* 2>/dev/null

[[ $# -eq 1 ]] && {
	checkFsm $1
} || {
	for input in $(ls ./plain/*) ; do
		compareFsm $input
	done
}
