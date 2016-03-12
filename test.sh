#!/bin/bash

for input in $(ls ./test/*.in) ; do
	diff -q <(./dka-2-mka -i $input) <($input)
	st=$?

	diff -q <(./dka-2-mka -t $input) <($(basename $input .in).out)
	nd=$?

	if [[ ! $st -eq 0 ]] || [[ ! $nd -eq 0 ]]; then
		printf "$input: -i %d -t %d\n" $st $nd
	fi
done
