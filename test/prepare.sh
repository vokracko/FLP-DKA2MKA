#!/bin/bash


find ./plain -type f | while read file ; do 
	echo $file
	./fstcompile.sh $file ${file/plain/minimal} minimize 
done

