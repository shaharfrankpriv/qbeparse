#!/bin/bash

ssa=$1
if [ -z "$ssa" ]; then
	echo "usage: $0 <file.ssa>"
	exit 1
fi

make $ssa
eog "${ssa//.ssa/.svg}"
