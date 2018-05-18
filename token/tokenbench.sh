#!/usr/bin/env bash
# tokenbench.sh
# runs token.c a set number of times and outputs its
# calculated times to a .dat-file

laps=100
threads=1000

echo '' > tokenbench_c.dat

for (( i = 2; i <= $threads; i++ )); do
	./token $laps $i >> tokenbench_c.dat
done
