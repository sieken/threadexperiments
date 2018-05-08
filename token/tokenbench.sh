#!/usr/bin/env bash

for (( i = 100; i < 10000; i+=100 )); do
	./token 1000 $i >> tokenfreq.dat
done

