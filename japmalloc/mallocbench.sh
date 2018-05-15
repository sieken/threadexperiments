#!/usr/bin/env bash

gcc jmallocbench.c -o jmallocbench -lpthread -lm

allocs=10000
threads=100

echo -n '' > jmalloc_single.dat
echo -n '' > jmalloc_multi.dat
echo -n '' > jmalloc_multi_tls.dat

./jmallocbench -T single -a $allocs >> jmalloc_single.dat

for ((i = 1; i <= $threads; i++)); do
	./jmallocbench -T multi -t $i -a $allocs >> jmalloc_multi.dat
done
for ((i = 1; i <= $threads; i++)); do
	./jmallocbench -T multi_tls -t $i -a $allocs >> jmalloc_multi_tls.dat
done

gnuplot -e "set terminal png;
			set xlabel 'Threads';
			set ylabel 'T [ms]';
			set title '$allocs allocations';
			plot 'jmalloc_multi.dat' w l title 'Non-TLS', 'jmalloc_multi_tls.dat' w l title 'TLS'" > mallocbench.png
