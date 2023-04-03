#!/usr/bin/env bash
# mallocbench.sh
# runs benchmarks for memory allocator with increasing number of threads
# outputs .dat-files

allocs=10000
threads=100

echo -n '' > jmalloc_multi.dat
echo -n '' > jmalloc_multi_tls.dat

for ((i = 1; i <= $threads; i++)); do
	./jmallocbench -T multi -t $i -a $allocs >> jmalloc_multi.dat
done

for ((i = 1; i <= $threads; i++)); do
	./jmallocbench -T multi_tls -t $i -a $allocs >> jmalloc_multi_tls.dat
done
