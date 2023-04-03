CC=gcc
CFLAGS=-lm

%.o:%.c
	$(CC) -c -o $@ $< $(CFLAGS)

jmallocbench: jmallocbench.o
	$(CC) -o jmallocbench jmallocbench.o $(CFLAGS)

.PHONY: run_all run_single run_multi run_multi_tls clean

run_all: jmallocbench
	./jmallocbench

run_single: jmallocbench
	./jmallocbench -T single

run_multi: jmallocbench
	./jmallocbench -T multi

run_multi_tls: jmallocbench
	./jmallocbench -T multi_tls

clean:
	rm -f *.o jmallocbench jmallocthreads