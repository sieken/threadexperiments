/* threadbench.c
 *
 * Benchmarking jmalloc for a comparison between two different multithread approaches
 * Author: David Henriksson & Eliaz Sundberg
 */

#include <stdio.h>
#include <pthread.h>
#include <string.h>
#include "threadtest.c"

/* Test types for bench;
 * ALL - Run all types
 * SINGLE - Run single-thread only
 * MULTI - Run multi-threaded (without thread local storage) only
 * MULTI_TLS - Run multi-threaded with thread local storage only */
enum testTypes { ALL, SINGLE, MULTI, MULTI_TLS };

#define DEFAULT_NUM_ALLOCS	10000
#define DEFAULT_NUM_THREADS	100
#define DEFAULT_TEST_TYPE	(enum testTypes) ALL

void parse_args(int argc, char *argv[]);
enum testTypes get_type(char *typeString);

/* Needs to be global for argument parsing */
/* TODO move parsing (and global vars into main) instead, if not too messy */
int numAllocs, numThreads;
enum testTypes type;

int main(int argc, char *argv[])
{
	numAllocs = DEFAULT_NUM_ALLOCS;
	numThreads = DEFAULT_NUM_THREADS;
	type = DEFAULT_TEST_TYPE;
	parse_args(argc, argv);

	double timeTotal;

	if (type == MULTI || type == ALL) {
		timeTotal = multi(numThreads, numAllocs);
		printf("Multi: Total time for %d allocations: %.6lf seconds with %d threads.\n", numAllocs, timeTotal, numThreads);
	}
	if (type == MULTI_TLS || type == ALL) {
		timeTotal = multi_tls(numThreads, numAllocs);
		printf("Multi_TLS: Total time for %d allocations: %.6lf seconds with %d threads.\n", numAllocs, timeTotal, numThreads);
	}

	printf("---------------Done, exiting main---------------\n");
	return 0;
}

/* parse_args parses any arguments passed to threadbench, setting any specified
 * supported variables */
void parse_args(int argc, char *argv[])
{
	int opt;
	while ((opt = getopt(argc, argv, "a:t:T:")) != -1) {
		switch (opt) {
			case 'a':
				numAllocs = atoi(optarg);
				break;
			case 't':
				numThreads = atoi(optarg);
				break;
			case 'T':
				type = get_type(optarg);
				break;
			default:
				fprintf(stderr, "Usage: %s [-a nallocs] [-t nthreads] [-T type]\n\n\
\t-a\tRuns nallocs number of allocations\n\
\t-t\tRuns nthreads number of threads (only applicable for\
multithreaded test types\n\
\t-T\tTypes of tests to be run (all, single, multi, multi_tls\n\n", argv[0]);
				exit(1);
		}
	}
}

enum testTypes get_type(char *typeString)
{
	if (strcmp(typeString, "all") == 0)
		return ALL;
	else if (strcmp(typeString, "single") == 0)
		return SINGLE;
	else if (strcmp(typeString, "multi") == 0)
		return MULTI;
	else if (strcmp(typeString, "multi_tls") == 0)
		return MULTI_TLS;
	else {
		printf("Unsupported test type, run with -h for more info\n");
		exit(1);
	}
}
