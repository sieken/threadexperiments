#include <stdio.h>
#include <pthread.h>
#include <math.h>
#include <time.h>

#include "jmalloc.c"

#define MIN 8
#define MAX 4000
#define BUFFER_SIZE 1000

struct {
	int counter;
	int done;
	pthread_mutex_t lock;
} allocStatus;

enum mode { NON_TLS, TLS };

clock_t end;

double single(int);
double multi(int, int);
double multi_tls(int, int);
static int request(void);
static double start_alloc(int, int, enum mode);
static void thread_allocate(void);
static void thread_tls_allocate(void);

double single(int nAllocs)
{
	int nThreads = 1;
	return start_alloc(nThreads, nAllocs, NON_TLS);
}

double multi(int nThreads, int nAllocs)
{
	return start_alloc(nThreads, nAllocs, NON_TLS);
}

double multi_tls(int nThreads, int nAllocs)
{
	return start_alloc(nThreads, nAllocs, TLS);
}

static double start_alloc(int nThreads, int nAllocs, enum mode mode)
{
	pthread_t threads[nThreads];

	clock_t start;
	double total;

	allocStatus.counter = nAllocs;
	allocStatus.done = 0;
	pthread_mutex_init(&allocStatus.lock, NULL);

	void (*funcToRun)(void);
	if (mode == NON_TLS) {
		funcToRun = thread_allocate;
	} else if (mode == TLS) {
		funcToRun = thread_tls_allocate;
	} else {
		fprintf(stderr, "Invalid mode, mode was: %d \n", mode);
		exit(1);
	}

	/* Start timer and spawn threads */
	start = clock();
	for (int i = 0; i < nThreads; i++) {
		pthread_create(&threads[i], NULL, (void*)funcToRun, NULL);
	}

	/* Wait for threads to finish */
 	for (int i = 0; i < nThreads; i++) {
		pthread_join(threads[i], NULL);
	}

	total = (double)(end - start) / CLOCKS_PER_SEC;

	return total;

}

/* request() written by Johan Montelius, KTH */
static int request()
{
	/* k is log (MAX/MIN) */
	double k = log((double) MAX/MIN);

	/* r is [0..k] */
	double r = ((double) (rand() % (int)(k * 10000))) / 10000;

	/* size is [0..MAX] */
	int size = (int) ((double) MAX/exp(r));

	return size;
}

static void thread_allocate()
{
	static __thread int *buffer[BUFFER_SIZE] = { NULL };
	while(1) {
		pthread_mutex_lock(&allocStatus.lock);
		if (allocStatus.done) {
			pthread_mutex_unlock(&allocStatus.lock);
			break;
		}
		pthread_mutex_unlock(&allocStatus.lock);

		/* Randomly free memory */
		int index = request() % BUFFER_SIZE;
		if (buffer[index] != NULL) {
			jfree(buffer[index]);
		}

		/* Allocate memory, write to memory */
		int randSize = request();
		int *mem = (int *) jmalloc(randSize);
		*mem = 123;

		buffer[index] = mem;

		pthread_mutex_lock(&allocStatus.lock);
		allocStatus.counter--;
		if ((allocStatus.counter < 1) && !allocStatus.done) {
			end = clock();
			allocStatus.done = 1;
		}
		pthread_mutex_unlock(&allocStatus.lock);
	}
}

static void thread_tls_allocate(void)
{
	static __thread int *buffer[BUFFER_SIZE] = { NULL };
	while(1) {
		pthread_mutex_lock(&allocStatus.lock);
		if (allocStatus.done) {
			pthread_mutex_unlock(&allocStatus.lock);
			break;
		}
		pthread_mutex_unlock(&allocStatus.lock);

		/* Randomly free memory */
		int index = request() % BUFFER_SIZE;
		if (buffer[index] != NULL) {
			jfree_tls(buffer[index]);
		}

		/* Allocate memory, write to memory */
		int randSize = request();
		int *mem = (int *) jmalloc_tls(randSize);
		*mem = 123;

		buffer[index] = mem;

		pthread_mutex_lock(&allocStatus.lock);
		allocStatus.counter--;
		if ((allocStatus.counter < 1) && !allocStatus.done) {
			end = clock();
			allocStatus.done = 1;
		}
		pthread_mutex_unlock(&allocStatus.lock);
	}
}
