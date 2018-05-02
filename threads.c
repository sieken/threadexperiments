/* threadtest.c */

#include <stdio.h>
#include <pthread.h>
#include <pthread.h>
#include <math.h>
#include <time.h>

#include "japmalloc.c"

#define MIN 8
#define MAX 4000

struct memthread {
	pthread_t thread;
	struct mempool *pool;
};

struct {
	int counter;
	int done;
	pthread_mutex_t lock;
} allocStatus;

clock_t end;

double multi(int, int);
double multi_tls(int, int);
int request(void);
void thread_allocate(void);
void thread_tls_allocate(struct memthread*);

/* TODO för multi_tls måste minnespoolen i varje tråd-struct allokeras på heapen,
 * samt initialiseras */

double multi(int nThreads, int nAllocs)
{
	struct memthread threads[nThreads];

	clock_t start;
	double total;

	/* Initialize allocStatus struct */
	allocStatus.counter = nAllocs;
	allocStatus.done = 0;
	pthread_mutex_init(&allocStatus.lock, NULL);

	/* Starting clock */
	start = clock();

	/* Spawn threads */
	for (int i = 0; i < nThreads; i++) {
		pthread_create(&threads[i].thread, NULL, (void*)thread_allocate, NULL);
	}

	/* Wait for threads to finish */
 	for (int i = 0; i < nThreads; i++) {
		pthread_join(threads[i].thread, NULL);
	}

	total = (double)(end - start) / CLOCKS_PER_SEC;

	return total;
}

double multi_tls(int nThreads, int nAllocs)
{
	struct memthread threads[nThreads];

	clock_t start;
	double total;

	allocStatus.counter = nAllocs;
	allocStatus.done = 0;
	pthread_mutex_init(&allocStatus.lock, NULL);


	/* Initialize mempools */
	for (int i = 0; i < nThreads; i++) {
		threads[i].pool = (struct mempool *) jmalloc(sizeof(struct mempool));
		initialize_mempool(&threads[i].pool);
	}

	/* Start timer */
	start = clock();

	/* Spawn threads */
	for (int i = 0; i < nThreads; i++) {
		pthread_create(&threads[i].thread, NULL, (void*)thread_tls_allocate, &threads[i]);
	}

	/* Wait for threads to finish */
 	for (int i = 0; i < nThreads; i++) {
		pthread_join(threads[i].thread, NULL);
	}

	return total;
}

/* request() written by Johan Montelius, KTH */
int request()
{
	/* k is log (MAX/MIN) */
	double k = log((double) MAX/MIN);

	/* r is [0..k] */
	double r = ((double) (rand() % (int)(k * 10000))) / 10000;

	/* size is [0..MAX] */
	int size = (int) ((double) MAX/exp(r));

	return size;
}

void thread_allocate()
{
	while(1) {
		pthread_mutex_lock(&allocStatus.lock);
		if (allocStatus.done) {
			pthread_mutex_unlock(&allocStatus.lock);
			break;
		}
		pthread_mutex_unlock(&allocStatus.lock);

		/* Allocate memory, write to memory, free memory */
		/* TODO buffer for random freeing */
		int randSize = request();
		int *mem = (int *) jmalloc(randSize);
		*mem = 123;
		jfree(mem);

		pthread_mutex_lock(&allocStatus.lock);
		allocStatus.counter--;
		if ((allocStatus.counter < 1) && !allocStatus.done) {
			allocStatus.done = 1;
			end = clock();
		}
		pthread_mutex_unlock(&allocStatus.lock);
	}
}

void thread_tls_allocate(struct memthread *threadStruct)
{
	while (1) {
		pthread_mutex_lock(&allocStatus.lock);
		if (allocStatus.done) {
			pthread_mutex_unlock(&allocStatus.lock);
			break;
		}
		pthread_mutex_unlock(&allocStatus.lock);

		/* Allocate memory, write to memory, free memory */
		/* TODO buffer for random freeing */
		int randSize = request();
		int *mem = (int *) jmalloc_tls(randSize, threadStruct->pool);
		*mem = 123;
		jfree_tls(mem, threadStruct->pool);

		pthread_mutex_lock(&allocStatus.lock);
		allocStatus.counter--;
		if ((allocStatus.counter < 1) && !allocStatus.done) {
			allocStatus.done = 1;
			end = clock();
		}
		pthread_mutex_unlock(&allocStatus.lock);
	}
}
