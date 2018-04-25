#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include "japmalloc.c"

#define ROUNDS 10
#define LOOP 1
#define BUFFER 100

#define MIN 8
#define MAX 4000

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


int main(void)
{
	void* buffer[BUFFER];
	for (int i = 0; i < BUFFER; ++i) {
		buffer[i] = NULL;
	}

	void* init = sbrk(0);
	void* current;

	printf("The initial top of the heap is %p\n", init);

	for (int j = 0; j < ROUNDS; ++j) {
		for (int i = 0; i < LOOP; ++i) {
			int index = rand() % BUFFER;
			if (buffer[index] != NULL) {
				free(buffer[index]);
			}
			size_t size = (size_t) request();
			int* memory;
			printf("Calling malloc\n");
			memory = jmalloc(size);

			if (memory == NULL) {
				fprintf(stderr, "memory allocation failed\n");
				return(1);
			}
			buffer[index] = memory;
			*memory = 12;
		}
		current = sbrk(0);
		int allocated = (int) ((current - init) / 1024);
		printf("%d\n", j);
		printf("The current top of the heap is %p\n", current);
		printf("    increased by %d KByte\n", allocated);
		printf("    which is 0x%x KByte in hex\n", allocated);
	}
	return 0;
}
