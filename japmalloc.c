/* japmalloc.c */

#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <stdio.h>

/* TODO recalculate HEADER_SIZE */
#define HEADER_SIZE 16
#define INITIAL_MEM_REQUEST 4096
#define MINIMUM_FREE_SIZE 1

struct chunk {
    size_t size;
    struct chunk *prev;
    struct chunk *next;
};

struct mempool {
    struct chunk *head;
    pthread_mutex_t lock;
    pthread_cond_t avail;
};

struct mempool *pool = NULL;

/* init_mempool, jmalloc & split_chunk written by David Henriksson 2018 */
void initialize_mempool(void);
void* jmalloc(size_t);
static struct chunk* split_chunk(size_t size, struct chunk *);

void initialize_mempool(void)
{
	void *bottom;
	bottom = sbrk(0);
	if (sbrk(INITIAL_MEM_REQUEST) < 0) {
		fprintf(stderr, "jmalloc: failed to initialize memory pool (out of memory?)\n");
		exit(1);
	} else {
		pool = bottom;
		pool->head = (struct chunk*) ((struct mempool*) pool + 1);
		pool->head->size = INITIAL_MEM_REQUEST - sizeof(struct chunk);
		/* TODO initialize lock? */
	}
}

void* jmalloc(size_t size)
{
	size_t totsize = size + HEADER_SIZE;

	/* TODO lock */

	if (pool == NULL) {
		initialize_mempool();
	}

	struct chunk *entry = pool->head;
	/* Step through free-list to find a fitting memory block */
	while (entry->size < totsize) {
		entry = entry->next;
		if (entry == NULL) {
			printf("jmalloc: out of memory in free-list (FIX)\n");
			return NULL;
		}
	}

	/* Place remainder in free list if split is successful */
	struct chunk *remainder;
	if ((remainder = split_chunk(totsize, entry)) != NULL) {
		remainder->prev = entry->prev;
		remainder->next = entry->next;
		if (entry == pool->head)
			pool->head = remainder;
	}

	/* TODO unlock*/

	return (void*) ((struct chunk *) entry + 1);

}

/* Split the block to fit size, and return pointer to block containing the remaining memory after splitting,
 * and return NULL if remaining space is too small to perform a split */
static struct chunk* split_chunk(size_t size, struct chunk *node)
{
	struct chunk *newBlock = NULL;

	/* Return if remainder size is too small */
	size_t remainderSize = node->size - size - HEADER_SIZE;
	if (remainderSize < MINIMUM_FREE_SIZE)
		return newBlock;

	newBlock = (struct chunk*) ((void*) node + HEADER_SIZE + size);
	newBlock->size = remainderSize;
	node->size = size;

	return newBlock;
}
