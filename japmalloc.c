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

/* init_mempool, jmalloc, split_chunk, request_memory written by David Henriksson 2018 */
static void initialize_mempool(void);
void* jmalloc(size_t);
static struct chunk* split_chunk(size_t size, struct chunk *);
static struct chunk* request_memory(size_t);

static struct chunk* request_memory(size_t size)
{
	struct chunk *newChunk;
	if ((newChunk = sbrk(size + HEADER_SIZE)) < 0) {
		fprintf(stderr, "jmalloc: memory request failed\n");
		return NULL;
	} else {
		newChunk->size = size;
		newChunk->prev = NULL;
		newChunk->next = NULL;
	}

	return newChunk;
}

static void initialize_mempool(void)
{
	void *bottom;
	if ((bottom = sbrk(INITIAL_MEM_REQUEST + HEADER_SIZE + sizeof(struct mempool))) < 0) {
		fprintf(stderr, "jmalloc: failed to initialize memory pool (out of memory?)\n");
		exit(1);
	} else {
		pool = bottom;
		pool->head = (struct chunk*) ((struct mempool*) pool + 1);
		pool->head->size = INITIAL_MEM_REQUEST;
		/* TODO initialize lock? */
	}
}

void* jmalloc(size_t size)
{
	/* TODO lock */

	if (pool == NULL) {
		initialize_mempool();
	}

	struct chunk *entry = pool->head;
	/* Step through free-list to find a fitting memory block */
	while (entry->size < size) {
		if (entry->next == NULL)
			entry->next = request_memory(size);
		entry = entry->next;
	}

	/* Place remainder in free list if split is successful */
	struct chunk *remainder;
	if ((remainder = split_chunk(size, entry)) != NULL) {
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
