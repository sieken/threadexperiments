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

/* jfree() and coalesce() written by Eliaz Sundberg 2018 */
void jfree(void*);
void coalesce(struct chunk*);


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

/*
 * jfree keeps the free list ordered from low addresses to high
 * TODO possibly implement binary search on list?
 * TODO locks and cond vars
 */
void jfree (void *addr) {
    struct chunk *ptr = (struct chunk*)((struct chunk*)addr - 1);

    /* if the memory to be freed is in low memory (before head), add to beginning of list and update head */
    if (ptr < pool->head) {
        ptr->next = pool->head;
        ptr->prev = NULL;
        pool->head->prev = ptr;
        pool->head = ptr;
        coalesce(pool->head);
        return;
    }

    /* traverse the list to find appropriate location for new chunk */
    struct chunk *current = pool->head->next;

    while (ptr < current) {
        /* if end of list, add */
        if (current->next == NULL) {
            current->next = ptr;
            ptr->prev = current;
            ptr->next = NULL;
            coalesce(ptr->prev);
            return;
        }
        current = current->next;
    }
    /* if ptr > current, squeeze in between current->prev and current */
    current->prev->next = ptr;
    ptr->prev = current->prev;
    current->prev = ptr;
    ptr->next = current;
    coalesce(ptr);
    return;
}

/* coalesces two adjecent chunks */
void coalesce(struct chunk* ptr) {
    void *prevaddr;
    prevaddr = (void*)((struct chunk*)ptr->prev + 1);

    /* if we're at the list head, skip */
    if (ptr->prev != NULL) {
        /* if the address to the end of the previous list entry (plus 1) is the same as
         * the current address, coalesce them
         */
        if (prevaddr + ptr->prev->size == ptr) {
            ptr->prev->next = ptr->next;
            ptr->next->prev = ptr->prev;
            ptr->prev->size += ptr->size + HEADER_SIZE;
        }
    }

    /* if we're at list tail, skip */
    if (ptr->next != NULL) {
        /* if the address of the current list entry plus its size (plus one HEADER_SIZE) is
         * the same as the next list entries address, coalesce them
         */
        if (ptr + ptr->size + HEADER_SIZE == ptr->next) {
            ptr->next = ptr->next->next;
            ptr->next->prev = ptr;
            ptr->size += ptr->next->size + HEADER_SIZE;
        }
    }
}
