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

/* jfree() and coalesce() written by Eliaz Sundberg 2018 */
void jfree(void*);
void coalesce(struct chunk*);

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
    struct chunk *current = head->next;

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
