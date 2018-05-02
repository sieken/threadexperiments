/* japmalloc.c */

#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <stdio.h>

#define HEADER_SIZE sizeof(struct chunk)
#define INITIAL_MEM_REQUEST 4096

/* TODO Clean up code before delivery */

struct chunk {
	size_t size;
	struct chunk *prev;
	struct chunk *next;
};

struct mempool {
	struct chunk *head;
	/* TODO consider removing memtot, possible better solution */
	size_t memtot;
	pthread_mutex_t lock;
	pthread_cond_t avail;
};

struct mempool *pool = NULL;
pthread_mutex_t sbrkLock;

/* init_mempool, jmalloc, request_memory written by David Henriksson 2018 */
static void initialize_mempool(struct mempool **);
void* jmalloc(size_t);
static struct chunk* request_memory(size_t);
void* jmalloc_tls(size_t size, struct mempool　*);
static void* allocate_from_list(size_t size, struct mempool　*);

/* jfree() and coalesce() written by Eliaz Sudberg 2018 */
void jfree(void *);
void jfree_tls (void *, struct mempool　*);
static void free_to_list(void *, struct mempool *);
static void coalesce(struct chunk　*);


/* Print functions for debugging/troubleshooting */
/* TODO delete before delivery */
void print_list(void);
void print_list_from(struct chunk　*);

static struct chunk* request_memory(size_t size)
{
	struct chunk *newChunk;
	pthread_mutex_lock(&sbrkLock);
	if ((newChunk = sbrk(size + HEADER_SIZE)) < 0) {
		fprintf(stderr, "jmalloc: memory request failed\n");
		return NULL;
	} else {
		newChunk->size = size;
		newChunk->prev = NULL;
		newChunk->next = NULL;
	}

	pool->memtot += sbrk(0) - (void*)newChunk;

	pthread_mutex_unlock(&sbrkLock);

	return newChunk;
}

void initialize_mempool(struct mempool **mempool)
{
	void *bottom;
	if ((bottom = sbrk(INITIAL_MEM_REQUEST + HEADER_SIZE + sizeof(struct mempool))) < 0) {
		fprintf(stderr, "jmalloc: failed to initialize memory pool (out of memory?)\n");
		exit(1);
	} else {
		*mempool = bottom;
		(*mempool)->head = (struct chunk*) ((struct mempool*) pool + 1);
		(*mempool)->head->size = INITIAL_MEM_REQUEST;
		(*mempool)->memtot = sbrk(0) - bottom;
		pthread_mutex_init(&(*mempool)->lock, NULL);
		pthread_cond_init(&(*mempool)->avail, NULL);
		pthread_mutex_init(&sbrkLock, NULL);
		// (*mempool)->lock = PTHREAD_MUTEX_INITIALIZER;
		// (*mempool)->avail = PTHREAD_COND_INITIALIZER;
		/* TODO initialize lock? */
	}
}

/*  */
void* jmalloc(size_t size)
{
	/* First jmalloc() call initializes the memory pool */
	if (pool == NULL) {
		initialize_mempool(&pool);
	}

	pthread_mutex_lock(&pool->lock);
	// while (pthread_mutex_lock(&pool->lock) != 0) {
	// 	pthread_cond_wait(&pool->avail, &pool->lock);
	// }

	void *allocated = allocate_from_list(size, pool);

	pthread_mutex_unlock(&pool->lock);
	// pthread_cond_signal(&pool->avail);

	return allocated;
}

/*	Wrapper function for jmalloc with thread local storage. Function assumes
	that each thread provides its own memory pool struct, and as such should be thread-
	safe without locks. */
void* jmalloc_tls(size_t size, struct mempool *mempool)
{
	return allocate_from_list(size, mempool);
}

static void* allocate_from_list(size_t size, struct mempool *mempool)
{
	/* If list is empty, allocate a big slab of new memory */
	if (mempool->head == NULL)
		mempool->head = request_memory(INITIAL_MEM_REQUEST);
	/* Walk through list to find entry with size >= requested size */
	struct chunk *entry = mempool->head;
	while (entry->size < size) {
		/* If end of list, request a new block of memory to fit requested size */
		if (entry->next == NULL) {
			entry->next = request_memory(size);
			entry->next->prev = entry;
		}
		entry = entry->next;
	}

	/* If entry is much larger than requested size we split and return
	* a memory chunk that just fits requested size, otherwise we return
	* the entire chunk */
	if (entry->size > size + HEADER_SIZE) {
		entry->size -= (size + HEADER_SIZE);

		/* Initialize the new memory block with a header */
		int addrDelta = HEADER_SIZE + entry->size;
		struct chunk *newChunk = (struct chunk*) ((void*) entry + addrDelta);
		newChunk->size = size;

		/* point entry to the new block */
		entry = newChunk;
	} else {
		/* Remove entry from list */
		if (entry->prev != NULL)
			entry->prev->next = entry->next;
		if (entry->next != NULL)
			entry->next->prev = entry->prev;
		if (mempool->head == entry)
			mempool->head = entry->next;
	}

	/* TODO unlock*/

	void *allocatedMem = (void*) ((void*) entry + HEADER_SIZE);

	return allocatedMem;
}

/*
* jfree keeps the free list ordered from low addresses to high
* TODO possibly implement binary search on list?
* TODO locks and cond vars
*/

/* Wrapper functions depending on TLS or not */
void jfree (void *addr)
{
	pthread_mutex_lock(&pool->lock);
	// while (pthread_mutex_lock(&pool->lock) != 0) {
	// 	pthread_cond_wait(&pool->avail, &pool->lock);
	// }

	free_to_list(addr, pool);

	pthread_mutex_unlock(&pool->lock);
	// pthread_cond_signal(&pool->avail);
}

void jfree_tls (void *addr, struct mempool *mempool)
{
	free_to_list(addr, mempool);
}

static void free_to_list(void *addr, struct mempool *mempool)
{
	struct chunk *ptr = (struct chunk*)((void*)(addr - HEADER_SIZE));

	if (mempool->head == NULL) {
		/* Empty list, ptr is put on head of list */
		ptr->next = NULL;
		ptr->prev = NULL;
		mempool->head = ptr;
	} else if (ptr < mempool->head) {
		/* ptr has lower address than current head, ptr is new head */
		ptr->next = mempool->head;
		ptr->prev = NULL;
		mempool->head->prev = ptr;
		mempool->head = ptr;
	} else if (mempool->head->next == NULL) {
		/* If list only contains one entry, ptr is put after it */
		ptr->next = NULL;
		ptr->prev = mempool->head;
		mempool->head->next = ptr;
	} else {
		/* Traverse list to find where the new entry should go */
		struct chunk *current = mempool->head->next;
		while (ptr > current) {
			/* If end of list is reached put the returned block on at the end */
			if (current->next == NULL) {
				ptr->next = NULL;
				ptr->prev = current;
				current->next = ptr;

				coalesce(ptr);

				return;
			}
			current = current->next;
		}
		/* The general case, ptr goes between two list entries */
		ptr->next = current;
		ptr->prev = current->prev;
		current->prev->next = ptr;
		current->prev = ptr;
	}
	coalesce(ptr);
	return;
}

/* Coalesces two adjecent chunks */
static void coalesce(struct chunk* ptr) {

	/* Try to coalesce ptr with its previous entry */
	if (ptr->prev != NULL) {
		void *prevaddr = (void*) ptr->prev;

		/* Check if previous entry aligns with ptr */
		if ((void*)(prevaddr + ptr->prev->size + HEADER_SIZE) == ptr) {
			ptr->prev->next = ptr->next;
			ptr->prev->size += ptr->size + HEADER_SIZE;

			/* Freed memory block is coalesced with, and starts from, its previous entry */
			ptr = ptr->prev;
		}
	}

	/* Try to coalesce ptr with its next entry */
	if (ptr->next != NULL) {
		int addrDelta = ptr->size + HEADER_SIZE;

		/* Check if next entry aligns with ptr */
		if (((void*)ptr + addrDelta) == ptr->next) {
			ptr->size += ptr->next->size + HEADER_SIZE;
			ptr->next = ptr->next->next;
			if (ptr->next != NULL) {
				ptr->next->prev = ptr;
			}
		} else {
			/* If size doesn't add up we knit together the list again */
			ptr->next->prev = ptr;
		}
	}
}

/* Print functions for debugging purposes */

/* Prints entire list */
void print_list(void)
{
	struct chunk *entry = pool->head;
	struct chunk *prev = NULL;

	size_t totalSize = 0, maxSize = 0, minSize;
	unsigned long counter = 0;
	int unordered = 0;

	if (entry != NULL) {
		minSize = entry->size;
	}
	while (entry != NULL) {
		totalSize += entry->size;

		if (entry->size > maxSize)
		maxSize = entry->size;

		if (entry->size < minSize)
		minSize = entry->size;

		if (entry < prev) {
			printf("entry < prev\n");
			unordered = 1;
		}
		counter++;
		prev = entry;
		entry = entry->next;
	}
	printf("Free list properties: \n");
	printf("	Total size: %ld\n", totalSize);
	printf("	Largest block: %ld\n", maxSize);
	printf("	Smallest block: %ld\n", minSize);
	printf("	Number of blocks: %ld\n", counter);
	if (unordered)
		printf("List is unordered!\n");
}

/* Prints 10 elements, from cnk - 1, in free list */
void print_list_from(struct chunk *cnk)
{
	int counter = 0;
	int prev = 0;

	struct chunk *entry;

	if (cnk->prev != NULL) {
		entry = cnk->prev;
		prev = 1;
	} else
		entry = cnk;

	if (prev)
		printf("prev\n |\n");
	else
		printf("cnk\n |\n");

	while (entry != NULL) {
		if (counter > 9) {
			printf("\n");
			break;
		}
		printf(" %ld ->", entry->size);
		entry = entry->next;
		counter++;
	}
	if (entry == NULL)
	printf(" %p\n", entry);
}
