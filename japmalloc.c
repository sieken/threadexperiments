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
	pthread_mutex_t lock;
};

static struct mempool pool = { NULL, 0, PTHREAD_MUTEX_INITIALIZER };
static __thread struct mempool tlsPool = { NULL, 0, PTHREAD_MUTEX_INITIALIZER };
static pthread_mutex_t poolLock = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t sbrkLock = PTHREAD_MUTEX_INITIALIZER;

void* jmalloc(size_t);
void* jmalloc_tls(size_t);
void jfree(void *);
void jfree_tls (void *);
static struct chunk* request_memory(size_t);
static void* allocate_from_list(size_t, struct mempool *);
static void free_to_list(void *, struct mempool *);
static void coalesce(struct chunk *);


/* Print functions for debugging/troubleshooting */
/* TODO delete before delivery */
void print_list(void);
void print_list_from(struct chunk *);

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

	pthread_mutex_unlock(&sbrkLock);

	return newChunk;
}

void initialize_mempool(struct mempool *mempool)
{
	pthread_mutex_lock(&sbrkLock);

	void *bottom;
	if ((bottom = sbrk(INITIAL_MEM_REQUEST + HEADER_SIZE)) < 0) {
		fprintf(stderr, "jmalloc: failed to initialize memory pool (out of memory?)\n");
		exit(1);
	} else {
		mempool->head = (struct chunk*) bottom;
		mempool->head->size = INITIAL_MEM_REQUEST;
		pthread_mutex_init(&mempool->lock, NULL);
		mempool->initialized = MEMPOOL_INITIALIZED;
	}

	pthread_mutex_unlock(&sbrkLock);
}

void* jmalloc(size_t size)
{
	/* First jmalloc() call initializes the memory pool */
	pthread_mutex_lock(&poolLock);
	if (pool.initialized == MEMPOOL_NOT_INITIALIZED) {
		initialize_mempool(&pool);
	}
	pthread_mutex_unlock(&poolLock);

	pthread_mutex_lock(&pool.lock);
	void *allocated = allocate_from_list(size, &pool);
	pthread_mutex_unlock(&pool.lock);

	return allocated;
}

/*	jmalloc_tls works as jmalloc above, but uses memory pool in thread local storage */
void* jmalloc_tls(size_t size)
{
	if (tlsPool.initialized == MEMPOOL_NOT_INITIALIZED) {
		initialize_mempool(&tlsPool);
	}
	void *mem = allocate_from_list(size, &tlsPool);
	return mem;
}

/* jfree takes an address to memory previously allocated by jmalloc and frees is, placing
 * it in internally managed free list ordered by addresses in ascending order */
void jfree (void *addr)
{
	pthread_mutex_lock(&pool.lock);
	free_to_list(addr, &pool);
	pthread_mutex_unlock(&pool.lock);
}

/* jfree_tls works as jfree above, but uses memory pool local to each thread */
void jfree_tls (void *addr)
{
	free_to_list(addr, &tlsPool);
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

	void *allocatedMem = (void*) ((void*) entry + HEADER_SIZE);

	return allocatedMem;
}

/* Finds chunk from addr, and places chunk in ascending address order in free list */
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

/* Coalesces a chunk with its previous and next entry if their addresses overlap */
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
