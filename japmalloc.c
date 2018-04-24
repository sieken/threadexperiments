/* japmalloc.c */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

struct memnode {
    size_t maxsize;
    struct memnode *left;
    struct memnode *right;
};

struct mempool {
    struct memnode *root;
    pthread_mutex_t lock;
    pthread_cond_t avail;
};
