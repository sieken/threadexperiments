/* japmalloc.c */

#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

#define HEADER_SIZE 16

struct memnode {
    size_t maxsize;
    struct memnode *left;
    struct memnode *right;
};

struct mempool {
    struct memnode *root;
    pthread_mutex_t lock;
    pthread_cond_t avail;
} pool;

/* jmalloc & traverse_tree written by David Henriksson 2018 */
void* jmalloc(size_t);
static void* traverse_tree(size_t, struct memnode *);
static struct memnode* split_block(size_t size, struct memnode *memblock);

void* jmalloc(size_t size)
{
	void *ptr = NULL;

	/* TODO Lock */
	
	if (size < 1)
		return ptr;

	if (pool.root->maxsize == 0) {
		ptr = sbrk(0);
		if (sbrk(1024) < 0)
			return NULL;
		else {
			pool.root = ptr;
			pool.root->maxsize = 1024 - HEADER_SIZE;
			pool.root->left = NULL;
			pool.root->right = NULL;
		}
	}

	if (pool.root->maxsize < size)
		/* TODO */
		return ptr;
	else
		traverse_tree(size, pool.root);

	/* TODO Unlock */

	/* TODO Fix address of actual memory block */
	return ptr;
}

/* Recursively traverse tree structure that holds memory blocks, and return appropriate sized block */
static void* traverse_tree(size_t size, struct memnode *node)
{
	/* If node has no subtrees we have reached a memory block */
	if (node->left == NULL && node->right == NULL) {
		/* Make sure we get an appropriate power of 2 size memory block */
		node = split_block(size, node);
		return node;
	}

	if (node->left->maxsize >= size)
		return traverse_tree(size, node->left);
	else
		return traverse_tree(size, node->right);

}

/* Recursively split current block in powers of two until good fit for request, and manage tree structure */
static struct memnode* split_block(size_t size, struct memnode *node)
{
	/* If half of size of memnode is big enough, we need to split*/
	if ((node->maxsize / 2) >= size + HEADER_SIZE) {
		/* TODO Split and make subtrees */
		int leftsize, rightsize;
		struct memnode *leftnode, *rightnode;
		rightsize = ((HEADER_SIZE + node->maxsize) / 2) - HEADER_SIZE;
		leftsize = rightsize - HEADER_SIZE;
		leftnode = (struct memnode*) ((struct memnode*) node + 1);
		rightnode = (struct memnode*) ((void*) leftnode + rightsize);
		leftnode->maxsize = leftsize;
		rightnode->maxsize = rightsize;
		node->left = leftnode;
		node->left->left = node->left->right = NULL;
		node->right = rightnode;
		node->right->left = node->right->right = NULL;
		return split_block(size, node->left);
	} else {
		return node;
	}
}
