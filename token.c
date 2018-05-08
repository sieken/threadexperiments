#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>

struct token {
	int token;
};

struct threadNode {
	struct threadNode *next;
	pthread_t thread;
	pthread_mutex_t lock;
	pthread_cond_t tokenReady;
	struct token *token;
};

int numRounds;
int numThreads;
clock_t start, end;

struct threadNode* createCircle(void);
void spawnThreads(struct threadNode *);
void joinThreads(struct threadNode *);
void threadFunc(struct threadNode *);

int main(int argc, char const *argv[]) {
	if (argc < 3) {
		fprintf(stderr, "rounds and threads needed as argument\n");
		exit(1);
	}

	numRounds = atoi(argv[1]);
	numThreads = atoi(argv[2]);

	struct threadNode *head = createCircle();
	spawnThreads(head);
	joinThreads(head);

	double total = ((double)end - (double)start) / CLOCKS_PER_SEC;
	printf("%d %.6lf\n", numThreads, total);

	return 0;
}

/* Creates a circular linked list of threadNodes and returns head of list */
struct threadNode* createCircle() {

	struct threadNode *head = (struct threadNode *) malloc(sizeof(struct threadNode));
	pthread_mutex_init(&head->lock, NULL);
	pthread_cond_init(&head->tokenReady, NULL);

	struct threadNode *current = head;
	for (int i = 0; i < numThreads - 1; i++) {
		pthread_mutex_init(&current->lock, NULL);
		pthread_cond_init(&current->tokenReady, NULL);
		current->next = (struct threadNode *) malloc(sizeof(struct threadNode));
		current = current->next;
	}

	/* Bring end of list around to create circle */
	current->next = head;

	return head;
}

/* Spawn numThreads threads to run threadFunc procedure, start clock */
void spawnThreads(struct threadNode *node) {
	for (int i = 0; i < numThreads; i++) {
		pthread_create(&node->thread, NULL, (void*)threadFunc, (void*)node);
		node = node->next;
	}
	struct token *token = (struct token*) malloc(sizeof(token));
	token->token = 42;

	node->token = token;
	start = clock();

	pthread_cond_signal(&node->tokenReady);
}

/* Join all threads and stop clock */
void joinThreads(struct threadNode *node) {
	for (int i = 0; i < numThreads; i++) {
		pthread_join(node->thread, NULL);
		node = node->next;
	}
	end = clock();
}

/* Thread procedure for passing token */
void threadFunc(struct threadNode *self) {
	int rounds = numRounds;
	while(rounds > 0) {
		pthread_mutex_lock(&self->next->lock);
		while(self->next->token == NULL) {
			pthread_cond_wait(&self->next->tokenReady, &self->next->lock);
		}
		self->token = self->next->token;
		self->next->token = NULL;

		pthread_mutex_unlock(&self->next->lock);
		pthread_cond_signal(&self->tokenReady);

		rounds--;
	}
}
