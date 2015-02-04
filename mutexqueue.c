#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct Node {
    struct Node* left;
    struct Node* right;
    int value;
} Node;

typedef struct Queue {
    struct Node* left;
    struct Node* right;
    pthread_mutex_t m;
} Queue;

void push_left(Queue* q, int val) {
    Node* new_left = (Node*)calloc(1, sizeof (Node));
    new_left->value = val;

    pthread_mutex_lock(&q->m);
    Node* old_left = q->left;
    q->left = new_left;
    if (!old_left) {
        q->right = new_left;
    }
    else {
        new_left->right = old_left;
        old_left->left = new_left;
    }
    pthread_mutex_unlock(&q->m);
}

int pop_right(Queue* q) {
    pthread_mutex_lock(&q->m);
    Node* old_right = q->right;
    if (!old_right) {
      pthread_mutex_unlock(&q->m);
      return -1;
    }
    Node* new_right = old_right->left;
    if (new_right)
        new_right->right = NULL;
    q->right = new_right;
    int v = old_right->value;
    pthread_mutex_unlock(&q->m);
    free(old_right);
    return v;
}

void for_all(Queue* q, void* data, void (*callback)(void*, int)) {
    Node* e = NULL;
    pthread_mutex_lock(&q->m);
    for (e = q->left; e != q->right; e=e->right) {
        callback(data, e->value);
    }
    callback(data, e->value);
    pthread_mutex_unlock(&q->m);
}

void printall(void* prefix, int val) {
    printf("%s: %d\n", prefix, val);
}

void lockup(void* data, int val) {
    Queue* q = (Queue*)data;
    push_left(q, 42);
}

int main(void) {
    Queue* q = (Queue*)calloc(1, sizeof (Queue));
    pthread_mutex_init(&q->m, NULL);
    int n = 10;

    for (int i = 0; i < n; i++) {
        push_left(q, i);
    }
    for_all(q, &"Run 1", printall);
    for_all(q, q, (void (*)(void*, int))&push_left);
    for_all(q, &"Run 2", printall);
    do {
        int res = pop_right(q);
        if (res == -1)
            break;
        printf("Popped: %d\n", res);
    } while (true);

    pthread_mutex_destroy(&q->m);
    free(q);
    pthread_exit(NULL);

    return 0;
}
