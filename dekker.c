#include <stdio.h>
#include <pthread.h>
#include <stdbool.h>

bool flag[2] = {false, false};
int turn = 0;

int nums = 0;

void* p0(void* whatevs) {
    flag[0] = true;
    while (__sync_synchronize(), flag[1] == true)
        if (__sync_synchronize(), turn != 0) {
            flag[0] = false;
            while (__sync_synchronize(), turn != 0) {}
            flag[0] = true;
        }
    // critical section
    for (int i = 0; i < 512; i++)
      nums += 1;

    turn = 1;
    __sync_synchronize();
    flag[0] = false;

    return NULL;
}

void* p1(void* whatevs) {
    flag[1] = true;
    while (__sync_synchronize(), flag[0] == true)
        if (__sync_synchronize(), turn != 1) {
            flag[1] = false;
            while (__sync_synchronize(), turn != 1) {}
            flag[1] = true;
        }
    // critical section
    for (int i = 0; i < 512; i++)
      nums -= 1;

    turn = 0;
    __sync_synchronize();
    flag[1] = false;

    return NULL;
}

int main(void) {
    pthread_t th0, th1;
    pthread_create(&th0, NULL, p0, 0);
    pthread_create(&th1, NULL, p1, 0);

    pthread_join(th0, NULL);
    pthread_join(th1, NULL);

    // should be 0 if correct
    printf("Nums: %d\n", nums);
    return nums;
}
