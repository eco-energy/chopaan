// Native throughput demo: steps the chopaan env (categorified Hopfield physics)
// with random actions and reports steps/sec. No Python/pufferlib needed.
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "chopaan.h"
#include "hopfield_step.c"   // pull the kernel into this TU

int main(void){
    Chopaan env; memset(&env, 0, sizeof(env));
    env.rng = 12345u;
    allocate_Chopaan(&env);
    c_reset(&env);
    long N = 5000000;
    double ret = 0.0;
    struct timespec t0,t1; clock_gettime(CLOCK_MONOTONIC,&t0);
    for (long i=0;i<N;i++){
        env.actions[0] = 2.0f*((float)ch_uni(&env.rng))-1.0f;
        env.actions[1] = 2.0f*((float)ch_uni(&env.rng))-1.0f;
        c_step(&env);
        ret += env.rewards[0];
    }
    clock_gettime(CLOCK_MONOTONIC,&t1);
    double sec = (t1.tv_sec-t0.tv_sec)+(t1.tv_nsec-t0.tv_nsec)/1e9;
    printf("%ld steps in %.3f s  =>  %.2f M steps/sec\n", N, sec, N/sec/1e6);
    printf("mean reward/step = %.5f ; last gridImport = %.3f\n", ret/N, env.grid_import);
    free_allocated(&env);
    return 0;
}
