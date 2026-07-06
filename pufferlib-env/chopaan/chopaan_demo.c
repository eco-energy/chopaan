// Native demo: run several envs, each on a DISTINCT mgenv feeder, at speed.
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "chopaan.h"
#include "hopfield_step.c"

static double run_env(int grid_id, long steps, unsigned seed){
    Chopaan env; memset(&env,0,sizeof(env));
    env.grid_id = grid_id; env.rng = seed;
    allocate_Chopaan(&env); c_reset(&env);
    double ret=0;
    for(long i=0;i<steps;i++){
        env.actions[0]=2.0f*((float)ch_uni(&env.rng))-1.0f;
        env.actions[1]=2.0f*((float)ch_uni(&env.rng))-1.0f;
        c_step(&env); ret+=env.rewards[0];
    }
    double gi=env.grid_import; free_allocated(&env);
    printf("grid %2d: mean_reward=%.5f  last_gridImport=%.4f\n", grid_id, ret/steps, gi);
    return gi;
}

int main(void){
    printf("=== distinct mgenv feeders (MGENV_NUM_GRIDS=%d) ===\n", MGENV_NUM_GRIDS);
    for(int g=0; g<6; g++) run_env(g, 100000, 1u+g);

    // throughput across all grids round-robin
    long N=5000000; Chopaan e; memset(&e,0,sizeof(e)); e.rng=7; allocate_Chopaan(&e);
    struct timespec t0,t1; clock_gettime(CLOCK_MONOTONIC,&t0);
    for(long i=0;i<N;i++){
        e.grid_id=(int)(i%MGENV_NUM_GRIDS);
        e.actions[0]=2.0f*((float)ch_uni(&e.rng))-1.0f; e.actions[1]=2.0f*((float)ch_uni(&e.rng))-1.0f;
        c_step(&e);
    }
    clock_gettime(CLOCK_MONOTONIC,&t1);
    double sec=(t1.tv_sec-t0.tv_sec)+(t1.tv_nsec-t0.tv_nsec)/1e9;
    printf("throughput: %ld steps / %.3fs = %.2f M steps/sec (rotating mgenv grids)\n", N, sec, N/sec/1e6);
    free_allocated(&e); return 0;
}
