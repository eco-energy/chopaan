// Chopaan grid-dispatch PufferLib ocean env — size-generic.
//
// Physics = the categorified Hopfield kernel (hopfield_step.c), whose dimension
// N is NOT hardcoded here: it is MGENV_N, defined in mgenv_grids.h, which
// build_kernel.sh derives from the max feeder mgenv actually sampled (and the
// kernel's F.hs is codegen'd to the same N). Change mgenv's size distribution
// and everything — kernel, grid table, this env — follows.

#ifndef CHOPAAN_ENV_H
#define CHOPAAN_ENV_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include "hopfield_step.h"   // categorified kernel (input_double[IN_DIM] -> output_double[OUT_DIM])
#include "mgenv_grids.h"     // MGENV_N, MGENV_E, MGENV_GRIDS[MGENV_NUM_GRIDS]

#define CH_HOURS 24
#define CH_OBS   6
#define CH_ACT   2
// kernel input/output layout for size N (matches gen_fhs.py field order)
#define IN_DIM  (MGENV_N*MGENV_E + MGENV_N + MGENV_N + MGENV_E + 2*MGENV_E + 1)
#define OUT_DIM (2*MGENV_E + 1)

typedef struct { float perf, score, episode_return, episode_length, n; } Log;

typedef struct {
    Log log;
    float* observations;   // [CH_OBS]
    float* actions;        // [CH_ACT] continuous [p_set, q_set] in [-1,1]
    float* rewards;        // [1]
    unsigned char* terminals;
    unsigned char* truncations;

    int    grid_id;                        // which mgenv feeder
    int    n_active;                       // real nodes of this feeder (<= MGENV_N)
    double B[MGENV_N][MGENV_E];
    double g[MGENV_E];
    double p_load_base[MGENV_N];
    double p_rated;                        // PV rating at node 1

    int hour;
    unsigned int rng;
    double xflow[2*MGENV_E];               // persisted edge flows
    double p_load[MGENV_N];
    double grid_import;
} Chopaan;

static inline unsigned int ch_xs(unsigned int* s){ unsigned int x=*s; x^=x<<13; x^=x>>17; x^=x<<5; *s=x?x:1u; return *s; }
static inline double ch_uni(unsigned int* s){ return (double)ch_xs(s)/4294967296.0; }
static inline double ch_solar(int h,double r){ if(h<6||h>18)return 0; double x=(h-12.0)/6.0,y=1-x*x; return y>0?r*y:0; }
static inline double ch_loadcurve(int h,double b){ double dm=h-8,de=h-19; double m=0.6+0.4*exp(-dm*dm*0.25),e=0.7+0.5*exp(-de*de/3.0); return b*(m>e?m:e); }
static inline double ch_tariff(int h){ return (h>=18&&h<=22)?30.0:15.0; }

static Chopaan* allocate_Chopaan(Chopaan* env){
    env->observations=(float*)calloc(CH_OBS,sizeof(float));
    env->actions=(float*)calloc(CH_ACT,sizeof(float));
    env->rewards=(float*)calloc(1,sizeof(float));
    env->terminals=(unsigned char*)calloc(1,sizeof(unsigned char));
    env->truncations=(unsigned char*)calloc(1,sizeof(unsigned char));
    return env;
}
static void free_allocated(Chopaan* env){
    free(env->observations);free(env->actions);free(env->rewards);free(env->terminals);free(env->truncations);
}

static void ch_load_grid(Chopaan* env,int gid){
    const MgenvGrid* G=&MGENV_GRIDS[((gid%MGENV_NUM_GRIDS)+MGENV_NUM_GRIDS)%MGENV_NUM_GRIDS];
    memcpy(env->B,G->B,sizeof(env->B));
    memcpy(env->g,G->g,sizeof(env->g));
    memcpy(env->p_load_base,G->p_load_base,sizeof(env->p_load_base));
    env->p_rated=G->p_rated; env->n_active=G->n_active;
}

static void add_log(Chopaan* env){
    env->log.episode_return+=env->rewards[0]; env->log.score+=(float)env->grid_import;
    env->log.episode_length+=1; env->log.perf+=env->rewards[0]; env->log.n+=1;
}

static void ch_write_obs(Chopaan* env){
    float* o=env->observations;
    double tl=0,tb=0; for(int n=1;n<env->n_active;n++){ tl+=env->p_load[n]; tb+=env->p_load_base[n]; }
    o[0]=env->hour/(float)CH_HOURS;
    o[1]=(float)(ch_solar(env->hour,env->p_rated)/(env->p_rated+1e-9));
    o[2]=(float)(tl/(tb+1e-9));
    o[3]=(float)(env->grid_import/100.0);
    o[4]=(float)ch_tariff(env->hour)/30.0f;
    o[5]=(float)env->n_active/(float)MGENV_N;
}

static void c_reset(Chopaan* env){
    ch_load_grid(env,env->grid_id);
    env->hour=0; env->grid_import=0; memset(env->xflow,0,sizeof(env->xflow));
    for(int n=0;n<MGENV_N;n++) env->p_load[n]=env->p_load_base[n];
    env->terminals[0]=0; env->rewards[0]=0; ch_write_obs(env);
}

// Pack the N-sized kernel input (gen_fhs.py field order), settle, price.
static void c_step(Chopaan* env){
    env->hour+=1; env->terminals[0]=0; env->rewards[0]=0;
    float a0=env->actions[0],a1=env->actions[1];
    if(a0<-1)a0=-1; if(a0>1)a0=1; if(a1<-1)a1=-1; if(a1>1)a1=1;
    double solar=ch_solar(env->hour,env->p_rated);
    double p_set=(a0+1.0)*0.5*env->p_rated; if(p_set>solar)p_set=solar;
    double q_set=a1*0.5*env->p_rated;

    double injP[MGENV_N], injQ[MGENV_N];
    for(int n=0;n<MGENV_N;n++){
        if(n==0 || n>=env->n_active) injP[n]=0;              // slack + padding
        else if(n==1) injP[n]=p_set - env->p_load[1];         // PV node
        else injP[n]=-env->p_load[n];                          // load
        injQ[n]=(n==1)? (q_set-0.3*env->p_load[1]) : 0.3*injP[n];
    }

    double in[IN_DIM]; int k=0;
    for(int n=0;n<MGENV_N;n++) for(int e=0;e<MGENV_E;e++) in[k++]=env->B[n][e];
    for(int n=0;n<MGENV_N;n++) in[k++]=injP[n];
    for(int n=0;n<MGENV_N;n++) in[k++]=injQ[n];
    for(int e=0;e<MGENV_E;e++) in[k++]=env->g[e];
    for(int i=0;i<2*MGENV_E;i++) in[k++]=env->xflow[i];
    in[k++]=0.5; // alpha

    double out[OUT_DIM];
    hopfield_step(0,0,0,0,0,0,0,0,0,0,in, 0,0,0,0,0,0,0,0,0,0,out);
    for(int i=0;i<2*MGENV_E;i++) env->xflow[i]=out[i];
    env->grid_import=out[2*MGENV_E];   // gridImport is the last output

    double price=ch_tariff(env->hour);
    double cost=(env->grid_import>0)? env->grid_import*price : 0.5*env->grid_import*price;
    double vproxy=fabs(env->grid_import)/100.0;
    double vv=vproxy>0.05?(vproxy-0.05):0.0;
    env->rewards[0]=(float)(-(cost/100.0 + 10.0*vv));

    if(env->hour>=CH_HOURS){ env->terminals[0]=1; add_log(env); c_reset(env); return; }
    for(int n=1;n<env->n_active;n++){
        double noise=1.0+0.05*(ch_uni(&env->rng)-0.5);
        env->p_load[n]=ch_loadcurve(env->hour,env->p_load_base[n])*noise;
    }
    ch_write_obs(env);
}

static void c_render(Chopaan* env){ (void)env; }
static void c_close(Chopaan* env){ (void)env; }

#endif // CHOPAAN_ENV_H
