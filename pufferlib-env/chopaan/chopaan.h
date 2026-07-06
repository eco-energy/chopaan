// Chopaan grid-dispatch PufferLib ocean env.
//
// Physics = the categorified Hopfield kernel generated from chopaan's
// HopfieldDynamics via Categorifier (hopfield_step.c). Each env step packs the
// grid (incidence matrix + conductances) and the action-driven node injections
// into the kernel's input_double[30], settles the edge flows, and prices the
// resulting grid import. The graph is mgenv's output; the dynamics are the
// lowered categorical Hopfield morphism.

#ifndef CHOPAAN_ENV_H
#define CHOPAAN_ENV_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include "hopfield_step.h"   // categorified kernel: void hopfield_step(...double in[30]...double out[7])
#include "mgenv_grids.h"     // real mgenv 4-node feeders (MGENV_GRIDS[MGENV_NUM_GRIDS])

#define CH_HOURS 24
#define CH_NODES 4
#define CH_EDGES 3
#define CH_OBS 6
#define CH_ACT 2

typedef struct {
    float perf;
    float score;
    float episode_return;
    float episode_length;
    float n;
} Log;

typedef struct {
    Log log;
    float* observations;   // [CH_OBS]
    float* actions;        // continuous [p_set, q_set] in [-1,1] (Box float32)
    float* rewards;        // [1]
    unsigned char* terminals;
    unsigned char* truncations;

    // grid topology (mgenv): directed incidence B[node][edge], conductances g
    double B[CH_NODES][CH_EDGES];
    double g[CH_EDGES];
    double p_load_base[CH_NODES];  // baseline real load per node (node0 slack=0)
    double p_rated;                // PV rating at the PV node (node1)

    // running state
    int hour;
    int grid_id;                   // which mgenv feeder (index into MGENV_GRIDS)
    unsigned int rng;
    double xflow[2*CH_EDGES];      // persisted edge flows (P,Q) across the settle
    double p_load[CH_NODES];
    double grid_import;
} Chopaan;

// --- small deterministic RNG -------------------------------------------------
static inline unsigned int ch_xs(unsigned int* s){
    unsigned int x=*s; x^=x<<13; x^=x>>17; x^=x<<5; *s=x?x:1u; return *s; }
static inline double ch_uni(unsigned int* s){ return (double)ch_xs(s)/4294967296.0; }

static inline double ch_solar(int h, double rated){
    if (h<6||h>18) return 0.0; double x=(h-12.0)/6.0; double y=1.0-x*x;
    return y>0?rated*y:0.0; }
static inline double ch_loadcurve(int h, double base){
    double de=(double)(h-19), dm=(double)(h-8);
    double m=0.6+0.4*exp(-dm*dm*0.25), e=0.7+0.5*exp(-de*de/3.0);
    return base*(m>e?m:e); }
static inline double ch_tariff(int h){ return (h>=18&&h<=22)?30.0:15.0; }

// --- lifecycle ---------------------------------------------------------------
static Chopaan* allocate_Chopaan(Chopaan* env){
    env->observations = (float*)calloc(CH_OBS,sizeof(float));
    env->actions = (float*)calloc(CH_ACT,sizeof(float));
    env->rewards = (float*)calloc(1,sizeof(float));
    env->terminals = (unsigned char*)calloc(1,sizeof(unsigned char));
    env->truncations = (unsigned char*)calloc(1,sizeof(unsigned char));
    return env;
}
static void free_allocated(Chopaan* env){
    free(env->observations); free(env->actions); free(env->rewards);
    free(env->terminals); free(env->truncations);
}

// Load one of mgenv's sampled 4-node feeders into this env.
static void ch_load_grid(Chopaan* env, int gid){
    const MgenvGrid* G = &MGENV_GRIDS[((gid % MGENV_NUM_GRIDS) + MGENV_NUM_GRIDS) % MGENV_NUM_GRIDS];
    memcpy(env->B, G->B, sizeof(env->B));
    memcpy(env->g, G->g, sizeof(env->g));
    memcpy(env->p_load_base, G->p_load_base, sizeof(env->p_load_base));
    env->p_rated = G->p_rated;
}

// Fallback synthetic feeder (unused once mgenv grids are loaded).
static void ch_default_grid(Chopaan* env){
    // e0:0->1, e1:1->2, e2:1->3
    double B[CH_NODES][CH_EDGES] = {{1,0,0},{-1,1,1},{0,-1,0},{0,0,-1}};
    memcpy(env->B, B, sizeof(B));
    env->g[0]=5.0; env->g[1]=3.0; env->g[2]=3.0;
    env->p_load_base[0]=0.0; env->p_load_base[1]=20.0;
    env->p_load_base[2]=30.0; env->p_load_base[3]=25.0;
    env->p_rated = 60.0;
}

static void add_log(Chopaan* env){
    env->log.episode_return += env->rewards[0];
    env->log.score += (float)env->grid_import;
    env->log.episode_length += 1;
    env->log.perf += env->rewards[0];
    env->log.n += 1;
}

static void ch_write_obs(Chopaan* env){
    float* o = env->observations;
    o[0] = env->hour/(float)CH_HOURS;
    o[1] = (float)(ch_solar(env->hour, env->p_rated)/env->p_rated);
    o[2] = (float)(env->p_load[2]/(env->p_load_base[2]+1e-9));
    o[3] = (float)(env->p_load[3]/(env->p_load_base[3]+1e-9));
    o[4] = (float)(env->grid_import/100.0);
    o[5] = (float)ch_tariff(env->hour)/30.0f;
}

static void c_reset(Chopaan* env){
    ch_load_grid(env, env->grid_id);   // this env trains on mgenv feeder grid_id
    env->hour = 0;
    env->grid_import = 0.0;
    memset(env->xflow, 0, sizeof(env->xflow));
    for (int n=0;n<CH_NODES;n++) env->p_load[n] = env->p_load_base[n];
    env->terminals[0]=0; env->rewards[0]=0;
    ch_write_obs(env);
}

// Pack the kernel input, settle, price. This is the hot loop.
static void c_step(Chopaan* env){
    env->hour += 1;
    env->terminals[0]=0; env->rewards[0]=0;

    // decode action -> PV setpoint at node 1 (curtailed by available solar)
    float a0 = env->actions[0], a1 = env->actions[1];
    if (a0<-1)a0=-1; if(a0>1)a0=1; if(a1<-1)a1=-1; if(a1>1)a1=1;
    double solar = ch_solar(env->hour, env->p_rated);
    double p_set = (a0+1.0)*0.5*env->p_rated; if (p_set>solar) p_set=solar;
    double q_set = a1*0.5*env->p_rated;

    // node injections: slack 0 free, PV node1 = p_set - load1, loads negative
    double injP[CH_NODES], injQ[CH_NODES];
    injP[0]=0; injQ[0]=0;
    injP[1]= p_set - env->p_load[1];     injQ[1]= q_set - 0.3*env->p_load[1];
    injP[2]= -env->p_load[2];            injQ[2]= -0.3*env->p_load[2];
    injP[3]= -env->p_load[3];            injQ[3]= -0.3*env->p_load[3];

    // pack input_double[30] in F.hs field order
    double in[30]; int k=0;
    for (int n=0;n<CH_NODES;n++) for (int e=0;e<CH_EDGES;e++) in[k++]=env->B[n][e];
    for (int n=0;n<CH_NODES;n++) in[k++]=injP[n];
    for (int n=0;n<CH_NODES;n++) in[k++]=injQ[n];
    for (int e=0;e<CH_EDGES;e++) in[k++]=env->g[e];
    for (int i=0;i<2*CH_EDGES;i++) in[k++]=env->xflow[i];
    in[k++]=0.5; // alpha

    double out[7];
    hopfield_step(0,0,0,0,0,0,0,0,0,0,in, 0,0,0,0,0,0,0,0,0,0,out);
    for (int i=0;i<2*CH_EDGES;i++) env->xflow[i]=out[i];   // persist settled flows
    env->grid_import = out[6];

    // reward: -(import cost + voltage-violation proxy)
    double price = ch_tariff(env->hour);
    double cost = (env->grid_import>0)? env->grid_import*price : 0.5*env->grid_import*price;
    double vproxy = fabs(env->grid_import)/100.0;   // surrogate sag
    double vv = vproxy>0.05 ? (vproxy-0.05) : 0.0;
    env->rewards[0] = (float)(-(cost/100.0 + 10.0*vv));

    if (env->hour >= CH_HOURS){
        env->terminals[0]=1; add_log(env);
        c_reset(env);
        return;
    }
    // advance loads
    for (int n=1;n<CH_NODES;n++){
        double noise = 1.0 + 0.05*(ch_uni(&env->rng)-0.5);
        env->p_load[n] = ch_loadcurve(env->hour, env->p_load_base[n])*noise;
    }
    ch_write_obs(env);
}

static void c_render(Chopaan* env){ (void)env; }
static void c_close(Chopaan* env){ (void)env; }

#endif // CHOPAAN_ENV_H
