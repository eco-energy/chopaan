// Fast PufferLib-style native environment for chopaan grid dispatch.
//
// Single-file C header. Mirrors the Python `ChopaanDispatchEnv`:
//   4-node grid (slack, PV, 2 loads), 24-hour episode, continuous P/Q action,
//   reward = -(grid_import_cost/100 + 10*voltage_violation).
//
// Layout follows ocean envs:
//   typedef struct ChopaanEnv with externally-owned observation/action/reward
//   buffers, plus init/allocate/free_allocated/reset/step/close entry points.

#ifndef CHOPAAN_ENV_H
#define CHOPAAN_ENV_H

#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define CHOPAAN_HOURS 24
#define CHOPAAN_N_NODES 4
#define CHOPAAN_N_LOADS 2
#define CHOPAAN_OBS_DIM (2 + 2 * CHOPAAN_N_LOADS + CHOPAAN_N_NODES)  // = 10
#define CHOPAAN_ACT_DIM 2

// Grid constants from summing_functor_net.create_test_grid().
static const float CHOPAAN_P_RATED = 60.0f;
static const float CHOPAAN_Q_RATED = 30.0f;
static const float CHOPAAN_P_LOAD_BASE[CHOPAAN_N_LOADS] = {30.0f, 25.0f};
static const float CHOPAAN_PF = 0.3f;  // Q = 0.3 * P

typedef struct ChopaanEnv {
    // Buffers owned by the caller (or by allocate()).
    float* observations;   // [obs_dim]
    float* actions;        // [act_dim]
    float* rewards;        // [1]
    uint8_t* terminals;    // [1]

    // Per-env state.
    uint32_t rng_state;
    int hour;
    float p_solar_avail;
    float p_load[CHOPAAN_N_LOADS];
    float q_load[CHOPAAN_N_LOADS];
    float v_last[CHOPAAN_N_NODES];

    // Logging (read by Python).
    float episode_return;
    float episode_cost_pkr;
} ChopaanEnv;

// xorshift32 — fast deterministic RNG, no libc dependency.
static inline uint32_t chopaan_xorshift32(uint32_t* s) {
    uint32_t x = *s;
    x ^= x << 13; x ^= x >> 17; x ^= x << 5;
    *s = x ? x : 1u;
    return *s;
}

static inline float chopaan_uniform(uint32_t* s) {
    return (float)chopaan_xorshift32(s) / 4294967296.0f;
}

// Box-Muller for Gaussian N(0,1); discards the second sample for simplicity.
static inline float chopaan_normal(uint32_t* s) {
    float u1 = chopaan_uniform(s);
    float u2 = chopaan_uniform(s);
    if (u1 < 1e-7f) u1 = 1e-7f;
    return sqrtf(-2.0f * logf(u1)) * cosf(6.2831853f * u2);
}

static inline float chopaan_solar_curve(int hour) {
    if (hour < 6 || hour > 18) return 0.0f;
    float x = (hour - 12.0f) / 6.0f;
    float y = 1.0f - x * x;
    return y > 0.0f ? CHOPAAN_P_RATED * y : 0.0f;
}

static inline float chopaan_load_curve(int hour, float p_base) {
    float dm = (float)(hour - 8);
    float de = (float)(hour - 19);
    float morning = 0.6f + 0.4f * expf(-dm * dm / 4.0f);
    float evening = 0.7f + 0.5f * expf(-de * de / 3.0f);
    return p_base * (morning > evening ? morning : evening);
}

static inline float chopaan_tariff(int hour) {
    return (hour >= 18 && hour <= 22) ? 30.0f : 15.0f;
}

static inline float chopaan_clip(float x, float lo, float hi) {
    return x < lo ? lo : (x > hi ? hi : x);
}

static inline void chopaan_write_obs(ChopaanEnv* env) {
    float* o = env->observations;
    o[0] = env->hour / (float)CHOPAAN_HOURS;
    o[1] = env->p_solar_avail / CHOPAAN_P_RATED;
    for (int i = 0; i < CHOPAAN_N_LOADS; i++) {
        o[2 + i] = env->p_load[i] / CHOPAAN_P_LOAD_BASE[i];
        o[2 + CHOPAAN_N_LOADS + i] = env->q_load[i] / (CHOPAAN_PF * CHOPAAN_P_LOAD_BASE[i]);
    }
    for (int i = 0; i < CHOPAAN_N_NODES; i++) {
        o[2 + 2 * CHOPAAN_N_LOADS + i] = env->v_last[i];
    }
}

// Lifecycle ----------------------------------------------------------------

static inline void chopaan_init(ChopaanEnv* env, uint32_t seed) {
    env->rng_state = seed ? seed : 1u;
    env->hour = 0;
    env->p_solar_avail = 0.0f;
    env->episode_return = 0.0f;
    env->episode_cost_pkr = 0.0f;
    for (int i = 0; i < CHOPAAN_N_NODES; i++) env->v_last[i] = 1.0f;
    for (int i = 0; i < CHOPAAN_N_LOADS; i++) {
        env->p_load[i] = CHOPAAN_P_LOAD_BASE[i];
        env->q_load[i] = CHOPAAN_PF * CHOPAAN_P_LOAD_BASE[i];
    }
}

static inline void chopaan_allocate(ChopaanEnv* env) {
    env->observations = (float*)calloc(CHOPAAN_OBS_DIM, sizeof(float));
    env->actions = (float*)calloc(CHOPAAN_ACT_DIM, sizeof(float));
    env->rewards = (float*)calloc(1, sizeof(float));
    env->terminals = (uint8_t*)calloc(1, sizeof(uint8_t));
}

static inline void chopaan_free_allocated(ChopaanEnv* env) {
    free(env->observations); env->observations = NULL;
    free(env->actions);      env->actions = NULL;
    free(env->rewards);      env->rewards = NULL;
    free(env->terminals);    env->terminals = NULL;
}

static inline void chopaan_reset(ChopaanEnv* env) {
    env->hour = 0;
    env->p_solar_avail = 0.0f;
    env->episode_return = 0.0f;
    env->episode_cost_pkr = 0.0f;
    for (int i = 0; i < CHOPAAN_N_NODES; i++) env->v_last[i] = 1.0f;
    for (int i = 0; i < CHOPAAN_N_LOADS; i++) {
        float noise = 1.0f + 0.05f * chopaan_normal(&env->rng_state);
        env->p_load[i] = CHOPAAN_P_LOAD_BASE[i] * noise;
        env->q_load[i] = CHOPAAN_PF * env->p_load[i];
    }
    *env->terminals = 0;
    *env->rewards = 0.0f;
    chopaan_write_obs(env);
}

// One environment step: consumes env->actions, writes obs/reward/terminal.
static inline void chopaan_step(ChopaanEnv* env) {
    float a0 = chopaan_clip(env->actions[0], -1.0f, 1.0f);
    float a1 = chopaan_clip(env->actions[1], -1.0f, 1.0f);
    float p_set = (a0 + 1.0f) * 0.5f * CHOPAAN_P_RATED;
    float q_set = a1 * CHOPAAN_Q_RATED;
    if (p_set > env->p_solar_avail) p_set = env->p_solar_avail;

    float total_p = 0.0f, total_q = 0.0f;
    for (int i = 0; i < CHOPAAN_N_LOADS; i++) {
        total_p += env->p_load[i];
        total_q += env->q_load[i];
    }
    float grid_p = total_p - p_set;
    float grid_q = total_q - q_set;

    float s_mag = sqrtf(grid_p * grid_p + grid_q * grid_q);
    float v_drop = 0.02f * s_mag / 100.0f;
    float v = chopaan_clip(1.0f - v_drop, 0.85f, 1.10f);
    for (int i = 0; i < CHOPAAN_N_NODES; i++) env->v_last[i] = v;

    float price = chopaan_tariff(env->hour);
    float import_cost = (grid_p > 0.0f) ? grid_p * price : 0.5f * grid_p * price;
    float v_viol = 0.0f;
    if (v < 0.95f) v_viol += 0.95f - v;
    if (v > 1.05f) v_viol += v - 1.05f;
    v_viol *= (float)CHOPAAN_N_NODES;

    float reward = -(import_cost / 100.0f + 10.0f * v_viol);
    *env->rewards = reward;
    env->episode_return += reward;
    env->episode_cost_pkr += import_cost;

    env->hour++;
    if (env->hour >= CHOPAAN_HOURS) {
        *env->terminals = 1;
    } else {
        *env->terminals = 0;
        env->p_solar_avail = chopaan_solar_curve(env->hour);
        for (int i = 0; i < CHOPAAN_N_LOADS; i++) {
            float noise = 1.0f + 0.05f * chopaan_normal(&env->rng_state);
            env->p_load[i] = chopaan_load_curve(env->hour, CHOPAAN_P_LOAD_BASE[i]) * noise;
            env->q_load[i] = CHOPAAN_PF * env->p_load[i];
        }
    }
    chopaan_write_obs(env);
}

static inline void chopaan_close(ChopaanEnv* env) {
    (void)env;  // nothing else to free besides allocate()'s buffers
}

#endif  // CHOPAAN_ENV_H
