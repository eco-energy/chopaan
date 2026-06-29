// Vectorized chopaan dispatch env — SoA layout, batch step in one C call.
//
// Design for millions of steps/sec:
//   - Struct-of-arrays so each per-field loop is contiguous and auto-vectorizable.
//   - One foreign call per batch (not per env): step(n_envs) crosses Python->C exactly once.
//   - Caller-owned numpy buffers (no copies; ctypes hands raw pointers).
//   - Auto-reset on terminal inside the C loop — no Python branch per env.
//   - Zero malloc in the hot path; xorshift32 RNG inline.

#ifndef CHOPAAN_VEC_H
#define CHOPAAN_VEC_H

#include <math.h>
#include <stdint.h>
#include <stdlib.h>

#define CV_HOURS 24
#define CV_N_NODES 4
#define CV_N_LOADS 2
#define CV_OBS_DIM 10  // 2 + 2*N_LOADS + N_NODES
#define CV_ACT_DIM 2

static const float CV_P_RATED = 60.0f;
static const float CV_Q_RATED = 30.0f;
static const float CV_P_BASE[CV_N_LOADS] = {30.0f, 25.0f};
static const float CV_PF = 0.3f;

// Pure SoA. All arrays sized to n_envs (or n_envs * dim where noted).
typedef struct ChopaanVec {
    int n_envs;

    // Caller-owned buffers (typically slices of a contiguous numpy array).
    float* observations;  // [n_envs * CV_OBS_DIM]
    float* actions;       // [n_envs * CV_ACT_DIM]
    float* rewards;       // [n_envs]
    uint8_t* terminals;   // [n_envs]

    // Internal state (owned by allocate_state).
    uint32_t* rng;        // [n_envs]
    int32_t* hour;        // [n_envs]
    float* p_solar;       // [n_envs]
    float* p_load;        // [n_envs * CV_N_LOADS]
    float* q_load;        // [n_envs * CV_N_LOADS]
    float* v_last;        // [n_envs]   (scalar voltage broadcast at obs time)

    // Episode logging.
    float* ep_return;     // [n_envs]
    float* ep_cost;       // [n_envs]
    float* last_return;   // [n_envs]   (filled when an env terminates)
    float* last_cost;     // [n_envs]
} ChopaanVec;

// ------------------------------------------------------------------ utilities

static inline uint32_t cv_xs(uint32_t* s) {
    uint32_t x = *s; x ^= x << 13; x ^= x >> 17; x ^= x << 5;
    *s = x ? x : 1u; return *s;
}
static inline float cv_uniform(uint32_t* s) {
    return (float)cv_xs(s) * (1.0f / 4294967296.0f);
}
static inline float cv_normal(uint32_t* s) {
    float u1 = cv_uniform(s), u2 = cv_uniform(s);
    if (u1 < 1e-7f) u1 = 1e-7f;
    return sqrtf(-2.0f * logf(u1)) * cosf(6.2831853f * u2);
}
static inline float cv_solar(int h) {
    if (h < 6 || h > 18) return 0.0f;
    float x = (h - 12.0f) / 6.0f;
    float y = 1.0f - x * x;
    return y > 0.0f ? CV_P_RATED * y : 0.0f;
}
static inline float cv_load(int h, float base) {
    float dm = (float)(h - 8), de = (float)(h - 19);
    float m = 0.6f + 0.4f * expf(-dm * dm * 0.25f);
    float e = 0.7f + 0.5f * expf(-de * de * (1.0f/3.0f));
    return base * (m > e ? m : e);
}
static inline float cv_tariff(int h) { return (h >= 18 && h <= 22) ? 30.0f : 15.0f; }
static inline float cv_clip(float x, float lo, float hi) { return x < lo ? lo : (x > hi ? hi : x); }

// Resample loads at the start of an episode or hour rollover for env i.
static inline void cv_resample(ChopaanVec* v, int i, int h) {
    float* pl = v->p_load + i * CV_N_LOADS;
    float* ql = v->q_load + i * CV_N_LOADS;
    uint32_t* rs = &v->rng[i];
    for (int k = 0; k < CV_N_LOADS; k++) {
        float noise = 1.0f + 0.05f * cv_normal(rs);
        pl[k] = (h == 0 ? CV_P_BASE[k] : cv_load(h, CV_P_BASE[k])) * noise;
        ql[k] = CV_PF * pl[k];
    }
}

// Write observation for env i (called inline at end of step/reset).
static inline void cv_write_obs(ChopaanVec* v, int i) {
    float* o = v->observations + i * CV_OBS_DIM;
    int h = v->hour[i];
    o[0] = h / (float)CV_HOURS;
    o[1] = v->p_solar[i] / CV_P_RATED;
    const float* pl = v->p_load + i * CV_N_LOADS;
    const float* ql = v->q_load + i * CV_N_LOADS;
    for (int k = 0; k < CV_N_LOADS; k++) {
        o[2 + k] = pl[k] / CV_P_BASE[k];
        o[2 + CV_N_LOADS + k] = ql[k] / (CV_PF * CV_P_BASE[k]);
    }
    // v_last is scalar in this model — broadcast across nodes.
    for (int k = 0; k < CV_N_NODES; k++) {
        o[2 + 2 * CV_N_LOADS + k] = v->v_last[i];
    }
}

// ----------------------------------------------------------------- lifecycle

static inline void cv_allocate_state(ChopaanVec* v) {
    int n = v->n_envs;
    v->rng        = (uint32_t*)calloc(n, sizeof(uint32_t));
    v->hour       = (int32_t*)calloc(n, sizeof(int32_t));
    v->p_solar    = (float*)calloc(n, sizeof(float));
    v->p_load     = (float*)calloc(n * CV_N_LOADS, sizeof(float));
    v->q_load     = (float*)calloc(n * CV_N_LOADS, sizeof(float));
    v->v_last     = (float*)calloc(n, sizeof(float));
    v->ep_return  = (float*)calloc(n, sizeof(float));
    v->ep_cost    = (float*)calloc(n, sizeof(float));
    v->last_return= (float*)calloc(n, sizeof(float));
    v->last_cost  = (float*)calloc(n, sizeof(float));
}

static inline void cv_free_state(ChopaanVec* v) {
    free(v->rng); free(v->hour); free(v->p_solar);
    free(v->p_load); free(v->q_load); free(v->v_last);
    free(v->ep_return); free(v->ep_cost); free(v->last_return); free(v->last_cost);
}

static inline void cv_reset_one(ChopaanVec* v, int i) {
    v->hour[i] = 0;
    v->p_solar[i] = 0.0f;
    v->v_last[i] = 1.0f;
    v->ep_return[i] = 0.0f;
    v->ep_cost[i] = 0.0f;
    cv_resample(v, i, 0);
    v->terminals[i] = 0;
    v->rewards[i] = 0.0f;
    cv_write_obs(v, i);
}

static inline void cv_reset_all(ChopaanVec* v, uint64_t seed) {
    for (int i = 0; i < v->n_envs; i++) {
        v->rng[i] = (uint32_t)((seed + (uint64_t)i * 2654435761ULL) | 1u);
        cv_reset_one(v, i);
    }
}

// ------------------------------------------------------------------- the loop

// Single foreign call per timestep. Auto-resets terminated envs so the
// rollout loop never needs to branch back to Python.
static inline void cv_step(ChopaanVec* v) {
    const int n = v->n_envs;
    for (int i = 0; i < n; i++) {
        // Decode action.
        const float* a = v->actions + i * CV_ACT_DIM;
        float a0 = cv_clip(a[0], -1.0f, 1.0f);
        float a1 = cv_clip(a[1], -1.0f, 1.0f);
        float p_set = (a0 + 1.0f) * 0.5f * CV_P_RATED;
        float q_set = a1 * CV_Q_RATED;
        if (p_set > v->p_solar[i]) p_set = v->p_solar[i];

        // Aggregate balance (single-bus surrogate of full AC-OPF).
        const float* pl = v->p_load + i * CV_N_LOADS;
        const float* ql = v->q_load + i * CV_N_LOADS;
        float tp = pl[0] + pl[1];
        float tq = ql[0] + ql[1];
        float gp = tp - p_set;
        float gq = tq - q_set;

        // Crude voltage proxy.
        float smag = sqrtf(gp * gp + gq * gq);
        float vmag = cv_clip(1.0f - 0.0002f * smag, 0.85f, 1.10f);
        v->v_last[i] = vmag;

        // Reward.
        int h = v->hour[i];
        float price = cv_tariff(h);
        float import_cost = (gp > 0.0f) ? gp * price : 0.5f * gp * price;
        float vv = 0.0f;
        if (vmag < 0.95f) vv += 0.95f - vmag;
        if (vmag > 1.05f) vv += vmag - 1.05f;
        vv *= (float)CV_N_NODES;
        float r = -(import_cost / 100.0f + 10.0f * vv);

        v->rewards[i] = r;
        v->ep_return[i] += r;
        v->ep_cost[i] += import_cost;

        // Advance.
        int next_h = h + 1;
        if (next_h >= CV_HOURS) {
            v->terminals[i] = 1;
            v->last_return[i] = v->ep_return[i];
            v->last_cost[i] = v->ep_cost[i];
            cv_reset_one(v, i);  // auto-reset: keep the C loop hot
        } else {
            v->terminals[i] = 0;
            v->hour[i] = next_h;
            v->p_solar[i] = cv_solar(next_h);
            cv_resample(v, i, next_h);
            cv_write_obs(v, i);
        }
    }
}

#endif  // CHOPAAN_VEC_H
