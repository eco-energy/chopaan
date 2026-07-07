#include "chopaan.h"
#include "hopfield_step.c"   // categorified Hopfield kernel (definition) into this TU

#define Env Chopaan
#include "../env_binding.h"

static int my_init(Env* env, PyObject* args, PyObject* kwargs) {
    // Topology defaults to the 4-node radial test feeder (see ch_default_grid).
    // A future my_put can stream an mgenv grid (incidence B + conductances g).
    (void)args;
    int seed = (int)unpack(kwargs, "seed");
    env->grid_id = seed;        // each env trains on a distinct mgenv feeder
    env->rng = 12345u + (unsigned)seed;
    return 0;
}

static int my_log(PyObject* dict, Log* log) {
    assign_to_dict(dict, "perf", log->perf);
    assign_to_dict(dict, "score", log->score);
    assign_to_dict(dict, "episode_return", log->episode_return);
    assign_to_dict(dict, "episode_length", log->episode_length);
    return 0;
}
