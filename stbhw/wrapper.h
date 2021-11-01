#include <stdint.h>

#define u8 uint8_t
#define u32 uint32_t

typedef u8 xs_seed[16];

int is_empty(xs_seed seed) {
    for (int i = 0; i < 16; i += 1) {
        if (seed[i]) {
            return 0;
        }
    }

    return 1;
}

typedef u32 xs_rng[4];

#include <stdio.h> // Just for debugging

u32 xs_xorshift(xs_rng *rng) {
    u32 t = (*rng)[3];

    (*rng)[3] = (*rng)[2];
    (*rng)[2] = (*rng)[1];
    (*rng)[1] = (*rng)[0];

    t ^= t << 11;
    t ^= t >> 8;
    (*rng)[0] = t ^ (*rng)[0] ^ ((*rng)[0] >> 19);

    return (*rng)[0];
}

u32 xs_u32(xs_rng *rng, u32 min, u32 one_past_max) {
    return (xs_xorshift(rng) % (one_past_max - min)) + min;
}

#define XS_BAD_SEED_RNG {0x0BAD5EED, 0, 0, 0}

void xs_from_seed(xs_seed seed, xs_rng *rng) {
    // 0 doesn't work as a seed, so use this one instead.
    if (is_empty(seed)) {
        xs_rng bad = XS_BAD_SEED_RNG;
        *rng[0] = bad[0];
        *rng[1] = bad[1];
        *rng[2] = bad[2];
        *rng[3] = bad[3];
        return;
    }

    (*rng)[0] = (seed[ 0]) | (seed[ 1] << 8) | (seed[ 2] << 16) | (seed[ 3] << 24);
    (*rng)[1] = (seed[ 4]) | (seed[ 5] << 8) | (seed[ 6] << 16) | (seed[ 7] << 24);
    (*rng)[2] = (seed[ 8]) | (seed[ 9] << 8) | (seed[10] << 16) | (seed[11] << 24);
    (*rng)[3] = (seed[12]) | (seed[13] << 8) | (seed[14] << 16) | (seed[15] << 24);
}

static xs_rng global_rng = XS_BAD_SEED_RNG;

void xs_seed_global(xs_seed seed) {
    xs_from_seed(seed, &global_rng);
}

u32 xs_global_xorshift() {
    return xs_xorshift(&global_rng);
}

u32 xs_global_u32(u32 min, u32 one_past_max) {
    return (xs_global_xorshift() % (one_past_max - min)) + min;
}

#define STB_HBWANG_RAND() (xs_global_xorshift())

#define STB_HERRINGBONE_WANG_TILE_IMPLEMENTATION
#include "stb_herringbone_wang_tile.h"
