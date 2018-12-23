#include <stdio.h>
#include <stdint.h>
int main (void)
{
    uint64_t r0 = 0; // the parameter to find
    uint64_t r2 = 0;
    uint64_t r3 = 0;
    uint64_t r4 = 0;
    uint64_t r5 = 0;
    uint64_t iters = 0;
AA: r4 = r5 | 65536;
    r5 = 13284195;
BB: iters++;
    r3 = r4 & 255;
    r5 = (65899 * (r5 + r3)) % (2^24 - 1);
    if (r4 >= 256) {
        r3 = 0;
        while (256 * (r3+1) <= r4) { r3++; }
        r4 = r3;
        goto BB;
    }
    if (r5 > 0) { printf("[iters %llu] r5 = %llu\n", iters, r5); }
    else          { goto AA; }
}
