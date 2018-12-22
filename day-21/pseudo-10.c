int main (void)
{
    int r0 = 0; // the parameter to find
    int r2 = 0;
    int r3 = 0;
    int r4 = 0;
    int r5 = 0;
AA: r4 = r5 | 65536;
    r5 = 13284195;
BB: r3 = r4 & 255;
    r5 = (65899 * (r5 + r3)) % (2^24 - 1);
    if (r4 >= 256) {
        r3 = 0;
        while (256 * (r3+1) <= r4) { r3++; }
        r4 = r3;
        goto BB;
    }
    if (r5 == r0) { return 0; }
    else          { goto AA; }
}
