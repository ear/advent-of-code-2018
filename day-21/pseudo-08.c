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
    if (256 > r4) { goto II; }
    else          { r3 = 0; }
EE: r2 = 256 * (r3 + 1);
    if (r2 > r4) { goto H; }
    else         { r3++; }
    goto EE;
 H: r4 = r3;
    goto BB;
II: if (r5 == r0) { return 0; }
    else          { goto AA; }
}
