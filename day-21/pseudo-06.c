 5        r5 = 0;
 6  AAAA: r4 = r5 | 65536;
 7        r5 = 13284195;
 8  BBBB: r3 = r4 & 255;
 9        r5 = (65899 * (r5 + r3)) % (2^24 - 1);
13        if (256 > r4) jmp C;
15        else          jmp D;
16     C: jmp IIII;
17     D: r3 = 0;
18  EEEE: r2 = 256 * (r3 + 1);
20        if (r2 > r4) jmp F;
22        else         jmp G;
23     F: jmp H;
24     G: r3++;
25        jmp EEEE;
26     H: r4 = r3;
27        jmp BBBB;
28  IIII: if (r5 == r0) exit;
29        else          jmp AAAA;
