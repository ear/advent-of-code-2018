        r5 = 0
outer:  r4 = r5 | 65536
        r5 = 13284195;
loop:   r3 = r4 & 255;
        r5 = (256 * (r5+r3)) mod ((2^24)-1)
        if (r4 < 256)
        {
            if (r5 == r0)
              exit();
            else
              goto outer;
        }
        else
        {
            for (r3 = 0; r2 <= r4; r3++)
            {
              r2 = 256 * (r3 + 1);
              if (r2 > r4)
              {
                  r4 = r3;
                  goto loop;
              }
            }
        }
