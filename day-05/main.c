#include <stdio.h>
#include <stdbool.h>
#include <strings.h>

char
to_lower (char c)
{
	return c - 'A' + 'a';
}

bool
matches (char a, char b)
{
	return to_lower(a) == b || to_lower(b) == a;
}

int
main (void)
{
	char inbuf[50001] = { 0 };
	char outbuf[50001] = { 0 };

	char * in = inbuf;
	char * out = outbuf;

	scanf("%50000s", in);

	while (*in++)
	{
		if ( (out > outbuf) && matches(*(out-1),*in) )
		{
			*out-- = 0;
		}
		else
		{
			*out++ = *in;
		}
	}

	printf("%lu\n", strlen(outbuf));
}
