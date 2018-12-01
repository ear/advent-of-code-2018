#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

#define MAXINPUT 1024
struct input
{
	int64_t changes[MAXINPUT];
	size_t n;
};

#define MAXFREQ 1000000
// index mapping to the interval [-MAXFREQ/2, MAXFREQ/2 - 1]
#define I(n) (n + (MAXFREQ/2))
struct freqs
{
	int64_t seen[MAXFREQ];
};

struct ans
{
	bool found;
	int64_t freq;
};

struct s
{
	struct input in;
	struct freqs fs;
	struct ans   ans;
};

size_t
parse_input(struct input *);

int64_t
resulting_frequency(struct input *);

bool
find_repeat(struct s *);

int
main (void)
{
	struct s s = { 0 };

	// struct s s = { {{0},0}, {{0}}, {0,0} };

	// struct s s;
	// memset((void *)&s, 0, sizeof(struct s));

	printf("# changes: %zu\n", parse_input(&s.in));
	printf("resulting freq: %lld\n", resulting_frequency(&s.in));
	if (find_repeat(&s))
		printf("first repeat: %lld\n", s.ans.freq);

	return 0;
}

size_t
parse_input(struct input * in)
{
	while (scanf("%lli", &in->changes[in->n]) == 1)
		++in->n;
	return in->n;
}

int64_t
resulting_frequency(struct input * in)
{
	int64_t * changes = &in->changes[0];
	int64_t f = 0;
	while (changes != &in->changes[in->n])
		f += *changes++;
	return f;
}

bool
find_repeat(struct s * s)
{
	int64_t *    changes = &s->in.changes[0];
	int64_t *    seen    = &s->fs.seen[0];
	struct ans * ans     = &s->ans;

	int64_t f = 0;
	for (size_t cycles = 1; cycles < 1000; ++cycles)
	{
	for (size_t i = 0; i < s->in.n; ++i)
	{
		f += *(changes + i);
		assert(I(f) >= 0 && I(f) < MAXFREQ);
		if (seen[I(f)]) {
			ans->found = true;
			ans->freq = f;
			goto end;
		}
		seen[I(f)] = true;
	}
	}
end:
	return ans->found;
}

