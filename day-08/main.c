#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define META_MAX 100
#define CHILDREN_MAX 100

// Trees

typedef struct Tree Tree;

struct Tree
{
	uint8_t Meta[META_MAX];
	size_t  MetaCount;

	Tree * Children[CHILDREN_MAX];
	size_t ChildrenCount;
};

// This prototype is needed because it is mutually recursive
// with the function AccumulateChildren
int *
BuildTree(int *, Tree *);

// Create an empty Tree (no Meta, no Children)
Tree *
EmptyTree()
{
	Tree * Result = (Tree *)calloc(1, sizeof(Tree));
	return Result;
}

// Add the meta numbers to the Tree
// Returns a pointer after the last parsed number
int *
AccumulateMeta(int * Input, Tree * Tree)
{
	for (size_t i = 0; i < Tree->MetaCount; ++i)
	{
		Tree->Meta[i] = *Input++;
	}
	return Input;
}

// Add all the children described in Input to the given Tree
// Returns a pointer after the last parsed number
int *
AccumulateChildren(int * Input, Tree * Tree)
{
	for (size_t i = 0; i < Tree->ChildrenCount; ++i)
	{
		Tree->Children[i] = EmptyTree();
		Input = BuildTree(Input, Tree->Children[i]);
	}
	return Input;
}

// Create a Tree out of an Input of numbers
int *
BuildTree(int * Input, Tree * Tree)
{
	Tree->ChildrenCount = *Input++;
	Tree->MetaCount     = *Input++;

	if (Tree->ChildrenCount > 0)
	{
		Input = AccumulateChildren(Input, Tree);
	}

	if (Tree->MetaCount > 0)
	{
		Input = AccumulateMeta(Input, Tree);
	}

	return Input;
}

// Print a Tree to stdout
void
DumpTree(Tree * Tree)
{
	printf("(Node [");
	for (size_t i = 0; i < Tree->MetaCount; ++i)
	{
		printf(i > 0 ? " %d" : "%d", Tree->Meta[i]);
	}
	printf("] [");
	for (size_t i = 0; i < Tree->ChildrenCount; ++i)
	{
		if (i > 0)
			printf(", ");
		DumpTree(Tree->Children[i]);
	}
	printf("])");
}

int
main (void)
{
	int Input[1 << 16] = { 0 };
	size_t InputLen = 0;

	// Parse Input and count its InputLen
	{
		int * p = Input;
		while (scanf("%d", p++) == 1)
			++InputLen;
	}

	Tree * Tree = EmptyTree();
	BuildTree(Input, Tree);

	DumpTree(Tree);

	return 0;
}
