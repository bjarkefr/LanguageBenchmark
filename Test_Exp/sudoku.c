/*
 ============================================================================
 Name        : sudoku.c
 Author      : Bjarke Frøsig
 Version     : 0.02
 Copyright   : GPL
 Description : Naive recursive Sudoku solver
 ============================================================================
 */
#include <stdio.h>
#include <stdlib.h>

//	rb = (rb << 1);
//	rb = (rb & 0b0000000001); //=> nz bit
//	rb << nz-bit; ? this is a rotate through nz bit....? possible?

#define div3(N) ((N * 11) >> 5)
#define div9(N) ((N * 57) >> 9)

struct bitboard
{
	unsigned int cols[9];
	unsigned int rows[9];
	unsigned int blocks[3][3];
	unsigned int trace[81];
} board;

static int bitpos(int n)
{
	switch(n)
	{
	case 0:
		return 0;
	case 1:
		return 1;
	case 2:
		return 2;
	case 4:
		return 3;
	case 8:
		return 4;
	case 16:
		return 5;
	case 32:
		return 6;
	case 64:
		return 7;
	case 128:
		return 8;
	case 256:
		return 9;
	default:
		printf("WTF %i", n);
		return 0;
	}
}

static void print_board()
{
	int p = 0;
	for(int y = 0; y < 9; ++y)
	{
		for(int x = 0; x < 9; ++x, ++p)
			printf("%i ", bitpos(board.trace[p] & 511));
		printf("\n");
	}
}

static void process(int p)
{
	int y = div9(p);
	int x = p % 9;

	if(board.trace[p] & 512)
	{
		process(p + 1);
		return;
	}

	for(unsigned int v = 1; v != 512; v <<= 1)
	{
		unsigned int* cx = board.cols + x;
		unsigned int* cy = board.rows + y;

		if(*cx & v | *cy & v)
			continue;

		unsigned int* cb = &board.blocks[div3(y)][div3(x)];
		if(*cb & v)
			continue;

		board.trace[p] = v;

		if(p == 80)
		{
			print_board();
			return;
		}

		*cx |= v;
		*cy |= v;
		*cb |= v;

		process(p + 1);

		*cb ^= v;
		*cy ^= v;
		*cx ^= v;
	}
}

static void read_board_from_console()
{
	int p = 0;
	for(int y = 0; y < 9; ++y)
		for(int x = 0; x < 9; ++x, ++p)
		{
			unsigned int v;
			scanf("%u", &v);
			board.trace[p] = v;
		}
}

static void preprocess()
{
	int p = 0;
	for(int y = 0; y < 9; ++y)
	{
		for(int x = 0; x < 9; ++x, ++p)
		{
			int v = board.trace[p];
			int bv = v != 0 ? 1 << (v - 1) : 0;
			board.trace[p] = bv | (bv != 0 ? 512 : 0);
			board.cols[x] |= bv;
			board.rows[y] |= bv;
			board.blocks[div3(y)][div3(x)] |= bv;
		}
	}
}

int main(void)
{
	read_board_from_console();

	preprocess();

	print_board();

	process(0);

	return EXIT_SUCCESS;
}
