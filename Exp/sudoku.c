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

struct bitboard
{
	unsigned int cols[9];
	unsigned int rows[9];
	unsigned int blocks[3][3];
	unsigned int trace[9][9];
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
	for(int y = 0; y < 9; ++y)
	{
		for(int x = 0; x < 9; ++x)
			printf("%i ", bitpos(board.trace[y][x] & 511));
		printf("\n");
	}
}

static void process(int x, int y)
{
	if(board.trace[y][x] & 512)
	{
		++x;
		if(x == 9)
		{
			x = 0;
			++y;
		}

		process(x, y);

		--x;
		if(x == -1)
		{
			x = 9;
			--y;
		}
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

		board.trace[y][x] = v;

		if(y == 8 && x == 8)
		{
			print_board();
			return;
		}

		*cx |= v;
		*cy |= v;
		*cb |= v;

		++x;
		if(x == 9)
		{
			x = 0;
			++y;
		}

		process(x, y);

		--x;
		if(x == -1)
		{
			x = 9;
			--y;
		}

		*cb ^= v;
		*cy ^= v;
		*cx ^= v;
	}
}

static void read_board_from_console()
{
	for(int y = 0; y < 9; ++y)
		for(int x = 0; x < 9; ++x)
		{
			unsigned int v;
			scanf("%u", &v);
			board.trace[y][x] = v;
		}
}

static void preprocess()
{
	for(int y = 0; y < 9; ++y)
	{
		for(int x = 0; x < 9; ++x)
		{
			int v = board.trace[y][x];
			int bv = v != 0 ? 1 << (v - 1) : 0;
			board.trace[y][x] = bv | (bv != 0 ? 512 : 0);
			board.cols[x] |= bv;
			board.rows[y] |= bv;
			board.blocks[div3(y)][div3(x)] |= bv;
			//printf("%u%c;", bitpos(board.trace[y][x] & 511), board.trace[y][x] & 512 ? '*' : ' ');
		}
	}
}

#define set_row(target, data) { for(int i = 0; i < 9; ++i) target[i] = data[i]; }

int main(void)
{
	read_board_from_console();

//	unsigned int r0[] = {0, 7, 0, 0, 0, 0, 0, 8, 0};
//	set_row(board.trace[0], r0);
//	unsigned int r1[] = {0, 3, 0, 7, 6, 2, 0, 0, 1};
//	set_row(board.trace[1], r1);
//	unsigned int r2[] = {0, 0, 1, 9, 8, 0, 0, 0, 0};
//	set_row(board.trace[2], r2);
//	unsigned int r3[] = {1, 0, 0, 0, 0, 0, 9, 0, 0};
//	set_row(board.trace[3], r3);
//	unsigned int r4[] = {8, 0, 3, 0, 0, 0, 1, 0, 2};
//	set_row(board.trace[4], r4);
//	unsigned int r5[] = {0, 0, 6, 0, 0, 0, 0, 0, 8};
//	set_row(board.trace[5], r5);
//	unsigned int r6[] = {0, 0, 0, 0, 3, 1, 6, 0, 0};
//	set_row(board.trace[6], r6);
//	unsigned int r7[] = {5, 0, 0, 2, 4, 9, 0, 1, 0};
//	set_row(board.trace[7], r7);
//	unsigned int r8[] = {0, 1, 0, 0, 0, 0, 0, 9, 0};
//	set_row(board.trace[8], r8);

	preprocess();

	process(0, 0);

	return EXIT_SUCCESS;
}
