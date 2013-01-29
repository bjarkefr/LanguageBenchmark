/*
 ============================================================================
 Name        : suduko.c
 Author      : Bjarke Frøsig
 Version     : 0.01
 Copyright   : GPL
 Description : Naive recursive Suduko solver
 ============================================================================
 */

#define BOARD_HEIGHT 9
#define BOARD_WIDTH 9
#define MAX_POSITION (BOARD_HEIGHT * BOARD_WIDTH)

#define DUMMY_NUMBER 0
#define MIN_NUMBER 1
#define NUMBERS 9

#define false 0
#define true !0

typedef int board[BOARD_HEIGHT][BOARD_WIDTH];

#include <stdio.h>
#include <stdlib.h>

void print_board(board b)
{
	for(int i = 0; i < BOARD_HEIGHT; ++i)
	{
		for(int j = 0; j < BOARD_WIDTH; ++j)
			printf("%i ", b[i][j]);
		printf("\n");
	}
}

#define board_row(position) ((position) / BOARD_WIDTH)
#define board_column(position) ((position) % BOARD_WIDTH)

#define board_box_top(pos) (board_row(pos) < 3 ? 0 : (board_row(pos) < 6 ? 3 : 6))
#define board_box_left(pos) (board_column(pos) < 3 ? 0 : (board_column(pos) < 6 ? 3 : 6))

static int check_for_number(board board, int position, int number)
{
	for(int i = 0; i < BOARD_WIDTH; ++i)
		if(board[board_row(position)][i] == number)
			return false;

	for(int i = 0; i < BOARD_HEIGHT; ++i)
		if(board[i][board_column(position)] == number)
			return false;

	int box_left = board_box_left(position), box_top = board_box_top(position);

	for(int x = box_left; x < box_left + 3; ++x)
		for(int y = box_top; y < box_top + 3; ++y)
			if(board[y][x] == number)
				return false;

	return true;
}

static void update_board(board board, int position, int number)
{
	board[board_row(position)][board_column(position)] = number;
}

static int board_position_assigned(board board, int position)
{
	return board[board_row(position)][board_column(position)] != DUMMY_NUMBER;
}

static void evaluate_board_setup(board board, int position)
{
	if(position == MAX_POSITION)
	{
		print_board(board);
		return;
	}

	if(board_position_assigned(board, position))
		evaluate_board_setup(board, position + 1);
	else
	{
		for(int number = MIN_NUMBER; number < (MIN_NUMBER + NUMBERS); ++number)
			if(check_for_number(board, position, number))
			{
				update_board(board, position, number);
				evaluate_board_setup(board, position + 1);
			}
		update_board(board, position, DUMMY_NUMBER);
	}
}

void setup_board(board board)
{
	for(int x = 0; x < BOARD_WIDTH; ++x)
		for(int y = 0; y < BOARD_HEIGHT; ++y)
			board[y][x] = DUMMY_NUMBER;
}

void setup_test_board(board board)
{
	for(int pos = 0; pos < MAX_POSITION; ++pos)
		board[board_row(pos)][board_column(pos)] = pos;
}

void read_board_from_console(board board)
{
	for(int pos = 0; pos < MAX_POSITION; ++pos)
		scanf("%i", &board[board_row(pos)][board_column(pos)]);
}

int main(void)
{
	board board;

	read_board_from_console(board);

	evaluate_board_setup(board, 0);

	return EXIT_SUCCESS;
}
