/*
 ============================================================================
 Name        : sudoku.d
 Author      : Bjarke Froesig (damn international characters...!)
 Version     : 0.01
 Copyright   : GPL
 Description : Naive recursive Sudoku solver
 ============================================================================
 */

//TODO: Create a class and mark eveything private to help the compiler determine that inlining is preferable?

const int BOARD_HEIGHT = 9;
const int BOARD_WIDTH  = 9;
const int MAX_POSITION = (BOARD_HEIGHT * BOARD_WIDTH);

const int DUMMY_NUMBER = 0;
const int MIN_NUMBER = 1;
const int NUMBERS = 9;

import std.stdio;

alias int[BOARD_HEIGHT][BOARD_WIDTH] board;

private void print_board(in ref board board)
{
	for(int i = 0; i < BOARD_HEIGHT; ++i)
	{
		for(int j = 0; j < BOARD_WIDTH; ++j)
			writef("%s ", board[i][j]);
		writeln();
	}
}

pure nothrow int board_row(in int position)
{
    return position / BOARD_WIDTH;
}

pure nothrow int board_column(in int position)
{
    return position % BOARD_WIDTH;
}

pure nothrow int board_box_top(in int pos)
{
    return board_row(pos) < 3 ? 0 : board_row(pos) < 6 ? 3 : 6;
}

pure nothrow int board_box_left(in int pos)
{
    return board_column(pos) < 3 ? 0 : board_column(pos) < 6 ? 3 : 6;
}

pure nothrow bool check_for_number(in ref board board, in int position, in int number)
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

pure nothrow void update_board(ref board board, in int position, in int number)
{
	board[board_row(position)][board_column(position)] = number;
}

pure nothrow int board_position_assigned(in ref board board, in int position)
{
	return board[board_row(position)][board_column(position)] != DUMMY_NUMBER;
}

void evaluate_board_setup(ref board board, in int position)
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

pure nothrow void setup_board(ref board board)
{
	for(int x = 0; x < BOARD_WIDTH; ++x)
		for(int y = 0; y < BOARD_HEIGHT; ++y)
			board[y][x] = DUMMY_NUMBER;
}

pure nothrow void setup_test_board(ref board board)
{
	for(int pos = 0; pos < MAX_POSITION; ++pos)
		board[board_row(pos)][board_column(pos)] = pos;
}

void read_board_from_console(ref board board)
{
	for(int pos = 0; pos < MAX_POSITION; ++pos)
		readf(" %s", &board[board_row(pos)][board_column(pos)]);
}

void main()
{
	board board;
	read_board_from_console(board);
	evaluate_board_setup(board, 0);
}
