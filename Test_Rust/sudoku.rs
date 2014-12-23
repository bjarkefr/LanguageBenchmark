/*
 ============================================================================
 Name        : sudoku.rs
 Author      : Bjarke Frøsig
 Version     : 0.01
 Copyright   : GPL
 Description : Naive recursive Sudoku solver
 ============================================================================
 */

use std::io;
use std::str;

const BOARD_HEIGHT: uint = 9;
const BOARD_WIDTH: uint = 9;
const MAX_POSITION: uint = (BOARD_HEIGHT * BOARD_WIDTH);

const DUMMY_NUMBER: int = 0;
const MIN_NUMBER: int = 1;
const NUMBERS: int = 9;

type Board = [[int, ..BOARD_WIDTH], ..BOARD_HEIGHT];

fn print_board(board: &Board)
{
    for row in board.iter()
    {
	for cell in row.iter()
	{
	    print!("{} ", cell);
	}
	println!("");
    }
}

fn board_row(pos: uint) -> uint
{
    return pos.div(BOARD_WIDTH);
}

fn board_column(pos: uint) -> uint
{
    return pos.rem(BOARD_WIDTH);
}

fn board_box_top(pos: uint) -> uint
{
    if board_row(pos) < 3
    {
	return 0;
    }
    else
    {
	if board_row(pos) < 6
	{
	    return 3;
	}
	else
        {
	    return 6;
	}
    }
}

fn board_box_left(pos: uint) -> uint
{
    return if board_column(pos) < 3
    {
	return 0;
    }
    else
    {
	if board_column(pos) < 6
	{
	    return 3;
	}
	else
	{
	    return 6;
	}
    }
}

fn check_for_number(board: &Board, pos: uint, number: int) -> bool
{
    for i in range(0, BOARD_WIDTH)
    {
	if board[board_row(pos)][i] == number
	{
	    return false;
	}
    }

    for i in range(0, BOARD_HEIGHT)
    {
	if board[i][board_column(pos)] == number
	{
	    return false;
	}
    }

    let box_left = board_box_left(pos);
    let box_top = board_box_top(pos);

    for x in range(box_left, box_left + 3)
    {
	for y in range(box_top, box_top + 3)
	{
	    if board[y][x] == number
	    {
		return false;
	    }
	}
    }

    return true;
}

fn update_board(board: &mut Board, pos: uint, number: int)
{
    board[board_row(pos)][board_column(pos)] = number;
}

fn board_position_assigned(board: &Board, pos: uint) -> bool
{
    return board[board_row(pos)][board_column(pos)] != DUMMY_NUMBER;
}

fn evaluate_board_setup(board: &mut Board, pos: uint)
{
    if pos == MAX_POSITION
    {
	print_board(board);
	return;
    }

    if board_position_assigned(board, pos)
    {
	evaluate_board_setup(board, pos + 1);
    }
    else
    {
	for number in range(MIN_NUMBER, (MIN_NUMBER + NUMBERS))
	{
	    if check_for_number(board, pos, number)
	    {
		update_board(board, pos, number);
    	        evaluate_board_setup(board, pos + 1);
	    }
	}
	update_board(board, pos, DUMMY_NUMBER);
    }
}

fn read_integer_from_console() -> int
{
    loop
    {
	let char = [io::stdin().read_byte().ok().expect("Unable to read from console")];
        let str = str::from_utf8(&char).expect("Failed to convert char to string, dammit!");
	let result = from_str(str);
	match result
	{
	    Some(i) => return i,
	    None => ()
	};
    }
}

fn read_board_from_console() -> Board
{
    let mut board:Board = [[DUMMY_NUMBER, ..BOARD_WIDTH], ..BOARD_HEIGHT];

    for r in range(0, BOARD_HEIGHT)
    {
	for c in range(0, BOARD_WIDTH)
	{
	    board[r][c] = read_integer_from_console();
	}
    }

    return board;
}

fn main()
{
    let mut board = read_board_from_console();

    evaluate_board_setup(&mut board, 0);
}
