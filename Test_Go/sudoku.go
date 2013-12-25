/*
 ============================================================================
 Name        : sudoku.go
 Author      : Bjarke Froesig (damn international characters...!)
 Version     : 0.01
 Copyright   : GPL
 Description : Naive recursive Sudoku solver
 ============================================================================
 */

package main

import "fmt"

const BOARD_HEIGHT = 9
const BOARD_WIDTH  = 9
const MAX_POSITION = (BOARD_HEIGHT * BOARD_WIDTH)

const DUMMY_NUMBER = 0
const MIN_NUMBER = 1
const NUMBERS = 9

type board [BOARD_HEIGHT][BOARD_WIDTH]int

func print_board(board *board) {
	for i := 0; i < BOARD_HEIGHT; i++ {
		for j := 0; j < BOARD_WIDTH; j++ {
			fmt.Printf("%d ", (*board)[i][j]);
		}
		fmt.Println();
	}
}

func board_row(position int) int {
	return position / BOARD_WIDTH
}


func board_column(position int) int {
	return position % BOARD_WIDTH;
}

func board_box_top(pos int) int {
	if board_row(pos) < 3 {
		return 0
	} else if board_row(pos) < 6 {
		return 3
	} else {
		return 6
	}
}

func board_box_left(pos int) int {
	if board_column(pos) < 3 {
		return 0
	} else if board_column(pos) < 6 {
		return 3
	} else {
		return 6
	}
}

func check_for_number(board *board, position int, number int) bool {
	for i := 0; i < BOARD_WIDTH; i++ {
		if (*board)[board_row(position)][i] == number {
			return false
		}
	}

	for i := 0; i < BOARD_HEIGHT; i++ {
		if (*board)[i][board_column(position)] == number {
			return false
		}
	}

	box_left, box_top := board_box_left(position), board_box_top(position)

	for x := box_left; x < box_left + 3; x++ {
		for y := box_top; y < box_top + 3; y++ {
			if (*board)[y][x] == number {
				return false
			}
		}
	}

	return true
}

func update_board(board *board, position int, number int) {
	(*board)[board_row(position)][board_column(position)] = number
}

func board_position_assigned(board *board, position int) bool {
	return (*board)[board_row(position)][board_column(position)] != DUMMY_NUMBER
}

func evaluate_board_setup(board *board, position int) {
	if position == MAX_POSITION {
		print_board(board)
		return
	}

	if board_position_assigned(board, position) {
		evaluate_board_setup(board, position + 1)
	} else {
		for number := MIN_NUMBER; number < (MIN_NUMBER + NUMBERS); number++ {
			if check_for_number(board, position, number) {
				update_board(board, position, number)
				evaluate_board_setup(board, position + 1)
			}
		}
		update_board(board, position, DUMMY_NUMBER)
	}
}

func setup_board(board *board) {
	for x := 0; x < BOARD_WIDTH; x++ {
		for y := 0; y < BOARD_HEIGHT; y++ {
			(*board)[y][x] = DUMMY_NUMBER;
		}
	}
}

func setup_test_board(board *board) {
	for pos := 0; pos < MAX_POSITION; pos++ {
		(*board)[board_row(pos)][board_column(pos)] = pos
	}
}

func read_board_from_console(board *board) {
	for pos := 0; pos < MAX_POSITION; pos++ {
		fmt.Scanf("%d", &board[board_row(pos)][board_column(pos)])
	}
}

func main() {
	var board board
	read_board_from_console(&board)
	evaluate_board_setup(&board, 0)
}
