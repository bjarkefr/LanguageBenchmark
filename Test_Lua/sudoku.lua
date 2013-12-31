--[[
 ============================================================================
 Name        : sudoku.lua
 Author      : Bjarke Froesig (damn international characters...!)
 Version     : 0.01
 Copyright   : GPL
 Description : Naive recursive Sudoku solver
 ============================================================================
--]]

local BOARD_RIGHTMOST = 8
local BOARD_BOTTOMMOST = 8

local BOARD_HEIGHT = 9
local BOARD_WIDTH  = 9
local MAX_POSITION = (BOARD_HEIGHT * BOARD_WIDTH)

local DUMMY_NUMBER = 0
local MIN_NUMBER = 1
local MAX_NUMBER = 9

function print_board(board)
	for i = 0, BOARD_BOTTOMMOST do
		for j = 0, BOARD_RIGHTMOST do
			io.write(string.format("%d ", board[i][j]))
		end
		print()
	end
end

function board_row(position)
    return math.floor(position / BOARD_WIDTH)
end

function board_column(position)
    return position % BOARD_WIDTH
end

function board_box_top(pos)
    return board_row(pos) < 3 and 0 or (board_row(pos) < 6 and 3 or 6)
end

function board_box_left(pos)
    return board_column(pos) < 3 and 0 or (board_column(pos) < 6 and 3 or 6)
end

function check_for_number(board, position, number)
	for i = 0, BOARD_RIGHTMOST do
		if board[board_row(position)][i] == number then
			return false
		end
	end

	for i = 0, BOARD_BOTTOMMOST do
		if board[i][board_column(position)] == number then
			return false
		end
	end

	local box_left = board_box_left(position)
	local box_top = board_box_top(position)

	for x = box_left, box_left + 2 do
		for y = box_top, box_top + 2 do
			if board[y][x] == number then
				return false
			end
		end
	end

	return true
end

function update_board(board, position, number)
	board[board_row(position)][board_column(position)] = number
end

function board_position_assigned(board, position)
	return board[board_row(position)][board_column(position)] ~= DUMMY_NUMBER
end

function evaluate_board_setup(board, position)
	if position == MAX_POSITION then
		print_board(board)
		return
	end

	if board_position_assigned(board, position) then
		evaluate_board_setup(board, position + 1)
	else
		for number = MIN_NUMBER, MAX_NUMBER do
			if check_for_number(board, position, number) then
				update_board(board, position, number)
				evaluate_board_setup(board, position + 1)
			end
		end
		update_board(board, position, DUMMY_NUMBER)
	end
end

function read_board_from_console()
	local board = {}
	for y = 0, BOARD_BOTTOMMOST do
		board[y] = {}
		for x = 0, BOARD_RIGHTMOST do
			board[y][x] = io.read("*n")
		end
	end
	return board
end

local board = read_board_from_console()
evaluate_board_setup(board, 0)
