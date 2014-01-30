<?php
/*
 ============================================================================
 Name        : sudoku.php
 Author      : Bjarke Frøsig
 Version     : 0.01
 Copyright   : GPL
 Description : Naive recursive Sudoku solver
 ============================================================================
*/

$BOARD_HEIGHT = 9;
$BOARD_WIDTH = 9;
$MAX_POSITION = ($BOARD_HEIGHT * $BOARD_WIDTH);

$DUMMY_NUMBER = 0;
$MIN_NUMBER = 1;
$NUMBERS = 9;

function print_board(&$b)
{
	global $BOARD_HEIGHT, $BOARD_WIDTH;
	
	for($i = 0; $i < $BOARD_HEIGHT; ++$i)
	{
		for($j = 0; $j < $BOARD_WIDTH; ++$j)
			printf("%d ", $b[$i][$j]);
		printf("\n");
	}
}

function board_row($position)
{
	global $BOARD_WIDTH;
	return (($position) / $BOARD_WIDTH);
}

function board_column($position)
{
	global $BOARD_WIDTH;
	return (($position) % $BOARD_WIDTH);
}

function board_box_top($pos)
{
	return (board_row($pos) < 3 ? 0 : (board_row($pos) < 6 ? 3 : 6));
}

function board_box_left($pos)
{
	return (board_column($pos) < 3 ? 0 : (board_column($pos) < 6 ? 3 : 6));
}

function check_for_number(&$board, $position, $number)
{
	global $BOARD_WIDTH;
	for($i = 0; $i < $BOARD_WIDTH; ++$i)
		if($board[board_row($position)][$i] == $number)
			return false;

	global $BOARD_HEIGHT;
	for($i = 0; $i < $BOARD_HEIGHT; ++$i)
		if($board[$i][board_column($position)] == $number)
			return false;

	$box_left = board_box_left($position);
	$box_top = board_box_top($position);

	for($x = $box_left; $x < $box_left + 3; ++$x)
		for($y = $box_top; $y < $box_top + 3; ++$y)
			if($board[$y][$x] == $number)
				return false;

	return true;
}

function update_board(&$board, $position, $number)
{
	$board[board_row($position)][board_column($position)] = $number;
}

function board_position_assigned(&$board, $position)
{
	global $DUMMY_NUMBER;
	return $board[board_row($position)][board_column($position)] != $DUMMY_NUMBER;
}

function evaluate_board_setup(&$board, $position)
{
	global $MAX_POSITION;

	if($position == $MAX_POSITION)
	{
		print_board($board);
		return;
	}

	if(board_position_assigned($board, $position))
		evaluate_board_setup($board, $position + 1);
	else
	{
		global $MIN_NUMBER, $NUMBERS, $DUMMY_NUMBER;
	
		for($number = $MIN_NUMBER; $number < ($MIN_NUMBER + $NUMBERS); ++$number)
			if(check_for_number($board, $position, $number))
			{
				update_board($board, $position, $number);
				evaluate_board_setup($board, $position + 1);
			}
		update_board($board, $position, $DUMMY_NUMBER);
	}
}

function read_board_from_console()
{
	global $BOARD_HEIGHT, $BOARD_WIDTH;
	
	$b = array_fill(0, $BOARD_HEIGHT, null);
	for($y = 0; $y < $BOARD_HEIGHT; ++$y)
	{
		$b[$y] = array_fill(0, $BOARD_WIDTH, null);
		fscanf(STDIN, "%d%d%d%d%d%d%d%d%d", $b[$y][0], $b[$y][1], $b[$y][2], $b[$y][3], $b[$y][4], $b[$y][5], $b[$y][6], $b[$y][7], $b[$y][8]);
	}
	return $b;
}

$board = read_board_from_console();
evaluate_board_setup($board, 0);
?>
