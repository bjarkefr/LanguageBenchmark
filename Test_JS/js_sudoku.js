var fs = require('fs');

var BOARD_HEIGHT = 9;
var BOARD_WIDTH = 9;
var MAX_POSITION = (BOARD_HEIGHT * BOARD_WIDTH);

var DUMMY_NUMBER = 0;
var MIN_NUMBER = 1;
var NUMBERS = 9;

function print_board(board)
{
	for(var y = 0; y < BOARD_HEIGHT; ++y)
    {
	    for(var x = 0; x < BOARD_WIDTH; ++x)
	        process.stdout.write(board[y][x] + ' ');

	    process.stdout.write('\n');
    }
}

function board_row(position) { return Math.floor(position / BOARD_WIDTH); }
function board_column(position) { return position % BOARD_WIDTH; }

function board_box_top(pos) { return board_row(pos) < 3 ? 0 : board_row(pos) < 6 ? 3 : 6; }
function board_box_left(pos) { return board_column(pos) < 3 ? 0 : board_column(pos) < 6 ? 3 : 6; }

function check_for_number(board, position, number)
{
	for(var i = 0; i < BOARD_WIDTH; ++i)
    if(board[board_row(position)][i] == number)
        return false;

    for(var i = 0; i < BOARD_HEIGHT; ++i)
        if(board[i][board_column(position)] == number)
            return false;

    var box_left = board_box_left(position), box_top = board_box_top(position);

    for(var x = box_left; x < box_left + 3; ++x)
        for(var y = box_top; y < box_top + 3; ++y)
            if(board[y][x] == number)
                return false;

    return true;
}

function update_board(board, position, number)
{
    board[board_row(position)][board_column(position)] = number;
}

function board_position_assigned(board, position)
{
    //process.stdout.write('X' + board_row(position).toString() + 'x');
    return board[board_row(position)][board_column(position)] != DUMMY_NUMBER;
}

function evaluate_board_setup(board, position)
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
        for(var number = MIN_NUMBER; number < (MIN_NUMBER + NUMBERS); ++number)
            if(check_for_number(board, position, number))
            {
                update_board(board, position, number);
                evaluate_board_setup(board, position + 1);
            }
        update_board(board, position, DUMMY_NUMBER);
    }
}

function read_board_from_console(filename, success) {
    process.stdin.resume();
    var fdata = fs.readFileSync(filename, "ascii");
    var board = new Array(BOARD_HEIGHT);
    var frows = fdata.split('\n');

    for(var y = 0; y < BOARD_HEIGHT; ++y)
    {
        var row = new Array(BOARD_WIDTH);
        board[y] = row;
        var frow = frows[y].split(' ');
        for(var x = 0; x < BOARD_WIDTH; ++x)
            row[x] = parseInt(frow[x]);
    }

    return board;
}

var board = read_board_from_console(process.argv[2]);
evaluate_board_setup(board, 0);
process.exit(0);
