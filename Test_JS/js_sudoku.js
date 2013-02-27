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

//#define board_row(position) ((position) / BOARD_WIDTH)
//#define board_column(position) ((position) % BOARD_WIDTH)

//#define board_box_top(pos) (board_row(pos) < 3 ? 0 : (board_row(pos) < 6 ? 3 : 6))
//#define board_box_left(pos) (board_column(pos) < 3 ? 0 : (board_column(pos) < 6 ? 3 : 6))

//static int check_for_number(board board, int position, int number)
//{
//	    for(int i = 0; i < BOARD_WIDTH; ++i)
//    if(board[board_row(position)][i] == number)
//        return false;

//    for(int i = 0; i < BOARD_HEIGHT; ++i)
//        if(board[i][board_column(position)] == number)
//            return false;

//    int box_left = board_box_left(position), box_top = board_box_top(position);

//    for(int x = box_left; x < box_left + 3; ++x)
//        for(int y = box_top; y < box_top + 3; ++y)
//            if(board[y][x] == number)
//                return false;

//    return true;
//    }

//static void update_board(board board, int position, int number)
//    {
//    board[board_row(position)][board_column(position)] = number;
//    }

//static int board_position_assigned(board board, int position)
//    {
//    return board[board_row(position)][board_column(position)] != DUMMY_NUMBER;
//    }

//static void evaluate_board_setup(board board, int position)
//    {
//    if(position == MAX_POSITION)
//    {
//        print_board(board);
//        return;
//    }

//    if(board_position_assigned(board, position))
//        evaluate_board_setup(board, position + 1);
//    else
//    {
//        for(int number = MIN_NUMBER; number < (MIN_NUMBER + NUMBERS); ++number)
//            if(check_for_number(board, position, number))
//    {
//                update_board(board, position, number);
//                evaluate_board_setup(board, position + 1);
//    }
//        update_board(board, position, DUMMY_NUMBER);
//    }
//    }

//void setup_board(board board)
//    {
//    for(int x = 0; x < BOARD_WIDTH; ++x)
//        for(int y = 0; y < BOARD_HEIGHT; ++y)
//            board[y][x] = DUMMY_NUMBER;
//    }

//void setup_test_board(board board)
//    {
//    for(int pos = 0; pos < MAX_POSITION; ++pos)
//        board[board_row(pos)][board_column(pos)] = pos;
//    }

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

process.stdout.write(process.argv[2] + '\n');
var board = read_board_from_console(process.argv[2]);
print_board(board);
//evaluate_board_setup(board, 0);
process.exit(0);
