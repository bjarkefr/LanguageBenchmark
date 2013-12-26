-module(sudoku).
-export([main/0]).
%%-compile({inline, [board_height/0, board_width/0, max_position/0, dummy_number/0, min_number/0, numbers/0]}).

%%board_height() -> 9.
board_width() -> 9.
%%max_position() -> board_height() * board_width().

dummy_number() -> 0.
%%min_number() -> 1.
%%numbers() -> 9.

print_board([]) -> ok;
print_board([Row | Rest]) ->
    io:format("~w ~w ~w ~w ~w ~w ~w ~w ~w ~n", Row),
    print_board(Rest).

main() ->
    Board = [[0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0]],
    print_board(Board).

board_row(Position) -> ((Position - 1) div board_width() + 1).

board_column(Position) -> ((Position - 1) rem board_width() + 1).

board_box_top(Pos) ->
	Br = board_row(Pos),
	if
		Br < 4 -> 1;
		Br < 7 -> 4;
		true -> 7
	end.

board_box_left(Pos) ->
	Br = board_column(Pos),
	if
		Br < 4 -> 1;
		Br < 7 -> 4;
		true -> 7
	end.

check_for_number(Board, Position, Number) ->
	Found_in_row = lists:member(Number, lists:nth(board_row(Position), Board)),
	if
		Found_in_row -> false;
		true ->
			Found_in_column = lists:any(fun(Row) -> lists:nth(board_column(Position), Row) == Number end, Board),
			if
				Found_in_column -> false;
				true ->
					{Box_left, Box_top} = {board_box_left(Position), board_box_top(Position)},
					Found_in_box = lists:any(fun(Row) -> lists:member(Number, lists:sublist(Row, Box_left, 3)) end, lists:sublist(Board, Box_top, 3)),
					not Found_in_box
			end
	end.

mapnth(1, [E|Rest], Fun) -> [Fun(E)|Rest];
mapnth(I, [E|Rest], Fun) -> [E|mapnth(I-1, Rest, Fun)].

setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

update_board(Board, Position, Number) ->
	mapnth(board_row(Position), Board, fun(Row) -> setnth(board_column(Position), Row, Number) end).

board_position_assigned(Board, Position) ->
	lists:nth(board_column(Position), lists:nth(board_row(Position), Board)) /= dummy_number().

evaluate_board_setup(Board, 82) -> print_board(Board);
evaluate_board_setup(Board, Position) ->
	Position_assigned = board_position_assigned(Board, Position),
	if
		Position_assigned -> evaluate_board_setup(Board, Position + 1);
		true ->
			
	end.



%%void evaluate_board_setup(ref board board, in int position)
%%{
%%	if(position == MAX_POSITION)
%%	{
%%		print_board(board);
%%		return;
%%	}
%%
%%	if(board_position_assigned(board, position))
%%		evaluate_board_setup(board, position + 1);
%%	else
%%	{
%%		for(int number = MIN_NUMBER; number < (MIN_NUMBER + NUMBERS); ++number)
%%			if(check_for_number(board, position, number))
%%			{
%%				update_board(board, position, number);
%%				evaluate_board_setup(board, position + 1);
%%			}
%%		update_board(board, position, DUMMY_NUMBER);
%%	}
%%}
%%
%%pure nothrow void setup_board(ref board board)
%%{
%%	for(int x = 0; x < BOARD_WIDTH; ++x)
%%		for(int y = 0; y < BOARD_HEIGHT; ++y)
%%			board[y][x] = DUMMY_NUMBER;
%%}
%%
%%pure nothrow void setup_test_board(ref board board)
%%{
%%	for(int pos = 0; pos < MAX_POSITION; ++pos)
%%		board[board_row(pos)][board_column(pos)] = pos;
%%}
%%
%%void read_board_from_console(ref board board)
%%{
%%	for(int pos = 0; pos < MAX_POSITION; ++pos)
%%		readf(" %s", &board[board_row(pos)][board_column(pos)]);
%%}
%%
%%void main()
%%{
%%	board board;
%%	read_board_from_console(board);
%%	evaluate_board_setup(board, 0);
%%}
