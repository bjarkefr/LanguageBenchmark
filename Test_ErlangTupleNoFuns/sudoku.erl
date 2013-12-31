-module(sudoku).
-export([main/0]).
-compile({inline, [t_member_range_column/4, t_member_range_box/4 , t_range_member/4, t_range_member_row_step/4, begin_next_row/1, begin_row/1, board_row/1, board_column/1, board_box_top/1, board_box_left/1, check_for_number/3, update_board/3, board_position_assigned/2, evaluate_board_setup/2, try_place_numbers/3]}).
-compile(inline_list_funcs).
-compile(no_line_info).

-define(BOARD_HEIGHT, 9).
-define(BOARD_WIDTH, 9).
-define(POSITIONS, ?BOARD_HEIGHT * ?BOARD_WIDTH).

-define(DUMMY_NUMBER, 0).
-define(MIN_NUMBER, 1).
-define(NUMBERS, 9).

print_board(Board) -> print_board(Board, 1).

print_board(_, 82) -> ok;
print_board(Board, I) ->
	C = element(I, Board),
	if
	    I rem 9 == 0 -> io:format("~w ~n", [C]);
	    true -> io:format("~w ", [C])
	end,
	print_board(Board, I + 1).

begin_next_row(Position) -> ?BOARD_WIDTH + begin_row(Position).

begin_row(Position) -> Position - ((Position - 1) rem ?BOARD_WIDTH).

board_row(Position) -> ((Position - 1) div ?BOARD_WIDTH + 1).

board_column(Position) -> ((Position - 1) rem ?BOARD_WIDTH + 1).

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

%% A generic range_member(Tuple, From, To, Step, E) is slower than the two special case methods below...
	
t_range_member(_, I, Max, _) when I == Max -> false;
t_range_member(T, I, Max, N) -> if element(I, T) == N -> true; true -> t_range_member(T, I + 1, Max, N) end.

t_range_member_row_step(_, I, Max, _) when I == Max -> false;
t_range_member_row_step(T, I, Max, N) -> if element(I, T) == N -> true; true -> t_range_member_row_step(T, I + ?BOARD_WIDTH, Max, N) end.

t_member_range_column(_, Position, PositionEnd, _) when Position == PositionEnd -> false;
t_member_range_column(Board, Position, PositionEnd, E) ->
	if
		element(Position, Board) == E -> true;
		true -> t_member_range_column(Board, Position + ?BOARD_WIDTH, PositionEnd, E)
	end.

t_member_range_box(_, Position, PositionEnd, _) when Position == PositionEnd -> false;
t_member_range_box(Board, Position, PositionEnd, E) ->
	Found = t_member_range_column(Board, Position, Position + ?BOARD_WIDTH * 3, E),
	if
		Found -> true;
		true -> t_member_range_box(Board, Position + 1, PositionEnd, E)
	end.

check_for_number(Board, Position, Number) ->
	Found_in_row = t_range_member(Board, begin_row(Position), begin_next_row(Position), Number),
	if
		Found_in_row -> false;
		true ->
			Found_in_column = t_range_member_row_step(Board, board_column(Position), board_column(Position) + ?POSITIONS, Number),
			if
				Found_in_column -> false;
				true ->
					BoxPosition = (board_box_top(Position) - 1) * ?BOARD_WIDTH + board_box_left(Position),
					Found_in_box = t_member_range_box(Board, BoxPosition, BoxPosition + 3, Number),
					not Found_in_box
			end
	end.

update_board(Board, Position, Number) ->
	setelement(Position, Board, Number).

board_position_assigned(Board, Position) ->
	element(Position, Board) /= ?DUMMY_NUMBER.

try_place_numbers(_, _, ?MIN_NUMBER + ?NUMBERS) -> ok;
try_place_numbers(Board, Position, N) ->
	PlacementValid = check_for_number(Board, Position, N),
	if
		PlacementValid ->
			NewBoard = update_board(Board, Position, N),
			evaluate_board_setup(NewBoard, Position + 1);
		true -> ok
	end,
	try_place_numbers(Board, Position, N + 1).

evaluate_board_setup(Board, ?POSITIONS + 1) -> print_board(Board);
evaluate_board_setup(Board, Position) ->
	Position_assigned = board_position_assigned(Board, Position),
	if
		Position_assigned -> evaluate_board_setup(Board, Position + 1);
		true -> try_place_numbers(Board, Position, ?MIN_NUMBER)
	end.

setup_board() ->
	{0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0}.

read_board_from_console() ->
	read_board_from_console(setup_board(), 1).

first([E|_]) -> E.
	
read_board_from_console(Board, ?POSITIONS + 1) -> Board;
read_board_from_console(Board, N) ->
	read_board_from_console(setelement(N, Board, first(element(2, io:fread("", "~d")))), N + 1).

main() ->
	Board = read_board_from_console(),
	evaluate_board_setup(Board, 1).
