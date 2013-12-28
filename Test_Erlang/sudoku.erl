-module(sudoku).
-export([main/0]).
-compile({inline, [board_width/0, dummy_number/0, min_number/0, numbers/0, board_row/1, board_column/1, board_box_top/1, board_box_left/1, check_for_number/3, mapnth/3, setnth/3, update_board/3, board_position_assigned/2, for/3, evaluate_board_setup/2, build/3]}).
-compile(inline_list_funcs).
-compile(no_line_info).

%%board_height() -> 9.
board_width() -> 9.
%%max_position() -> board_height() * board_width().

dummy_number() -> 0.
min_number() -> 1.
numbers() -> 9.

print_board([]) -> ok;
print_board([Row | Rest]) ->
    io:format("~w ~w ~w ~w ~w ~w ~w ~w ~w ~n", Row),
    print_board(Rest).

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

for(I, Max, _) when I == Max -> ok;
for(I, Max, Fun) -> Fun(I), for(I + 1, Max, Fun). %% io:format("success ~w ~n", [I]), 

evaluate_board_setup(Board, 82) -> print_board(Board);
evaluate_board_setup(Board, Position) ->
	Position_assigned = board_position_assigned(Board, Position),
	if
		Position_assigned -> evaluate_board_setup(Board, Position + 1);
		true ->
			for(min_number(), min_number() + numbers(), fun(N) ->
				PlacementValid = check_for_number(Board, Position, N),
				if
					PlacementValid ->
						NewBoard = update_board(Board, Position, N),
						evaluate_board_setup(NewBoard, Position + 1);
					true -> ok
				end
			end)
	end.

setup_board() ->
	[[0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0],
	    [0,0,0,0,0,0,0,0,0]].
	
build(I, Max, _) when I == Max -> [];
build(I, Max, Fun) -> [Fun(I) | build(I + 1, Max, Fun)].
	
read_board_from_console() ->
	build(1, 10, fun(_) -> element(2, io:fread("", "~d~d~d~d~d~d~d~d~d")) end).
	
main() ->
	Board = read_board_from_console(),
    evaluate_board_setup(Board, 1).
