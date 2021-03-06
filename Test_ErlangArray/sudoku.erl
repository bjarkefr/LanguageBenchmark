-module(sudoku).
-export([main/0]).
-compile({inline, [for_false/3, t_range_member/4, t_range_member_9step/4, begin_next_row/1, begin_row/1, board_width/0, dummy_number/0, min_number/0, numbers/0, board_row/1, board_column/1, board_box_top/1, board_box_left/1, check_for_number/3, update_board/3, board_position_assigned/2, for/3, evaluate_board_setup/2]}).
-compile(inline_list_funcs).
-compile(inline_array_funcs).
-compile(no_line_info).

%%board_height() -> 9.
board_width() -> 9.
%%max_position() -> board_height() * board_width().

dummy_number() -> 0.
min_number() -> 1.
numbers() -> 9.

print_board(Board) -> print_board(Board, 0).

print_board(_, 81) -> ok;
print_board(Board, I) ->
	C = array:get(I, Board),
	if
	    I rem 9 == 8 -> io:format("~w ~n", [C]);
	    true -> io:format("~w ", [C])
	end,
	print_board(Board, I + 1).

begin_next_row(Position) -> board_width() + begin_row(Position).

begin_row(Position) -> Position - (Position rem board_width()).

board_row(Position) -> Position div board_width().

board_column(Position) -> Position rem board_width().

board_box_top(Pos) ->
	Br = board_row(Pos),
	if
		Br < 3 -> 0;
		Br < 6 -> 3;
		true -> 6
	end.

board_box_left(Pos) ->
	Br = board_column(Pos),
	if
		Br < 3 -> 0;
		Br < 6 -> 3;
		true -> 6
	end.

t_range_member(_, I, Max, _) when I == Max -> false;
t_range_member(T, I, Max, N) ->
	E = array:get(I, T),
	if E == N -> true; true -> t_range_member(T, I + 1, Max, N) end.

t_range_member_9step(_, I, Max, _) when I == Max -> false;
t_range_member_9step(T, I, Max, N) ->
	E = array:get(I, T),
	if E == N -> true; true -> t_range_member_9step(T, I + 9, Max, N) end.

for_false(I, Max, _) when I == Max -> false;
for_false(I, Max, Fun) ->
	Res = Fun(I),
	if Res -> true; true -> for_false(I + 1, Max, Fun) end.

check_for_number(Board, Position, Number) ->
	Found_in_row = t_range_member(Board, begin_row(Position), begin_next_row(Position), Number),
	if
		Found_in_row -> false;
		true ->
			Found_in_column = t_range_member_9step(Board, board_column(Position), board_column(Position) + 81, Number),
			if
				Found_in_column -> false;
				true ->
					{Box_left, Box_top} = {board_box_left(Position), board_box_top(Position)},
					Found_in_box = for_false(Box_left, Box_left + 3, fun(X) ->
						for_false(Box_top, Box_top + 3, fun(Y) -> array:get(X + Y * board_width(), Board) == Number end)
					end),
					not Found_in_box
			end
	end.

update_board(Board, Position, Number) ->
	array:set(Position, Number, Board).

board_position_assigned(Board, Position) ->
	array:get(Position, Board) /= dummy_number().

for(I, Max, _) when I == Max -> ok;
for(I, Max, Fun) -> Fun(I), for(I + 1, Max, Fun).

evaluate_board_setup(Board, 81) -> print_board(Board);
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

read_board_from_console() ->
	read_board_from_console(array:new([{size, 82}, {fixed, true}]), 0).

first([E|_]) -> E.
	
read_board_from_console(Board, 81) -> Board;
read_board_from_console(Board, N) ->
	read_board_from_console(array:set(N, first(element(2, io:fread("", "~d"))), Board), N + 1).

main() ->
	Board = read_board_from_console(),
	evaluate_board_setup(Board, 0).
