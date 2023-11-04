% FILEPATH: /Users/tonevanda/Documents/GitHub/PFL-Crosscut/src/board.pl

:- use_module(library(lists)).
:- consult('utils.pl').
:- dynamic gameState/3.

clear_buffer:-
	repeat,
	get_char(C),
	C='\n',
	!.

get_game_state(Board, Rows, Columns) :-
    gameState(Board, Rows, Columns).
% draw_horizontal_line(+N)
% Draws a horizontal line of size N with the character '-'
draw_horizontal_line(N) :-
    N > 0,
    write('_'),
    N1 is N - 1,
    draw_horizontal_line(N1).
draw_horizontal_line(0).

draw_top_half(N) :-
    N > 0,
    write(' '),
    write(' '),
    write(' '),
    write('|'),
    N1 is N - 1,
    draw_top_half(N1).
draw_top_half(0).

draw_middle_half(Row, ColumnIndex) :-
    get_game_state(Board,_,Columns),
    ColumnIndex < Columns,
    write(' '),
    get_piece(Board, Row, ColumnIndex, Piece),
    write(Piece),
    write(' '),
    write('|'),
    ColumnIndex1 is ColumnIndex + 1,
    draw_middle_half(Row, ColumnIndex1).
draw_middle_half(_, _).

draw_bottom_half(N) :-
    N > 0,
    write('_'),
    write('_'),
    write('_'),
    write('|'),
    N1 is N - 1,
    draw_bottom_half(N1).
draw_bottom_half(0).

display_board :-
    get_game_state(Board, Rows, Columns),
    write(' '),
    Columns1 is Columns * 3 + Columns-1,
    draw_horizontal_line(Columns1),
    display_board_aux(0, Rows, Columns, Board).
    
display_board_aux(RowIndex, Rows, Columns, Board) :-
    RowIndex < Rows,  
    nl,
    write('|'),
    draw_top_half(Columns),
    nl,
    write('|'),
    draw_middle_half(RowIndex, 0),
    nl,
    write('|'),
    draw_bottom_half(Columns),
    RowIndex1 is RowIndex + 1,
    display_board_aux(RowIndex1, Rows, Columns, Board).
display_board_aux(Rows, Rows, _, _).

% get_element(+Board, +I, +J, ?Element)
% Gets the piece at row I and column J of the board
get_piece(Board, I, J, Element) :-
    length(Board, N), % get the number of rows
    nth0(I, Board, Row), % get the row at index I
    length(Row, M), % get the number of columns
    I >= 0, I < N, % check if the row index is within bounds
    J >= 0, J < M, % check if the column index is within bounds
    nth0(J, Row, Element), % get the element at index J in the row
    !.
    
% get_piece(+Board, +I, +J, +State, +Current)
% Gets the piece at row I and column J of the board
get_piece(Board, I, _, Element, Current) :-
    nth0(I, Board, Row), % get the row at index I
    nth0(Current, Row, Element). % get the piece at index Current


% is_empty(+Board, +I, +J)
% Checks if the position at row I and column J of the board is empty
is_empty(Board, I, J) :-
    get_piece(Board, I, J, ' '), !.
is_empty(_, _, _):- !, fail.

% not_edge(+Board, +I, +J)
% Checks if the position at row I and column J of the board is not at the edge
not_edge(Board, I, J) :-
    length(Board, N), % get the number of rows
    nth0(I, Board, Row), % get the row at index I
    length(Row, M), % get the number of columns
    I > 0, I < N - 1, % check if the row index is not at the edge
    J > 0, J < M - 1. % check if the column index is not at the edge





validate_move(Board, I, J,State) :-
    is_empty(Board, I, J),
    \+not_edge(Board, I, J),!, % check if the position is at the edge´
    flip(Board, State, I, J, NewBoard),
    write('piece was atempted to be placed on edge'),nl, 
    update_game_state(NewBoard).

validate_move(Board, I, J,State) :-
    is_empty(Board, I, J),
    not_edge(Board, I, J),
    place_piece(Board, I, J, State, NewBoard),
    write('not on an edge'),nl,
    update_game_state(NewBoard),
    flip(NewBoard, State, I, J, NewestBoard),
    update_game_state(NewestBoard).

validate_move(Board, I, J,State) :-
    is_empty(Board, I, J),
    not_edge(Board, I, J),
    place_piece(Board, I, J, State, NewBoard),
    write('not on an edge'),nl,
    update_game_state(NewBoard).
    
% place_piece(+Board, +I, +J, +NewPiece, -NewBoard)
% Places the piece at row I and column J of the board
place_piece(Board,I, J, NewPiece,NewBoard) :-
    nth0(I, Board, Row), % get the row at index I
    replace(Row, J, NewPiece, NewRow),
    replace(Board, I, NewRow, NewBoard).



flip_left(Ally, LineIndex, ColumnIndex, Board, Board) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally).
flip_left(Ally, LineIndex, ColumnIndex, Board, NewestBoard) :-
    ColumnIndex>0,
    get_state(Ally, Enemy),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    ColumnIndex1 is ColumnIndex - 1,
    flip_left(Ally, LineIndex, ColumnIndex1, NewBoard, NewestBoard).

flip_right(Ally, LineIndex, ColumnIndex, Board, _, Board) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),
    update_game_state(Board),!.
flip_right(Ally, LineIndex, ColumnIndex, Board, Columns, NewestBoard) :-
    ColumnIndex<Columns,
    get_state(Ally, Enemy),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    ColumnIndex1 is ColumnIndex + 1,
    flip_right(Ally, LineIndex, ColumnIndex1, NewBoard, Columns, NewestBoard).

flip_up(Ally, LineIndex, ColumnIndex, Board, Board) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),
    update_game_state(Board),!.
flip_up(Ally, LineIndex, ColumnIndex, Board, NewestBoard) :-
    LineIndex>0,
    get_state(Ally, Enemy),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    LineIndex1 is LineIndex - 1,
    flip_up(Ally, LineIndex1, ColumnIndex, NewBoard, NewestBoard).


flip_down(Ally, LineIndex, ColumnIndex, Board, _, Board) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),
    update_game_state(Board),!.
flip_down(Ally, LineIndex, ColumnIndex, Board, Rows, NewestBoard) :-
    LineIndex>Rows,
    get_state(Ally, Enemy),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    LineIndex1 is LineIndex + 1,
    flip_down(Ally, LineIndex1, ColumnIndex, NewBoard, Rows, NewestBoard).

flip(Board, State, LineIndex, ColumnIndex, NewBoard) :-
    ColumnIndex1 is ColumnIndex - 1,
    get_state(State, Enemy),
    get_piece(Board, LineIndex, ColumnIndex1, Enemy),
    flip_left(State, LineIndex, ColumnIndex1, Board, NewBoard).

flip(Board, State, LineIndex, ColumnIndex, NewBoard) :-
    get_game_state(_, _, Columns),
    ColumnIndex1 is ColumnIndex + 1,
    get_state(State, Enemy),
    get_piece(Board, LineIndex, ColumnIndex1, Enemy),
    flip_right(State, LineIndex, ColumnIndex1, Board, Columns, NewBoard).
flip(Board, State, LineIndex, ColumnIndex, NewBoard) :-
    LineIndex1 is LineIndex - 1,
    get_state(State, Enemy),
    get_piece(Board, LineIndex1, ColumnIndex, Enemy),
    flip_up(State, LineIndex1, ColumnIndex, Board, NewBoard).
flip(Board, State, LineIndex, ColumnIndex, NewBoard) :-
    get_game_state(_, Rows, _),
    LineIndex1 is LineIndex + 1,
    get_state(State, Enemy),
    get_piece(Board, LineIndex1, ColumnIndex, Enemy),
    flip_down(State, LineIndex1, ColumnIndex, Board , Rows, NewBoard).
flip(Board,_,_,_,Board):-write('nuhuh'),fail.
/*
flip_check_vertical().
flip_check_vertical().
flip_check_horizontal().
flip_check_horizontal().
*/
% check_win(+State)
% Checks if there is a win condition for the given state
check_win(State, LineIndex, ColumnIndex) :-
    get_game_state(Board, Rows, Columns),
    check_horizontal(Board, LineIndex, ColumnIndex, State, Columns),
    nl,write(State),
    write(' horizontal win!'),
    retractall(gameState(Board, Rows, Columns)),
    !.

check_win(State, _, ColumnIndex) :-
    get_game_state(Board, Rows, Columns),
    RowNumber is Rows - 1,
    check_vertical(Board, 1, ColumnIndex, State, RowNumber),
    write(Board),
    nl,write(State),
    write(' vertical win!'),
    retractall(gameState(Board, Rows, Columns)),
    !.


% check_horizontal(+Board, +I, +J, +State, +Columns)
% Checks if there is a continuous horizontal segment from one side of the board to the other
check_horizontal(Board, I, J, State, Columns) :-
    my_forall(1, Columns - 2, get_piece(Board, I, J, State)).

% check_vertical(+Board, +I, +J, +State, +Rows)
% Checks if there is a continuous vertical segment from one side of the board to the other
check_vertical(Board, I, J, State, Rows) :-
    I < Rows,
    get_piece(Board, I, J, State),
    write(State),
    write(' found at: Row '),
    write(I),
    write(',Collumn '),
    write(J),
    nl,
    I1 is I + 1,
    check_vertical(Board, I1, J, State, Rows).
check_vertical(_, Rows, _, _, Rows).

move(State, LineIndex, ColumnIndex):-
    repeat,
    nl,
    write(State),
    write(', select the row:'), nl,
    read_letter(LineIndex),
    write(State),
    write(', select the column:'), nl,
    read_number(ColumnIndex),
    get_game_state(Board, _, _),
    validate_move(Board, LineIndex, ColumnIndex, State),!.
    
get_state('R', 'B').

get_state('B', 'R').

