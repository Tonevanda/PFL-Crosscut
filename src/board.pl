% FILEPATH: /Users/tonevanda/Documents/GitHub/PFL-Crosscut/src/board.pl

:- use_module(library(lists)).
:- consult('utils.pl').
:- dynamic gameState/3.

clear_buffer:-
	repeat,
	get_char(C),
	C='\n',
	!.

get_gameState(gameState(Board, Rows, Columns, PlayerPiece)) :-
    gameState(Board, Rows, Columns, PlayerPiece).
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

draw_middle_half(Row, ColumnIndex, Board, Columns) :-
    ColumnIndex < Columns,
    write(' '),
    get_piece(Board, Row, ColumnIndex, Piece),
    write(Piece),
    write(' '),
    write('|'),
    ColumnIndex1 is ColumnIndex + 1,
    draw_middle_half(Row, ColumnIndex1, Board, Columns).
draw_middle_half(_, Columns, _, Columns).

draw_bottom_half(N) :-
    N > 0,
    write('_'),
    write('_'),
    write('_'),
    write('|'),
    N1 is N - 1,
    draw_bottom_half(N1).
draw_bottom_half(0).

display_game(gameState(Board, Rows, Columns, _)) :-
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
    draw_middle_half(RowIndex, 0, Board, Columns),
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


% validate_move(+Board, +I, +J)
% Checks if the position at row I and column J of the board is a valid move
validate_move(Board, I, J) :-
    is_empty(Board, I, J),
    not_edge(Board, I, J).
validate_move(_, _, _):- !, fail.

% place_piece(+Board, +I, +J, +NewPiece, -NewBoard)
% Places the piece at row I and column J of the board
place_piece(Board, Rows, Columns, NewPiece,I, J, NewBoard) :-
    nth0(I, Board, Row), % get the row at index I
    replace(Row, J, NewPiece, NewRow),
    replace(Board, I, NewRow, NewBoard).
place_piece( _, _, _, _, _, _, _):- !, fail.



flip_left(Ally, LineIndex, ColumnIndex, Board, Board) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),!.
    %update_gameState(Board),!.
flip_left(Ally, LineIndex, ColumnIndex, Board, NewestBoard) :-
    ColumnIndex>0,
    get_state(Ally, Enemy),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    ColumnIndex1 is ColumnIndex - 1,
    flip_left(Ally, LineIndex, ColumnIndex1, NewBoard, NewestBoard).
flip_left(_, _, 1, _, _).
/*
flip_right(Ally, LineIndex, ColumnIndex, Board, _) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),
    update_gameState(Board),!.
flip_right(Ally, LineIndex, ColumnIndex, Board, Columns) :-
    ColumnIndex<Columns,
    get_state(Ally, Enemy),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    ColumnIndex1 is ColumnIndex + 1,
    flip_right(Ally, LineIndex, ColumnIndex1, NewBoard, Columns).
flip_right(_, _, Columns, _, Columns).

flip_up(Ally, LineIndex, ColumnIndex, Board) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),
    update_gameState(Board),!.
flip_up(Ally, LineIndex, ColumnIndex, Board) :-
    LineIndex>0,
    get_state(Ally, Enemy),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    LineIndex1 is LineIndex - 1,
    flip_up(Ally, LineIndex1, ColumnIndex, NewBoard).
flip_up(_, _, 1, _).

flip_down(Ally, LineIndex, ColumnIndex, Board, _) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),
    update_gameState(Board),!.
flip_down(Ally, LineIndex, ColumnIndex, Board, Rows) :-
    LineIndex>Rows,
    get_state(Ally, Enemy),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    LineIndex1 is LineIndex + 1,
    flip_down(Ally, LineIndex1, ColumnIndex, NewBoard, Rows).
flip_down(_, _, Rows, _, Rows).
*/
flip(Board, Rows, Columns, Ally, LineIndex, ColumnIndex, NewBoard) :-
    ColumnIndex1 is ColumnIndex - 1,
    get_state(Ally, Enemy),
    get_piece(Board, LineIndex, ColumnIndex1, Enemy),
    flip_left(State, LineIndex, ColumnIndex1, Board, NewBoard).
    /*
flip(State, LineIndex, ColumnIndex) :-
    get_gameState(Board, _, Columns),
    ColumnIndex1 is ColumnIndex + 1,
    get_state(State, Enemy),
    get_piece(Board, LineIndex, ColumnIndex1, Enemy),
    flip_right(State, LineIndex, ColumnIndex1, Board , Columns),
    fail.
flip(State, LineIndex, ColumnIndex) :-
    get_gameState(Board, _, _),
    LineIndex1 is LineIndex - 1,
    get_state(State, Enemy),
    get_piece(Board, LineIndex1, ColumnIndex, Enemy),
    flip_up(State, LineIndex1, ColumnIndex, Board),
    fail.
flip(State, LineIndex, ColumnIndex) :-
    get_gameState(Board, Rows, _),
    LineIndex1 is LineIndex + 1,
    get_state(State, Enemy),
    get_piece(Board, LineIndex1, ColumnIndex, Enemy),
    flip_down(State, LineIndex1, ColumnIndex, Board , Rows),
    fail.*/
flip(Board,_,_,_,_,_,Board).



% game_over(+State)
% Checks if there is a win condition for the given state
game_over(gameState(Board, Rows, Columns, PlayerPiece),LineIndex-ColumnIndex,PlayerPiece) :-
    write('test'),
    check_horizontal(Board, LineIndex, ColumnIndex, PlayerPiece, Columns),
    nl,write(PlayerPiece),
    write(' horizontal win!'),
    !.

game_over(gameState(Board, Rows, Columns, PlayerPiece),LineIndex-ColumnIndex,PlayerPiece) :-
    RowNumber is Rows - 1,
    check_vertical(Board, 2, ColumnIndex, PlayerPiece, RowNumber),
    nl,write(PlayerPiece),
    write(' vertical win!').


% check_horizontal(+Board, +I, +J, +State, +Columns)
% Checks if there is a continuous horizontal segment from one side of the board to the other
check_horizontal(Board, I, J, State, Columns) :-
    my_forall(1, Columns - 2, get_piece(Board, I, J, State)).

% check_vertical(+Board, +I, +J, +State, +Rows)
% Checks if there is a continuous vertical segment from one side of the board to the other
check_vertical(Board, I, J, State, Rows) :-
    I < Rows,
    get_piece(Board, I, J, State),
    I1 is I + 1,
    check_vertical(Board, I1, J, State, Rows).
check_vertical(_, Rows, _, _, Rows).

move(gameState(Board, Rows, Columns, PlayerPiece), LineIndex-ColumnIndex, gameState(NewestBoard, Rows, Columns, PlayerPiece)):-
    validate_move(Board, LineIndex, ColumnIndex),
    place_piece(Board, Rows, Columns, PlayerPiece, LineIndex, ColumnIndex, NewestBoard),
    nl,write('Newest Game State: '),
    write(NewestBoard),
    asserta(NewestBoard).
    %flip(NewBoard, Rows, Columns, PlayerPiece, LineIndex, ColumnIndex, NewestBoard),!.

get_state(gameState(Board, Rows, Columns, PlayerPiece), gameState(Board, Rows, Columns, NewPlayerPiece)):-
    get_state(PlayerPiece, NewPlayerPiece).
get_state('R', 'B').
get_state('B', 'R').

