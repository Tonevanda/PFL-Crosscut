% FILEPATH: /Users/tonevanda/Documents/GitHub/PFL-Crosscut/src/board.pl

:- use_module(library(random)).
:- use_module(library(lists)).
:- consult('utils.pl').
:- consult('flip.pl').
:- consult('computer.pl').
:- dynamic gameState/5.

get_game_state(Board, Rows, Columns, BlueLevel, RedLevel) :-
    gameState(Board, Rows, Columns, BlueLevel, RedLevel).
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
    get_game_state(Board,_,Columns,_,_),
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
    get_game_state(Board, Rows, Columns,_,_),
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

% place_piece(+Board, +I, +J, +NewPiece, -NewBoard)
% Places the piece at row I and column J of the board
place_piece(Board,I-J, NewPiece,NewBoard) :-
    nth0(I, Board, Row), % get the row at index I
    replace(Row, J, NewPiece, NewRow),
    replace(Board, I, NewRow, NewBoard).

% is_empty(+Board, +I, +J)
% Checks if the position at row I and column J of the board is empty
is_empty(Board, I, J) :-
    get_piece(Board, I, J, ' '), !.

% not_edge(+Board, +I, +J)
% Checks if the position at row I and column J of the board is not at the edge
not_edge(Board, I, J) :-
    length(Board, N), % get the number of rows
    nth0(I, Board, Row), % get the row at index I
    length(Row, M), % get the number of columns
    I > 0, I < N - 1, % check if the row index is not at the edge
    J > 0, J < M - 1. % check if the column index is not at the edge


% validate_move(+Board,+I, +J, +State)
% Checks if it is an edge move results in disc flipping
validate_move(Board, I-J, State, NewBoard) :-
    write('Getting piece'),nl,
    write('I-J: '), write(I-J),nl,
    is_empty(Board, I, J),
    write('Validating move1'),nl,
    \+not_edge(Board, I, J),!, % check if the position is at the edge
    flip(Board, State, I-J, NewBoard),nl,
    write('Piece was attempted to be placed on edge'),nl.

% validate_move(+Board,+I, +J, +State)
% Checks if it is not an edge move and results in flipping 
validate_move(Board, I-J,State, NewestBoard) :-
    is_empty(Board, I, J),
    not_edge(Board, I, J),
    place_piece(Board, I-J, State, NewBoard),nl,
    write('Not on an edge'),nl,
    flip(NewBoard, State, I-J, NewestBoard),!.

% validate_move(+Board,+I, +J, +State)
% Checks if it is not an edge move
validate_move(Board, I-J,State, NewBoard) :-
    is_empty(Board, I, J),
    not_edge(Board, I, J),
    place_piece(Board, I-J, State, NewBoard),nl,
    write('Not on an edge'),nl.
