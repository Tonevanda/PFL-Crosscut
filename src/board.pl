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

/*
valid_moves(gameState(Board,Rows,Columns), Player, ValidMoves) :-
    valid_moves_aux(Board, Rows, Columns, State, 0, 0, ValidMoves).
*/
validate_move(Board, I, J,State, NewBoard) :-
    is_empty(Board, I, J),
    \+not_edge(Board, I, J),!, % check if the position is at the edge´
    flip(Board, State, I, J, NewBoard),
    write('piece was atempted to be placed on edge'),nl.

validate_move(Board, I, J,State, NewestBoard) :-
    is_empty(Board, I, J),
    not_edge(Board, I, J),
    place_piece(Board, I, J, State, NewBoard),
    write('not on an edge'),nl,
    flip(NewBoard, State, I, J, NewestBoard),!.

validate_move(Board, I, J,State, NewBoard) :-
    is_empty(Board, I, J),
    not_edge(Board, I, J),
    place_piece(Board, I, J, State, NewBoard),
    write('not on an edge'),nl.
    
% place_piece(+Board, +I, +J, +NewPiece, -NewBoard)
% Places the piece at row I and column J of the board
place_piece(Board,I, J, NewPiece,NewBoard) :-
    nth0(I, Board, Row), % get the row at index I
    replace(Row, J, NewPiece, NewRow),
    replace(Board, I, NewRow, NewBoard).


flip_check_up(Board, Enemy, LineIndex, ColumnIndex, Acumulator):-
    LineIndex>0,
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    LineIndex1 is LineIndex - 1,
    flip_check_up(Board, Enemy, LineIndex1, ColumnIndex, Acumulator1),
    Acumulator is Acumulator1 + 1.
flip_check_up(_,_,0,_,0).
flip_check_down(Board, Enemy, LineIndex, ColumnIndex, Acumulator):-
    get_game_state(_, Rows, _),
    LineIndex<Rows,
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    LineIndex1 is LineIndex + 1,
    flip_check_down(Board, Enemy, LineIndex1, ColumnIndex, Acumulator1),
    Acumulator is Acumulator1 + 1.
flip_check_down(_,_,_,_,0).
flip_check_vertical(Board, Enemy, LineIndex, ColumnIndex, Longestinput, Longestoutput):-
    LineIndexUp is LineIndex - 1,
    LineIndexDown is LineIndex + 1,
    flip_check_up(Board, Enemy, LineIndexUp, ColumnIndex, AcumulatorUp),
    flip_check_down(Board, Enemy, LineIndexDown, ColumnIndex, AcumulatorDown),
    Longestoutput is AcumulatorDown + AcumulatorUp + 1,
    Longestoutput > Longestinput.
flip_check_vertical(_,_,_,_,Longestinput,Longestinput).

flip_check_left(Board, Enemy, LineIndex, ColumnIndex, Acumulator):-
    ColumnIndex>0,
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    ColumnIndex1 is ColumnIndex - 1,
    flip_check_left(Board, Enemy, LineIndex, ColumnIndex1, Acumulator1),
    Acumulator is Acumulator1 + 1.
flip_check_left(_,_,_,0,0).
flip_check_right(Board, Enemy, LineIndex, ColumnIndex, Acumulator):-
    get_game_state(_, _, Columns),
    ColumnIndex<Columns,
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    ColumnIndex1 is ColumnIndex + 1,
    flip_check_right(Board, Enemy, LineIndex, ColumnIndex1, Acumulator1),
    Acumulator is Acumulator1 + 1.
flip_check_right(_,_,_,_,0).

flip_check_horizontal(Board, Enemy, LineIndex, ColumnIndex, Longestinput, Longestoutput):-
    ColumnIndexLeft is ColumnIndex - 1,
    ColumnIndexRight is ColumnIndex + 1,
    flip_check_left(Board, Enemy, LineIndex, ColumnIndexLeft, AcumulatorLeft),
    flip_check_right(Board, Enemy, LineIndex, ColumnIndexRight, AcumulatorRight),
    Longestoutput is AcumulatorLeft + AcumulatorRight + 1,
    Longestoutput > Longestinput.
flip_check_horizontal(_,_,_,_,Longestinput,Longestinput).


flip_left(Ally, LineIndex, ColumnIndex, Board, Board, Longest, Acumulator) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),
    write(' basecase '),
    write('longest: '),write(Longest),nl,
    write('acumulator: '),write(Acumulator),nl,!,
    Acumulator > Longest.
flip_left(Ally, LineIndex, ColumnIndex, Board, NewestBoard, Longest, Acumulator) :-
    ColumnIndex>0,
    get_state(Ally, Enemy),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),

    flip_check_vertical(Board, Enemy, LineIndex, ColumnIndex, Longest, Longestoutput),

    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    ColumnIndex1 is ColumnIndex - 1,
    Acumulator1 is Acumulator + 1,!,
    flip_left(Ally, LineIndex, ColumnIndex1, NewBoard, NewestBoard, Longestoutput, Acumulator1).

flip_right(Ally, LineIndex, ColumnIndex, Board, _, Board, Longest, Acumulator) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),!,
    Acumulator > Longest.
flip_right(Ally, LineIndex, ColumnIndex, Board, Columns, NewestBoard, Longest, Acumulator) :-
    ColumnIndex<Columns,
    get_state(Ally, Enemy),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),

    flip_check_vertical(Board, Enemy, LineIndex, ColumnIndex, Longest, Longestoutput),

    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    ColumnIndex1 is ColumnIndex + 1,
    Acumulator1 is Acumulator + 1,!,
    flip_right(Ally, LineIndex, ColumnIndex1, NewBoard, Columns, NewestBoard, Longestoutput, Acumulator1).

flip_up(Ally, LineIndex, ColumnIndex, Board, Board, Longest, Acumulator) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),!,
    Acumulator > Longest.
flip_up(Ally, LineIndex, ColumnIndex, Board, NewestBoard, Longest, Acumulator) :-
    LineIndex>0,
    get_state(Ally, Enemy),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    flip_check_horizontal(Board, Enemy, LineIndex, ColumnIndex, Longest, Longestoutput),
    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    LineIndex1 is LineIndex - 1,
    Acumulator1 is Acumulator + 1,!,
    flip_up(Ally, LineIndex1, ColumnIndex, NewBoard, NewestBoard, Longestoutput, Acumulator1).


flip_down(Ally, LineIndex, ColumnIndex, Board, _, Board, Longest, Acumulator) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),!,
    Acumulator > Longest.
flip_down(Ally, LineIndex, ColumnIndex, Board, Rows, NewestBoard, Longest, Acumulator) :-
    LineIndex>Rows,
    get_state(Ally, Enemy),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    flip_check_horizontal(Board, Enemy, LineIndex, ColumnIndex, Longest, Longestoutput),
    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    LineIndex1 is LineIndex + 1,
    Acumulator1 is Acumulator + 1,!,
    flip_down(Ally, LineIndex1, ColumnIndex, NewBoard, Rows, NewestBoard, Longestoutput, Acumulator1).

flip(Board, State, LineIndex, ColumnIndex, NewBoard) :-
    ColumnIndex1 is ColumnIndex - 1,
    get_state(State, Enemy),
    get_piece(Board, LineIndex, ColumnIndex1, Enemy),
    flip_left(State, LineIndex, ColumnIndex1, Board, NewBoard, 0, 2).
flip(Board, State, LineIndex, ColumnIndex, NewBoard) :-
    get_game_state(_, _, Columns),
    ColumnIndex1 is ColumnIndex + 1,
    get_state(State, Enemy),
    get_piece(Board, LineIndex, ColumnIndex1, Enemy),
    flip_right(State, LineIndex, ColumnIndex1, Board, Columns, NewBoard,0,2).
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
    validate_move(Board, LineIndex, ColumnIndex, State, NewBoard),
    update_game_state(NewBoard),
    !.
    
get_state('R', 'B').

get_state('B', 'R').

