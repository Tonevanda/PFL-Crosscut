flip_check_up(Board, Enemy, LineIndex, ColumnIndex, Accumulator):-
    LineIndex>0,
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    LineIndex1 is LineIndex - 1,
    flip_check_up(Board, Enemy, LineIndex1, ColumnIndex, Accumulator1),
    Accumulator is Accumulator1 + 1.
flip_check_up(_,_,0,_,0).
flip_check_down(Board, Enemy, LineIndex, ColumnIndex, Accumulator):-
    get_game_state(_, Rows, _, _, _),
    LineIndex<Rows,
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    LineIndex1 is LineIndex + 1,
    flip_check_down(Board, Enemy, LineIndex1, ColumnIndex, Accumulator1),
    Accumulator is Accumulator1 + 1.
flip_check_down(_,_,_,_,0).
flip_check_vertical(Board, Enemy, LineIndex, ColumnIndex, Longestinput, Longestoutput):-
    LineIndexUp is LineIndex - 1,
    LineIndexDown is LineIndex + 1,
    flip_check_up(Board, Enemy, LineIndexUp, ColumnIndex, AccumulatorUp),
    flip_check_down(Board, Enemy, LineIndexDown, ColumnIndex, AccumulatorDown),
    Longestoutput is AccumulatorDown + AccumulatorUp + 1,
    Longestoutput > Longestinput.
flip_check_vertical(_,_,_,_,Longestinput,Longestinput).

flip_check_left(Board, Enemy, LineIndex, ColumnIndex, Accumulator):-
    ColumnIndex>0,
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    ColumnIndex1 is ColumnIndex - 1,
    flip_check_left(Board, Enemy, LineIndex, ColumnIndex1, Accumulator1),
    Accumulator is Accumulator1 + 1.
flip_check_left(_,_,_,0,0).
flip_check_right(Board, Enemy, LineIndex, ColumnIndex, Accumulator):-
    get_game_state(_, _, Columns, _, _),
    ColumnIndex<Columns,
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    ColumnIndex1 is ColumnIndex + 1,
    flip_check_right(Board, Enemy, LineIndex, ColumnIndex1, Accumulator1),
    Accumulator is Accumulator1 + 1.
flip_check_right(_,_,_,_,0).

flip_check_horizontal(Board, Enemy, LineIndex, ColumnIndex, Longestinput, Longestoutput):-
    ColumnIndexLeft is ColumnIndex - 1,
    ColumnIndexRight is ColumnIndex + 1,
    flip_check_left(Board, Enemy, LineIndex, ColumnIndexLeft, AccumulatorLeft),
    flip_check_right(Board, Enemy, LineIndex, ColumnIndexRight, AccumulatorRight),
    Longestoutput is AccumulatorLeft + AccumulatorRight + 1,
    Longestoutput > Longestinput.
flip_check_horizontal(_,_,_,_,Longestinput,Longestinput).


flip_left(Ally, LineIndex, ColumnIndex, Board, Board, Longest, Accumulator) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),
    write(' basecase '),
    write('longest: '),write(Longest),nl,
    write('Accumulator: '),write(Accumulator),nl,!,
    Accumulator > Longest.
flip_left(Ally, LineIndex, ColumnIndex, Board, NewestBoard, Longest, Accumulator) :-
    ColumnIndex>0,
    get_state(Ally, Enemy, _),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),

    flip_check_vertical(Board, Enemy, LineIndex, ColumnIndex, Longest, Longestoutput),

    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    ColumnIndex1 is ColumnIndex - 1,
    Accumulator1 is Accumulator + 1,!,
    flip_left(Ally, LineIndex, ColumnIndex1, NewBoard, NewestBoard, Longestoutput, Accumulator1).

flip_right(Ally, LineIndex, ColumnIndex, Board, _, Board, Longest, Accumulator) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),!,
    Accumulator > Longest.
flip_right(Ally, LineIndex, ColumnIndex, Board, Columns, NewestBoard, Longest, Accumulator) :-
    ColumnIndex<Columns,
    get_state(Ally, Enemy, _),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),

    flip_check_vertical(Board, Enemy, LineIndex, ColumnIndex, Longest, Longestoutput),

    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    ColumnIndex1 is ColumnIndex + 1,
    Accumulator1 is Accumulator + 1,!,
    flip_right(Ally, LineIndex, ColumnIndex1, NewBoard, Columns, NewestBoard, Longestoutput, Accumulator1).

flip_up(Ally, LineIndex, ColumnIndex, Board, Board, Longest, Accumulator) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),!,
    Accumulator > Longest.
flip_up(Ally, LineIndex, ColumnIndex, Board, NewestBoard, Longest, Accumulator) :-
    LineIndex>0,
    get_state(Ally, Enemy, _),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    flip_check_horizontal(Board, Enemy, LineIndex, ColumnIndex, Longest, Longestoutput),
    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    LineIndex1 is LineIndex - 1,
    Accumulator1 is Accumulator + 1,!,
    flip_up(Ally, LineIndex1, ColumnIndex, NewBoard, NewestBoard, Longestoutput, Accumulator1).


flip_down(Ally, LineIndex, ColumnIndex, Board, _, Board, Longest, Accumulator) :-
    get_piece(Board, LineIndex, ColumnIndex, Ally),!,
    Accumulator > Longest.
flip_down(Ally, LineIndex, ColumnIndex, Board, Rows, NewestBoard, Longest, Accumulator) :-
    LineIndex<Rows,
    get_state(Ally, Enemy, _),
    get_piece(Board, LineIndex, ColumnIndex, Enemy),
    flip_check_horizontal(Board, Enemy, LineIndex, ColumnIndex, Longest, Longestoutput),
    nth0(LineIndex, Board, Row), % get the row at index I
    replace(Row, ColumnIndex, Ally, NewRow),
    replace(Board, LineIndex, NewRow, NewBoard),
    LineIndex1 is LineIndex + 1,
    Accumulator1 is Accumulator + 1,!,
    flip_down(Ally, LineIndex1, ColumnIndex, NewBoard, Rows, NewestBoard, Longestoutput, Accumulator1).

flip(Board, State, LineIndex-ColumnIndex, NewBoard) :-
    ColumnIndex1 is ColumnIndex - 1,
    get_state(State, Enemy, _),
    get_piece(Board, LineIndex, ColumnIndex1, Enemy),
    flip_left(State, LineIndex, ColumnIndex1, Board, NewBoard, 0, 2).
flip(Board, State, LineIndex-ColumnIndex, NewBoard) :-
    get_game_state(_, _, Columns, _, _),
    ColumnIndex1 is ColumnIndex + 1,
    get_state(State, Enemy, _),
    get_piece(Board, LineIndex, ColumnIndex1, Enemy),
    flip_right(State, LineIndex, ColumnIndex1, Board, Columns, NewBoard, 0, 2).
flip(Board, State, LineIndex-ColumnIndex, NewBoard) :-
    LineIndex1 is LineIndex - 1,
    get_state(State, Enemy, _),
    get_piece(Board, LineIndex1, ColumnIndex, Enemy),
    flip_up(State, LineIndex1, ColumnIndex, Board, NewBoard,0,2).
flip(Board, State, LineIndex-ColumnIndex, NewBoard) :-
    get_game_state(_, Rows, _, _, _),
    LineIndex1 is LineIndex + 1,
    get_state(State, Enemy, _),
    get_piece(Board, LineIndex1, ColumnIndex, Enemy),
    flip_down(State, LineIndex1, ColumnIndex, Board , Rows, NewBoard, 0, 2).
flip(Board,_,_,Board):-write('nuhuh'),fail.