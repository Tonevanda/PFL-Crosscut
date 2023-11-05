
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