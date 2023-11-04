:- consult('board.pl').


read_input(Rows-Columns):-
    repeat,
    write('Please insert the board size: '), nl,
    write('Rows (5-9): '),
    read_number(Rows),
    Rows >= 5, Rows =< 9, 
    write('Columns (5-9): '),
    read_number(Columns),
    Rows >= 5, Rows =< 9.


% create_board(-Rows, -Columns)
% Prompts the user for the board size
% then creates a board with the given size
initial_state(Rows-Columns,gameState(Board,Rows,Columns)) :-
    length(Row, Columns), % create a row of length M
    maplist(=(' '), Row), % fill the row with empty pieces
    length(Board, Rows), % create a list of N rows
    maplist(=(Row), Board). % fill the list with the row

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

% Predicate to start the game
play :-
    read_input(Size),
    initial_state(Size,GameState),
    asserta(GameState),
    display_board,!,
    gameLoop('R').

gameLoop(State) :- 
    move(State, LineIndex, ColumnIndex),
    %flip(State, LineIndex, ColumnIndex),
    display_board,!,
    \+check_win(State, LineIndex, ColumnIndex), %isto aqui era para ver se check win funcionava, nao podemos usar isto
    get_state(State, NewState),
    gameLoop(NewState).
