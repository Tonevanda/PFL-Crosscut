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
