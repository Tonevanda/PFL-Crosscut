:- consult('board.pl').


% initial_state(+Size, -GameState)
initial_state(Rows-Columns,gameState(Board,Rows,Columns,PlayerPiece)) :-
    PlayerPiece = 'R',
    length(Row, Columns), % create a row of length M
    maplist(=(' '), Row), % fill the row with empty pieces
    length(Board, Rows), % create a list of N rows
    maplist(=(Row), Board), % fill the list with the row
    write(Board).

read_input(Rows-Columns):-
    repeat,
    write('Please insert the board size: '), nl,
    write('Rows (5-9): '),
    read_number(Rows),
    Rows >= 5, Rows =< 9, 
    write('Columns (5-9): '),
    read_number(Columns),
    Columns >= 5, Columns =< 9.

alt_read_input(LineIndex-ColumnIndex):-
    %write(State),
    write(', select the row:'), nl,
    read_letter(LineIndex),
    %write(State),
    write(', select the column:'), nl,
    read_number(ColumnIndex).

% Predicate to start the game
play :-
    retractall(gameState(_)),
    read_input(Size),
    initial_state(Size,GameState),
    display_game(GameState),!,
    asserta(GameState),
    gameLoop.


gameLoop :- 
    repeat,
    get_gameState(GameState),
    retractall(gameState(_)),
    %write('Game State: '),nl,
    %write(GameState),
    alt_read_input(Move),
    move(GameState, Move, NewGameState),
    display_game(NewGameState),
    get_state(NewGameState, NewestGameState),
    %nl,write('Newest Game State: '),
    %write(NewestGameState),
    %asserta(NewestGameState),
    game_over(NewGameState,Move,Winner). %isto aqui era para ver se check win funcionava, nao podemos usar isto

