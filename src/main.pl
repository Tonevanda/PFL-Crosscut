:- consult('board.pl').


% create_board(-Rows, -Columns)
% Prompts the user for the board size
% then creates a board with the given size
create_board(info(Board,N,M)) :-
    repeat,
    write('Please insert the board size: '), nl,
    write('Rows (5-9): '),
    read_number(N),
    N >= 5, N =< 9, 
    write('Columns (5-9): '),
    read_number(M),
    M >= 5, M =< 9,
    length(Row, M), % create a row of length M
    maplist(=(' '), Row), % fill the row with empty pieces
    length(Board, N), % create a list of N rows
    maplist(=(Row), Board). % fill the list with the row

% Predicate to start the game
play :-
    %clear_buffer,
    create_board(Struct),
    %create_board(Struct),
    asserta(Struct),
    gameLoop('R').
    %create_board(5, 5, Board),
    %info('R',Board,5,5),
    %gameLoop('R', 5, 5, Board).
    %get_piece(Board, 1, 1, Element),
    %write(Element), nl.

gameLoop(State) :- 
    display_board,
    nl,
    write(State),
    move(State, LineIndex, ColumnIndex),
    %(check_win(State, LineIndex, ColumnIndex) -> write('WIN'), nl ; true), isto aqui era para ver se check win funcionava, nao podemos usar isto
    get_state(State, NewState),
    gameLoop(NewState).