:- consult('board.pl').

% play
% Starts the game
play :-
    retractall(gameState(_, _, _, _, _)),
    write('Welcome to CROSSCUT'), nl,
    read_initial_input(Size, BlueLevel, RedLevel),
    initial_state(Size, BlueLevel, RedLevel, GameState),
    display_game(GameState),!,
    nl,nl,write('Press enter to continue'),get_char(_),nl,
    game_loop('R',RedLevel).

% print_played_move(+Piece, +Move)
print_played_move(Piece, I-J) :-
    I1 is I + 65,
    char_code(Char, I1),
    nl,write(Piece),write(' played at '),write(Char),write(J),
    nl,nl,write('Press enter to continue'),get_char(_),nl.

% game_loop(+Piece, +Level)
% Main game loop
game_loop(Piece,Level) :- 
    get_game_state(Board, Rows, Columns, _, _),
    move(Board, Piece, Level, Move, NewBoard),
    display_game(gameState(NewBoard, Rows, Columns,_,_)),
    !, % cut to avoid backtracking
    \+game_over(NewBoard, Piece, Move), % if there is no win condition, continue the game
    print_played_move(Piece, Move),
    update_game_state(NewBoard), 
    get_next_state(Piece, NewState,  NewLevel),
    game_loop(NewState,NewLevel).

% read_initial_input(-Size, -BlueLevel, -RedLevel)
% Reads the initial input from the user to create the initial game state
read_initial_input(Rows-Columns, BlueLevel, RedLevel):-
    repeat,
    write('Please insert the board size: '), nl,
    write('Rows (5-9): '),
    read_number(Rows),
    Rows >= 5, Rows =< 9, 
    write('Columns (5-9): '),
    read_number(Columns),
    Columns >= 5, Columns =< 9,
    repeat,
    write('Red piece is: \n0: Human\n1: Easy\n2: Hard\n'),
    read_number(RedLevel),
    RedLevel >= 0, RedLevel =< 2,
    write('Blue piece is: \n0: Human\n1: Easy\n2: Hard\n'),
    read_number(BlueLevel),
    BlueLevel >= 0, BlueLevel =< 2.

% initial_state(+Size, +BlueLevel, +RedLevel, -GameState)
% Creates the initial game state
initial_state(Rows-Columns, BlueLevel, RedLevel,gameState(Board,Rows,Columns,BlueLevel,RedLevel)) :-
    length(Row, Columns), % create a row of length M
    maplist(=(' '), Row), % fill the row with empty pieces
    length(Board, Rows), % create a list of N rows
    maplist(=(Row), Board), % fill the list with the row
    asserta(gameState(Board,Rows,Columns,BlueLevel,RedLevel)).

% move(+Board, +Piece, +Level, -Move, -NewBoard)
% Gets the move from the player and validates it
move(Board, Piece, Level, Move, NewBoard):-
    repeat,
    choose_move(Board, Piece, Level, Move),
    validate_move(Board, Move, Piece, NewBoard).
    
% game_over(+Board, +Piece, +Move)
% Checks if there is a horizontal win condition
game_over(Board, Piece, LineIndex-ColumnIndex) :-
    get_game_state(_, Rows, Columns, _, _),
    check_horizontal(Board, LineIndex, ColumnIndex, Piece, Columns),
    nl,write(Piece),
    write(' horizontal win!'),
    retractall(gameState(Board, Rows, Columns, _, _)),
    !.

% game_over(+Board, +Piece, +Move)
% Checks if there is a vertical win condition
game_over(Board, Piece, _-ColumnIndex) :-
    get_game_state(_, Rows, Columns, _, _),
    RowNumber is Rows - 1,
    check_vertical(Board, 1, ColumnIndex, Piece, RowNumber),
    nl,write(Piece),
    write(' vertical win!'),
    retractall(gameState(Board, Rows, Columns, _, _)),
    !.

% check_win(+Board, +Piece, +Move)
% Checks if there is a horizontal win condition
check_win(Board, Piece, LineIndex-ColumnIndex) :-
    get_game_state(_, _, Columns, _, _),
    check_horizontal(Board, LineIndex, ColumnIndex, Piece, Columns),
    nl,write(Piece),
    !.

% check_win(+Board, +Piece, +Move)
% Checks if there is a vertical win condition
check_win(Board,  Piece, _-ColumnIndex) :-
    get_game_state(_, Rows, _, _, _),
    RowNumber is Rows - 1,
    check_vertical(Board, 1, ColumnIndex, Piece, RowNumber),
    nl,write(Piece),
    !.

%check_horizontal(+Board, +I, +J, +Piece, +Columns)
% Checks if there is a contiguous horizontal segment of Piece ranging from one side of the board to the other, except the edge
check_horizontal(Board, I, J, Piece, Columns) :-
    forall(1, Columns - 2, get_piece(Board, I, J, Piece)).

%check_vertical(+Board, +I, +J, +Piece, +Rows)
% Checks if there is a contiguous vertical segment of Piece ranging from one side of the board to the other, except the edge
check_vertical(Board, I, J, Piece, Rows) :-
    I < Rows,
    get_piece(Board, I, J, Piece),
    nl,
    I1 is I + 1,
    check_vertical(Board, I1, J, Piece, Rows).
check_vertical(_, Rows, _, _, Rows).

%compare_moves(+Order, +Move1, +Move2)
% Compares two moves based on their value
compare_moves(Order, I1-J1, I2-J2) :-
    get_game_state(Board, _,_, _, _),
    place_piece(Board, I1-J1, Piece, NewBoard1),
    value(NewBoard1, Piece, Value1),
    place_piece(Board, I2-J2, Piece, NewBoard2),
    value(NewBoard2, Piece, Value2),
    compare(Order, Value1, Value2).

% choose_move(+Board, +Piece, +Level, -Move)
% Chooses a move for the human player
choose_move(_, Piece, 0, LineIndex-ColumnIndex):-
    nl,
    write(Piece),
    write(', select the row:'), nl,
    read_letter(LineIndex),
    write(Piece),
    write(', select the column:'), nl,
    read_number(ColumnIndex).

% choose_move(+Board, +Piece, +AILevel, -Move)
% Chooses a move for the easy AI
choose_move(Board, Piece, 1, Move):- 
    valid_moves(Board, Piece, 1, ListOfMoves),
    random_member(Move, ListOfMoves).

% choose_move(+Board, +Piece, +AILevel, -Move)
% Chooses a move for the hard AI if there are currently winning moves
choose_move(Board, Piece, 2, Move):-
    write('winning moves'),nl,
    get_winning_moves(Board, Piece, ListOfMoves),
    write(ListOfMoves),nl,
    random_member(Move, ListOfMoves).

% choose_move(+Board, +Piece, +AILevel, -Move)
% Chooses a move for the hard AI if there are currently preventable enemy winning moves
choose_move(Board, Piece, 2, Move):- 
    write('enemy winning moves'),nl,
    get_enemy_winning_moves(Board, Piece, ListOfMoves), 
    write(ListOfMoves),nl,

    random_member(Move, ListOfMoves). 

% choose_move(+Board, +Piece, +AILevel, -Move)
% Chooses a move for the hard AI that decreases the value of its game state
choose_move(Board, Piece, 2, Move):- 
    write('decreasing value moves'),nl,
    valid_moves(Board, Piece, 2, ListOfMoves),
    write(ListOfMoves),nl,
    %predsort(compare_moves, ListOfMoves, SortedMoves),
    %nth0(0, SortedMoves, Move), %nth0(0, ListOfMoves, Move) vai buscar o primeiro elemento da lista, que é o que tem o menor
    random_member(Move, ListOfMoves).

% choose_move(+Board, +Piece, +AILevel, -Move)
% Chooses a move for the hard AI if there are no winning moves or preventable enemy winning moves and no moves that decrease the value of its game state
choose_move(Board, Piece, 2, Move):-    
    write('random moves'),nl,
    valid_moves(Board, Piece, 1, ListOfMoves),
    random_member(Move, ListOfMoves).

% get_next_state(+Piece, -NewState, -NewLevel)
% Gets the next state and level
get_next_state('R', 'B',BlueLevel):-
    get_game_state(_, _, _, BlueLevel,_).

% get_next_state(+Piece, -NewState, -NewLevel)
% Gets the next state and level
get_next_state('B', 'R',RedLevel):-
    get_game_state(_, _, _, _, RedLevel).

