:- consult('board.pl').

play :-
    retractall(gameState(_, _, _, _, _)),
    write('Welcome to CROSSCUT'), nl,
    read_initial_input(Size, BlueLevel, RedLevel),
    initial_state(Size, BlueLevel, RedLevel, GameState),
    display_game(GameState),!,
    game_loop('R',RedLevel).

game_loop(Piece,Level) :- 
    %write('\e[0m'),
    get_game_state(Board, Rows, Columns, _, _),
    move(Board, Piece, Level, Move,NewBoard),
    display_game(gameState(NewBoard, Rows, Columns,_,_)),
    !, % cut to avoid backtracking
    \+game_over(NewBoard, Piece, Move), % if there is no win condition, continue the game
    update_game_state(NewBoard), 
    get_next_state(Piece, NewState,  NewLevel),
    game_loop(NewState,NewLevel).

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
    write('Blue piece is: \n0: Human\n1: Easy\n2: Hard\n'),
    read_number(BlueLevel),
    BlueLevel >= 0, BlueLevel =< 2,
    write('Red piece is: \n0: Human\n1: Easy\n2: Hard\n'),
    read_number(RedLevel),
    RedLevel >= 0, RedLevel =< 2.

initial_state(Rows-Columns, BlueLevel, RedLevel,gameState(Board,Rows,Columns,BlueLevel,RedLevel)) :-
    length(Row, Columns), % create a row of length M
    maplist(=(' '), Row), % fill the row with empty pieces
    length(Board, Rows), % create a list of N rows
    maplist(=(Row), Board), % fill the list with the row
    asserta(gameState(Board,Rows,Columns,BlueLevel,RedLevel)).
move(Board, Piece, Level, Move, NewBoard):-
    repeat,
    choose_move(Board, Piece, Level, Move),
    validate_move(Board, Move, Piece, NewBoard).
    
game_over(Board, Piece, LineIndex-ColumnIndex) :-
    get_game_state(_, Rows, Columns, _, _),
    check_horizontal(Board, LineIndex, ColumnIndex, Piece, Columns),
    nl,write(Piece),
    write(' horizontal win!'),
    retractall(gameState(Board, Rows, Columns, _, _)),
    !.

game_over(Board, Piece, _-ColumnIndex) :-
    get_game_state(_, Rows, Columns, _, _),
    RowNumber is Rows - 1,
    check_vertical(Board, 1, ColumnIndex, Piece, RowNumber),
    nl,write(Piece),
    write(' vertical win!'),
    retractall(gameState(Board, Rows, Columns, _, _)),
    !.

check_win(Board, Piece, LineIndex-ColumnIndex) :-
    get_game_state(_, _, Columns, _, _),
    check_horizontal(Board, LineIndex, ColumnIndex, Piece, Columns),
    nl,write(Piece),
    !.

check_win(Board,  Piece, _-ColumnIndex) :-
    get_game_state(_, Rows, _, _, _),
    RowNumber is Rows - 1,
    check_vertical(Board, 1, ColumnIndex, Piece, RowNumber),
    nl,write(Piece),
    !.


check_horizontal(Board, I, J, Piece, Columns) :-
    forall(1, Columns - 2, get_piece(Board, I, J, Piece)).


check_vertical(Board, I, J, Piece, Rows) :-
    I < Rows,
    get_piece(Board, I, J, Piece),
    nl,
    I1 is I + 1,
    check_vertical(Board, I1, J, Piece, Rows).
check_vertical(_, Rows, _, _, Rows).


compare_moves(Order, I1-J1, I2-J2) :-
    get_game_state(Board, Rows, Columns),
    place_piece(Board, I1, J1, Piece, NewBoard1),
    value(NewBoard1, Piece, Rows, Columns, Value1),
    place_piece(Board, I2, J2, Piece, NewBoard2),
    value(NewBoard2, Piece, Rows, Columns, Value2),
    compare(Order, Value1, Value2).

choose_move(_, Piece, 0, LineIndex-ColumnIndex):-
    nl,
    write(Piece),
    write(', select the row:'), nl,
    read_letter(LineIndex),
    write(Piece),
    write(', select the column:'), nl,
    read_number(ColumnIndex).
% choose_move(+Board, +Piece, +AILevel, -Move)
% Chooses a move for the AI
choose_move(Board, Piece, 1, Move):- %Se falhar quer dizer que ListOfMoves é vazia, pode ser necessário fazer uma cena que acabe o jogo se nao der para fazer mais movimentos, mas nao sei se é possivel neste jogo acabar em draw
    valid_moves(Board, Piece, 1, ListOfMoves),
    random_member(Move, ListOfMoves).
% Apercebi-me agora que esta forma é meia à bruta, porque se não encontrar um movimento que aumente o maior segmento, vai escolher um movimento aleatório, 
% mas tecnicamente a melhor jogada possível seria tentar aumentar o segundo maior segmento, seria muito mais dificil fazer isso tho acho
% Para fazermos essa de depois meter no segundo melhor ig que teriamos que mudar o find_longest_segment todo para ele retornar uma lista sorted por tamanho 
% de segmento, e depois era so ir buscar o segundo elemento da lista e por ai fora até funcionar, se nunca conseguir então acaba por escolher um movimento aleatório

choose_move(Board, Piece, 2, Move):-
    get_winning_moves(Board, Piece, ListOfMoves),
    random_member(Move, ListOfMoves).

choose_move(Board, Piece, 2, Move):- 
    get_enemy_winning_moves(Board, Piece, ListOfMoves), 
    random_member(Move, ListOfMoves).
    
choose_move(Board, Piece, 2, Move):- %Se falhar quer dizer que ListOfMoves é vazia, pode ser necessário fazer uma cena que acabe o jogo se nao der para fazer mais movimentos, mas nao sei se é possivel neste jogo acabar em draw
    valid_moves(Board, Piece, 2, ListOfMoves),
    predsort(compare_moves, ListOfMoves, SortedMoves),
    nth0(0, SortedMoves, Move). %nth0(0, ListOfMoves, Move) vai buscar o primeiro elemento da lista, que é o que tem o menor
    %random_member(Move, ListOfMoves).

    
choose_move(Board, Piece, 2, Move):-
    valid_moves(Board, Piece, 1, ListOfMoves),
    random_member(Move, ListOfMoves).

get_next_state('R', 'B',BlueLevel):-
    get_game_state(_, _, _, BlueLevel,_).

get_next_state('B', 'R',RedLevel):-
    get_game_state(_, _, _, _, RedLevel).

