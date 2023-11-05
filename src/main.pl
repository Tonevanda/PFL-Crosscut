:- consult('board.pl').

read_input(Rows-Columns, BlueLevel, RedLevel):-
    repeat,
    write('Please insert the board size: '), nl,
    write('Rows (5-9): '),
    read_number(Rows),
    Rows >= 5, Rows =< 9, 
    write('Columns (5-9): '),
    read_number(Columns),
    Columns >= 5, Columns =< 9,
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
    maplist(=(Row), Board). % fill the list with the row


check_win(Piece, LineIndex-ColumnIndex) :-
    get_game_state(Board, Rows, Columns, _, _),
    check_horizontal(Board, LineIndex, ColumnIndex, Piece, Columns),
    nl,write(Piece),
    write(' horizontal win!'),
    retractall(gameState(Board, Rows, Columns, _, _)),
    !.

check_win(Piece, _-ColumnIndex) :-
    get_game_state(Board, Rows, Columns, _, _),
    RowNumber is Rows - 1,
    check_vertical(Board, 1, ColumnIndex, Piece, RowNumber),
    write(Board),
    nl,write(Piece),
    write(' vertical win!'),
    retractall(gameState(Board, Rows, Columns, _, _)),
    !.


check_horizontal(Board, I, J, Piece, Columns) :-
    my_forall(1, Columns - 2, get_piece(Board, I, J, Piece)).


check_vertical(Board, I, J, Piece, Rows) :-
    I < Rows,
    get_piece(Board, I, J, Piece),
    write(Piece),
    write(' found at: Row '),
    write(I),
    write(',Collumn '),
    write(J),
    nl,
    I1 is I + 1,
    check_vertical(Board, I1, J, Piece, Rows).
check_vertical(_, Rows, _, _, Rows).

move(Piece, Move, Level, NewState, NewLevel):-
    repeat,
    get_game_state(Board, _, _, _, _),
    write('Current level: '),
    write(Level),nl,
    choose_move(Board, Piece, Level, Move),
    validate_move(Board, Move, Piece, NewBoard),
    update_game_state(NewBoard), 
    get_state(Piece, NewState,  NewLevel),
    !.
    
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
choose_move(Board, Piece, 2, Move):- %Só entra neste caso o de cima falhar, ou seja ListOfMoves está vazia, que em princípio só acontece quando o hard AI não consegue encontrar um movimento que aumente o seu maior segmento
    better_valid_moves(Board, Piece, ListOfMoves), 
    random_member(Move, ListOfMoves).

get_state('R', 'B',BlueLevel):-
    get_game_state(_, _, _, BlueLevel,_).

get_state('B', 'R',RedLevel):-
    get_game_state(_, _, _, _, RedLevel).

% Predicate to start the game
play :-
    read_input(Size, BlueLevel, RedLevel),
    initial_state(Size, BlueLevel, RedLevel, GameState),
    asserta(GameState),
    display_board,!,
    gameLoop('R',RedLevel).

gameLoop(Piece,Level) :- 
    move(Piece, Move, Level,NewState, NewLevel),
    display_board,
    !, % cut to avoid backtracking
    \+check_win(Piece, Move), % if there is no win condition, continue the game
    gameLoop(NewState,NewLevel).
