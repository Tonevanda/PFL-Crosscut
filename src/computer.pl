
longest_segment(Board, Piece, LongestHorizontal) :-
    find_longest_horizontal(Board, Piece, LongestHorizontal).

longest_segment(Board, Piece, LongestVertical) :-
    find_longest_vertical(Board, Piece, LongestVertical).


find_longest_segment_in_row(Row, Piece, LongestSegment) :-
    find_longest_segment_in_list(Row, 0, 0, Piece, LongestSegment).


find_longest_segment_in_list([], _, MaxLength, _, MaxLength).
find_longest_segment_in_list([CurrentPiece|Rest], CurrentLength, MaxLength, CurrentPiece, LongestSegment) :-
    NewLength is CurrentLength + 1,
    NewMaxLength is max(NewLength, MaxLength),
    find_longest_segment_in_list(Rest, NewLength, NewMaxLength, CurrentPiece, LongestSegment).
find_longest_segment_in_list([Piece|Rest], _, MaxLength, _CurrentPiece, LongestSegment) :-
    find_longest_segment_in_list(Rest, 1, MaxLength, Piece, LongestSegment).

find_longest_horizontal(Board, Piece, LongestHorizontal) :-
    maplist(find_longest_segment_in_row(_Row, Piece, _LongestSegment), Board, Segments),
    max_list(Segments, LongestHorizontal).

find_longest_vertical(Board, Piece, LongestVertical) :-
    transpose(Board, TransposedBoard), % Swap rows and columns
    maplist(find_longest_segment_in_row(_Row, Piece, _LongestSegment), TransposedBoard, Segments),
    max_list(Segments, LongestVertical).

value(Board, Piece, Value) :-
    get_game_state(_, Rows, Columns),
    find_longest_horizontal(Board, Piece, LongestHorizontal),
    find_longest_vertical(Board, Piece, LongestVertical),
    HorizontalValue is min(LongestHorizontal, Columns - 2),
    VerticalValue is min(LongestVertical, Rows - 2),
    Value is min(HorizontalValue, VerticalValue).


% valid_moves(+Piece, +AILevel, -ListOfMoves)
% Finds all valid moves for the AI
valid_moves(Board, Piece, 1, ListOfMoves) :-
    get_game_state(Board, Rows, Columns,_,_),
    Rows1 is Rows-1,
    Columns1 is Columns-1,
    findall(I-J, (between(1, Rows1, I), between(1, Columns1, J), validate_move(Board, I-J, Piece, _)), ListOfMoves).
/*
% valid_move(+Board, +AILevel, +Piece, -ListOfMoves)
% Generates a list of all valid moves for the easy AI
valid_moves(Board, Piece, 1, ListOfMoves):-
    validate_move(Board, I-J, Piece, _),
    append(ListOfMoves, [I-J], ListOfMoves),
    %findall(I-J, validate_move(Board, I-J, Piece, _), ListOfMoves),
    write(ListOfMoves), nl.
*/
% valid_move(+Board, +AILevel, +Piece, -ListOfMoves)
% Generates a list of all valid moves for the hard AI
valid_moves(Board, '2', Piece, ListOfMoves):- %dependo do flow do codigo talvez nao seja preciso passar piece e é so preciso chamar get_state
    value(Board, Piece, InitialValue),
    get_state(Piece, Enemy),
    findall(I-J, (
        validate_move(Board, I, J, NewBoard),
        \+results_in_win(Board,I,J,Enemy),
        value(NewBoard, Piece, NewValue), 
        NewValue < InitialValue
        ), ListOfMoves). % Se NewValue < InitialValue, então quer dizer que a diferença entre o maior segmento e o tamanho necessário para ganhar diminuiu, então o movimento é bom

better_valid_moves(Board, Piece, ListOfMoves):-
    get_state(Piece, Enemy),
    findall(I-J, (validate_move(Board, I, J), \+results_in_win(Board, I, J, Enemy)), ListOfMoves).

% results_in_win(+Board, +I, +J, +Piece)
% Checks if the move to the position at row I and column J of the board results in a win condition for the given piece
results_in_win(Board, I, J, Piece) :-
    copy_term(Board, TempBoard),
    place_piece(TempBoard, I, J, Piece),
    check_win(TempBoard, I, J, Piece).

% choose_move(+Board, +Piece, +AILevel, -Move)
% Chooses a move for the AI
choose_move(Board, Piece, Level, Move):- %Se falhar quer dizer que ListOfMoves é vazia, pode ser necessário fazer uma cena que acabe o jogo se nao der para fazer mais movimentos, mas nao sei se é possivel neste jogo acabar em draw
    valid_moves(Board, Piece, Level, ListOfMoves),
    random_member(Move, ListOfMoves).

% Apercebi-me agora que esta forma é meia à bruta, porque se não encontrar um movimento que aumente o maior segmento, vai escolher um movimento aleatório, mas tecnicamente a melhor jogada possível seria tentar aumentar o segundo maior segmento, seria muito mais dificil fazer isso tho acho
% Para fazermos essa de depois meter no segundo melhor ig que teriamos que mudar o find_longest_segment todo para ele retornar uma lista sorted por tamanho de segmento, e depois era so ir buscar o segundo elemento da lista e por ai fora até funcionar, se nunca conseguir então acaba por escolher um movimento aleatório
choose_move(Board, Piece, '2', Move):- %Só entra neste caso o de cima falhar, ou seja ListOfMoves está vazia, que em princípio só acontece quando o hard AI não consegue encontrar um movimento que aumente o seu maior segmento
    better_valid_moves(Board, Piece, ListOfMoves), 
    random_member(Move, ListOfMoves).
