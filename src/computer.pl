
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

% valid_move(+Board, +AILevel, +Piece, -ListOfMoves)
% Generates a list of all valid moves for the easy AI
valid_moves(Board, '1', _Piece, ListOfMoves):-
    findall(I-J, validate_move(Board, I, J, _NewBoard), ListOfMoves).

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

% results_in_win(+Board, +I, +J, +Piece)
% Checks if the move to the position at row I and column J of the board results in a win condition for the given piece
results_in_win(Board, I, J, Piece) :-
    copy_term(Board, TempBoard),
    place_piece(TempBoard, I, J, Piece),
    check_win(TempBoard, I, J, Piece).

% choose_move(+Piece, +AILevel, -Move)
% Chooses a move for the AI
choose_move(Piece, Level, Move):-
    valid_moves(Piece, Level, ListOfMoves),
    random_member(Move, ListOfMoves).
choose_move(Piece, '2', []):-
    get_state(Piece, Enemy),
    findall(I-J, (validate_move(Board, I, J, _NewBoard), \+results_in_win(Board,I,J,Enemy)), ListOfMoves),
    random_member(Move, ListOfMoves).