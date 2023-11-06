% find_longest_horizontal(+Board, +Piece, -LongestHorizontal)
% Finds the longest horizontal segment of Piece on Board
find_longest_horizontal([Row|Rows], Piece, LongestHorizontal) :-
    find_longest_segment_in_row(Piece, Row, LongestInRow),
    find_longest_horizontal(Rows, Piece, LongestInRest),
    LongestHorizontal is max(LongestInRow, LongestInRest).
find_longest_horizontal([], _, 0).

% find_longest_vertical(+Board, +Piece, -LongestVertical)
% Finds the longest vertical segment of Piece on Board
find_longest_vertical(Board, Piece, LongestVertical) :-
    transpose(Board, TransposedBoard),
    find_longest_horizontal(TransposedBoard, Piece, LongestVertical).

% find_longest_segment_in_row(+Piece, +Row, -LongestSegment)
% Finds the longest segment of Piece in Row
find_longest_segment_in_row(Piece, [Piece|T], LongestSegment) :-
    find_longest_segment_in_row(Piece, T, LongestInRest),
    LongestSegment is 1 + LongestInRest.
find_longest_segment_in_row(Piece, [_|T], LongestSegment) :-
    find_longest_segment_in_row(Piece, T, LongestSegment).
find_longest_segment_in_row(_, [], 0).

value(Board, Piece, Value) :-
    get_game_state(_, Rows, Columns, _, _),
    %write('test'), nl,
    find_longest_horizontal(Board, Piece, LongestHorizontal),
    %write('test1'), nl,
    find_longest_vertical(Board, Piece, LongestVertical),
    %write('LongestHorizontal: '), write(LongestHorizontal), nl,
    %write('LongestVertical: '), write(LongestVertical), nl,
    Columns1 is Columns-2,
    Rows1 is Rows-2,
    min(LongestHorizontal, Columns1,HorizontalValue),
    min(LongestVertical, Rows1, VerticalValue),
    %write('HorizontalValue: '), write(HorizontalValue), nl,
    %write('VerticalValue: '), write(VerticalValue), nl,
    min(HorizontalValue, VerticalValue, Value).


% valid_moves(+Piece, +AILevel, -ListOfMoves)
% Finds all valid moves for the AI
valid_moves(Board, Piece, 1, ListOfMoves) :-
    get_game_state(_, Rows, Columns,_,_),
    Rows1 is Rows-1,
    Columns1 is Columns-1,
    findall(I-J, (between(1, Rows1, I), between(1, Columns1, J), validate_move(Board, I-J, Piece, _)), ListOfMoves).

% valid_move(+Board, +AILevel, +Piece, -ListOfMoves)
% Generates a list of all valid moves for the hard AI
valid_moves(Board, Piece, 2, ListOfMoves):- %dependo do flow do codigo talvez nao seja preciso passar piece e é so preciso chamar get_state
    value(Board, Piece, InitialValue),
    get_game_state(Board, Rows, Columns,_,_),
    Rows1 is Rows-1,
    Columns1 is Columns-1,
    
    findall(I-J, (
        between(1, Rows1, I), between(1, Columns1, J),
        validate_move(Board, I-J, Piece, NewBoard),
        value(NewBoard, Piece, NewValue), 
        NewValue < InitialValue
        ), ListOfMoves).% Se NewValue < InitialValue, então quer dizer que a diferença entre o maior segmento e o tamanho necessário para ganhar diminuiu, então o movimento é bom

% get_enemy_winning_moves(+Board, +Piece, -ListOfMoves)
% Generates a list of moves that the enemy can play next turn to win
get_enemy_winning_moves(Board, Piece, ListOfMoves):-
    get_game_state(Board, Rows, Columns,_,_),
    get_next_state(Piece, EnemyPiece,_),
    Rows1 is Rows-1,
    Columns1 is Columns-1,
    findall(I-J, (between(1, Rows1, I), between(1, Columns1, J), validate_move(Board, I-J, EnemyPiece, _), validate_move(Board, I-J, Piece, _), results_in_win(Board, I, J, EnemyPiece)), ListOfMoves)
    .

get_winning_moves(Board, Piece, ListOfMoves):-
    get_game_state(_, Rows, Columns,_,_),
    Rows1 is Rows-1,
    Columns1 is Columns-1,
    findall(I-J, (between(1, Rows1, I), between(1, Columns1, J), validate_move(Board, I-J, Piece, _), check_win(Board, Piece, I-J)), ListOfMoves).

% results_in_win(+Board, +I, +J, +Piece)
% Checks if the move to the position at row I and column J of the board results in a win condition for the given piece
results_in_win(Board, I, J, Piece) :-
    place_piece(Board, I-J, Piece, NewBoard),
    check_win(NewBoard, Piece, I-J).