
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

value(Board, Piece, Rows, Columns, Value) :-
    find_longest_horizontal(Board, Piece, LongestHorizontal),
    find_longest_vertical(Board, Piece, LongestVertical),
    HorizontalValue is min(LongestHorizontal, Columns - 2),
    VerticalValue is min(LongestVertical, Rows - 2),
    Value is min(HorizontalValue, VerticalValue).