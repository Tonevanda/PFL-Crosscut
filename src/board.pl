% FILEPATH: /Users/tonevanda/Documents/GitHub/PFL-Crosscut/src/board.pl

% draw_horizontal_line(+N)
% Draws a horizontal line of size N with the character '-'
draw_horizontal_line(N) :-
    N > 0,
    write('-'),
    N1 is N - 1,
    draw_horizontal_line(N1).
draw_horizontal_line(0).

create_board(N, M, Board) :-
    length(Row, M), % create a row of length M
    length(Board, N), % create a list of N rows
    findall(Row, member(_, Board), Board). % fill the list with rows

% get_element(+Board, +I, +J, -Element)
% Gets the piece at row I and column J of the board
get_piece(Board, I, J, Element) :-
    nth0(I, Board, Row), % get the row at index I
    nth0(J, Row, Element). % get the element at index J in the row

% place_piece(+Board, +I, +J, +NewPiece, -NewBoard)
% Places the piece at row I and column J of the board
place_piece(Board, I, J, NewPiece, NewBoard) :-
    nth0(I, Board, Row), % get the row at index I
    replace(Row, J, NewPiece, NewRow),
    replace(Board, I, NewRow, NewBoard).