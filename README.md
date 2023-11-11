# Crosscut by Mark Steere

## Developers

This project was developed by the group Crosscut6, composed by:

- João Miguel da Silva Lourenço (up202108863) 50%
- Tomás Filipe Fernandes Xavier (up202108759) 50%

## Instalation and Execution

To install this game, simply download the *src* folder, which contains all the code you need to start playing.<br>
Then, once you have downloaded and unzipped the file, simply consult the `main.pl` file via the SICStus console.<br>
Finally, to start playing, run `play.` on the SICStus console. Enjoy!

## Description of the game

The rules of **Crosscut** implemented by us were taken from Mark Steere's website:<br>
https://marksteeregames.com/Crosscut_rules.pdf

### Summary

**Crosscut** is game that takes place in a 2-D rectangular board, size 9x9 or smaller.<br>
There are 2 players, Red and Blue, denoted by the color of their pieces. <br>
The goal of crosscut is simple: every turn, starting with Red, a player must place a colored disc on the board (if there are placements available, or else the turn is skipped). Whoever can form a contiguous horizontal or vertical segment from one side of the board to the other, except the spaces on the edges, with just his colored discs wins the game!

### Extra rules

#### Placement

You can place a disc on any unoccupied square (subject to “perimeter” restrictions. See [Perimeter](#perimeter) section below.)

#### Flanking, Cutting and Disc Flipping

“Flanked” means an enemy segment flanked between the newly
placed friendly disc and another friendly disc.<br>
Disc Flipping happens when flanking occurs. An enemy segment will switch its color when flanked.<br>
If there are 2 segments crossing each other, the segment that was flanked is flipped and turns into the other color and the other segment is **cut**.<br>
Flanking does not always result in flipping. <br>
If the longest friendly segment created would be longer than the longest enemy segment cut, then all flanked segments must be flipped. Otherwise no segments are flipped.<br>

#### Perimeter

The perimeter is comprised of the edge and corner squares. The player can temporarily place a disc in the perimeter only if it will cause an enemy segment to be flipped. (The perimeter disc is not considered to be part of a newly formed friendly segment in determining whether a placement would cause flipping.) Immediately after flipping, the player must remove their perimeter disc. The perimeter is always unoccupied at the conclusion of a turn.


## Game Logic

### Internal Game State Representation

To describe the game state, we created a dynamic predicate `gameState/5` that holds the current **Board**, its **Rows** and **Columns**, **BlueLevel** and **RedLevel**. **BlueLevel** and **RedLevel** represent the level of player of both pieces, 0 for human player, 1 for the easy AI and 2 for the hard AI. The **Board** is a list of lists of variable width and height (from 5x5 to 9x9), initialized at the beggining of the game with just the ` ` (space) character.

#### Game State Examples

The following is a representation of the **Board**, at different stages of the game:

```
      Initial State            Middle of the game              End of the game
[ [x, x , x , x , x ,x],    [ [x, x , x , x , x ,x],      [ [x, x , x , x , x ,x],
  [x, x , x , x , x ,x],      [x,'R','R','R','B',x],        [x,'R','R','R','R',x],
  [x, x , x , x , x ,x],      [x, x , x , x ,'B',x],        [x, x , x , x ,'B',x],
  [x, x , x , x , x ,x],      [x, x , x , x ,'B',x],        [x, x , x , x ,'B',x],
  [x, x , x , x , x ,x] ]     [x, x , x , x , x ,x] ]       [x, x , x , x , x ,x] ]

```

### Game State Visualization

For the initial menu, we created the predicate `read_initial_input` which prompts the user for the number of rows, then the number of columns, followed by the type of player for the `R` piece and the `B` piece. If any of these inputs aren't valid, the predicate repeats until all of them are.<br>
With the input validated, we call the `initial_state/4`, which uses the **Rows** and **Columns** inputted by the user to create an empty list of lists, which represents the `Board` at the beggining of the game.<br>
To display the board, we created the predicate `display_board/0`. This predicate uses the `Board` information stored in the `gameState` dynamic predicate to display the board in a user-friendly way to the SICStus console.

### Move Validation and Execution

To validate a move, we created the predicate `move/5`.This predicate calls upon `choose_move/4`, which, depending on the type of player, will generate a move differently. For the human player, for example, `choose_move` simply takes the users input for the row and column the user chooses.<br>
Afterwards, that move will be sent to the predicate `validate_move/4`, which, as the name suggests, validates the move.<br>
To do this, we check **two** different conditions in which a move is valid:
- The move is an **edge** move, but results in disc flipping, therefore it is valid
- The move is **not** an **edge** 

To verify the first **both** conditions, we needed to implement the `flip/4` predicate, because, i<br>
This predicate, everytime a piece is placed on the `Board`, checks **all four** directions around the piece placed. If there is an enemy segment adjacent to said piece and, at the end of that segment, there is an **Ally** piece (a piece of the same color of the piece just placed), then that segment will be flipped.<br>
However, this does not always happen. If there is an **Enemy** segment ***perpendicular*** to the segment to be flipped, there will be no flipping if the ***perpendicular*** segment is the same size or larger than the size of the segment to be flipped, **if** it were flipped.
So, to do this, we needed to implement a variety of different predicates:
- `flip_check_up/5`
- `flip_check_down/5`
- `flip_check_left/5`
- `flip_check_right/5`

These 4 predicates do essentially the same thing but for different directions:
For every **Enemy** piece on the segment **to be** flipped, it checks if that piece is already on a ***perpendicular*** segment larger than or the same size as the segment to be flipped.<br>
Because we have to check different directions depending on whether the segment **to be** flipped is horizontal or vertical, we implemented the predicates `flip_check_horizontal/6` and `flip_check_vertical/6`. So, for example, if we're checking to the left of the piece just placed, then, inside the `flip_left` predicate, we would call  `flip_check_vertical`, which, in turn, calls both the `flip_check_up` and the `flip_check_down` predicates. If the sum of the `Accumulator` of these 2 predicates is larger or equal to the segment **to be** flipped + 2 (the 2 **Ally** pieces that ***flanked*** the **Enemy** segment), then that segment will not be flipped. And the same goes for the `flip_check_horizontal`, and `flip_check_left` and `flip_check_right/5` predicates.

### List of Valid Moves

To obtain a list of possible moves, we created the `valid_moves/4` predicate.<br>
There are 2 different implementations of this predicate, one for each level of AI.<br>
For the easy AI, `valid_moves` creates a list of valid moves called `ListOfMoves` by using a findall:

```prolog
valid_moves(Board, Piece, 1, ListOfMoves) :-
    get_game_state(_, Rows, Columns,_,_),
    Rows1 is Rows-1,
    Columns1 is Columns-1,
    findall(I-J, (between(1, Rows1, I), between(1, Columns1, J), validate_move(Board, I-J, Piece, _)), ListOfMoves).
```

This creates every I-J pair that satisfies the `validate_move` predicate, that was alreay previously explained.

For the hard AI, we do essentially the same thing, but instead of having every valid move available to the player, we only take into account moves that decrease the `value` of the state of the game for that player's piece. The evaluation is explained below in [Game State Evaluation](#game-state-evaluation).

```prolog
valid_moves(Board, Piece, 2, ListOfMoves):-//
    value(Board, Piece, InitialValue),
    get_game_state(Board, Rows, Columns,_,_),
    Rows1 is Rows-1,
    Columns1 is Columns-1,
    findall(I-J, (
        between(1, Rows1, I), between(1, Columns1, J),
        validate_move(Board, I-J, Piece, NewBoard),
        value(NewBoard, Piece, NewValue), 
        NewValue < InitialValue
        ), ListOfMoves).
```

### End of Game

To verify the end of the game, we implemented the `game_over/3` predicate.<br>
For `game_over` to be true, either one of two conditions must be met:
- There is a contiguous horizontal segment of the same piece from one side of the board to the other, except for the edge spaces
- There is a contiguous vertical segment of the same piece from the top to the bottom of the board, except for the edge spaces
For this, we made 2 different implementations of the `game_over` predicate.<br>
The first one calls the `check_horizontal/5` predicate, that uses the `forall/3` predicate, defined in the `utils.pl` file, to iterate through the row of the recently placed piece to see if it resulted in a win.<br>
If that one fails, then the other implementation of the `game_over` predicate is called.<br>
This one calls the `check_vertical/5` predicate, which checks if the column in which the most recent piece has been placed contains a vertical segment that would result in a win.<br>
After every move, in the `game_loop/0` predicate, we check whether or not the move made resulted in a win. If not then `\+game_over` is true, and the game loop continues.<br>
If the last move did result in a win, then `\+game_over` is false, and, because of the ***cut*** placed above `game_over`, `game_loop` does not backtrack and the game ends.

### Game State Evaluation

To evaluate the state of the game, we created the `value/3` predicate.<br>
We represent the value of the current state of the game as the difference between the longest **Ally** segment and the needed size to win.<br>
To do that, we calculate the longest horizontal **Ally** segment and the longest vertical **Ally** segment, and we calculate the difference between the longest horizontal **Ally** segment and the number of columns - 2 (because the edges don't count) and the difference between the longest vertical **Ally** segment and the number of rows - 2.<br>
The value of the current state of the game is the minimum value of those differentials.<br

### Computer Plays

To generate a move, we created the `choose_move/4` predicate, which has 3 different implementations, one for each type of player.<br>
The implementation of the human player version was already talked about previously in the [Move Validation and Execution](#move-validation-and-execution) part of this report.
For the easy AI, we simply called the `valid_moves` predicate with Level as 1 to get a list of all valid moves. Then, we simply choose a random move with:

```prolog
random_member(Move, ListOfMoves).
```

Finally, for the hard AI, we generate a list of valid moves that decrease the **current value** of the game, for the AI's piece.<br>. 
Because the hard AI always tries to make a move that decreases the value of the state of the game, he is actively trying to elongate its longest segment, and, therefore, actively trying to win.

## Conclusions

This project was very interesting to develop, although hard, because Prolog is a very unusual language, at least compared to what we are used to. Even though it was hard, when everything works, it all makes perfect sense and looks great. The beggining was hard because we had to re-wire our brain into forgetting object-oriented and imperative programming and understanding logical programming.<br>
It's a shame we had a little less than a week to develop this because of other projects. With more time, we could try upgrading the smart AI to become even smarter, possibly through the Minimax algorithm.<br>
Another limitation we had was the Prolog version used during development. Many predicates documented in the SWI Prolog website do not exist in the Prolog version used to develop this project, such as the `forall/2` predicate. Therefore, we had to create our own, which was a little annoying.<br>
The last limitation we had was with the smart AI. We tried to make our own predsort predicate and order the ListOfMoves by their value, but with big boards it becomes too cumbersome.


## Bibliography

To develop this project, we used the following resources:

- https://www.swi-prolog.org/
- https://marksteeregames.com/Crosscut_rules.pdf