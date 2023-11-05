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

The rules of **Crosscut** implemented by us were taken from Mark Steere's website
https://marksteeregames.com/Crosscut_rules.pdf

### Summary

**Crosscut** is game that takes place in a 2-D rectangular board, size 10x10 or smaller.<br>
There are 2 players, Red and Blue, denoted by the color of their pieces. <br>
The goal of crosscut is simple: every turn, starting with Red, a player must place a colored disc on the board (if there are placements available, or else the turn is skipped). Whoever can form a contiguous horizontal or vertical segment from one side of the board to the other, except the spaces on the edges, with just his colored discs wins the game!

### Extra rules

#### Placement

You can place a disc on any unoccupied square (subject to “perimeter” restrictions. See Perimeter section below.)

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

To describe the game state, we created a dynamic predicate `gameState/5` that holds the current **Board**, its **Rows** and **Columns**, **BlueLevel** and **RedLevel**.**BlueLevel** and **RedLevel** represent the level of player of both pieces, 0 for human player, 1 for the easy AI and 2 for the hard AI. The **Board** is a list of lists of variable width and height (from 5x5 to 9x9), initialized at the beggining of the game with just the ` ` (space) character.

%%%%%%%%%%%%%%%%%% DEPOIS TEMOS DE JOGAR E METER EXEMPLOS DE JOGO NO INICIO, MEIO E FIM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Game State Visualization

For the initial menu, we created the predicate `read_input`%%%%%vamos mudar este nome por favor%%%%%%%% which prompts the user for the number of rows, then the number of columns, followed by the type of player for the `R` piece and the `B` piece. If any of these inputs aren't valid, the predicate repeats until all of them are.%%%%temos que mudar isto%%%%<br>
With the input validated, we call the `initial_state/4`, which uses the **Rows** and **Columns** inputted by the user to create an empty list of lists, which represents the `Board` at the beggining of the game.<br>
To display the board, we created the predicate `display_board/0`%%%%podiamos mudar para display_game para nao chatear tanto o stor%%%%. This predicate uses the `Board` information stored in the `gameState` dynamic predicate to display the board in a user-friendly way to the SICStus console.

### Move Validation and Execution

To validate a move, we created the predicate `move/5`%%%vamos mudar a posição dos argumentos, Move devia estar no fim dos argumentos porque é um - e não +%%%.This predicate calls upon `choose_move/4`, which, depending on the type of player, will generate a move differently. For the human player, for example, `choose_move` simply takes the users input for the row and column the user chooses.<br>
Afterwards, that move will be sent to the predicate `validate_move/4`, which, as the name suggests, validates the move.