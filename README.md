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

To describe the game state, we created a dynamic predicate `gameState/3` that holds the current **Board** and its **Rows** and **Columns**. This **Board** is a list of lists of variable width and height (from 5x5 to 9x9), initialized at the beggining of the game with just the ` ` (space) character. Because there are 2 players, `Red` and `Blue`, represented as `R` and `B`, and we need to know which type of player both `R` and `B` are (0 for human player, 1 for easy AI and 2 for harder AI), we created another dynamic predicate called %%%%%% that associates each Piece with its type of player, so, for example, if `Red` was a human player and `Blue` was the harder AI we would represent it as:  

- %%%%%%%('R', 0).
- %%%%%%%%('B', 2). 

%%%%%%%%%%%%%%%%%% DEPOIS TEMOS DE JOGAR E METER EXEMPLOS DE JOGO NO INICIO, MEIO E FIM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Game State Visualization

