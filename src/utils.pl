% my_forall(+Start, +End, +Goal)
% Succeeds if Goal holds for all integers between Start and End (inclusive)
my_forall(Start, End, Goal) :-
    Start =< End,
    copy_term(Goal, GoalCopy),
    GoalCopy =.. [Pred|Args],
    append(Args, [Start], NewArgs),
    NewGoal =.. [Pred|NewArgs],
    call(NewGoal),
    Next is Start + 1,
    my_forall(Next, End, Goal).
my_forall(Start, End, _) :-
    Start > End.

% replace(+List, +Index, +NewElement, -NewList)
% Replaces the element at index Index in List with NewElement
replace([_|Tail], 0, NewElement, [NewElement|Tail]).
replace([Head|Tail], Index, NewElement, [Head|Rest]) :-
    Index > 0,
    Index1 is Index - 1,
    replace(Tail, Index1, NewElement, Rest).

% read_number(-Number)
% Reads a number from the user input
read_number(Number):-
    read_number_aux(Number,0).

read_number_aux(Number,Acc):- 
    get_code(Character),
    Character >= 48,
    Character =< 57,
    !,
    Acc1 is 10*Acc + (Character - 48),
    read_number_aux(Number,Acc1).
read_number_aux(Number,Number).


% read_letter(-Number)
% Reads a lowercase letter from the user input and maps it to a number
read_letter(Number):- 
    read_letter_aux(Number,0).

read_letter_aux(Number,Acc):- 
    get_code(Character),
    Character >= 97,
    Character =< 105,
    !,
    Acc1 is 26*Acc + (Character - 97),
    read_letter_aux(Number,Acc1).
read_letter_aux(Number,Number).

update_game_state(Board) :-
    retract(gameState(_, Rows, Columns)),
    asserta(gameState(Board, Rows, Columns)).