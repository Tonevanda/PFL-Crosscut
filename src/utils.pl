% forall(+Start, +End, +Goal)
% Succeeds if Goal holds for all integers between Start and End (inclusive)
forall(Start, End, Goal) :-
    Start =< End,
    copy_term(Goal, GoalCopy),
    GoalCopy =.. [Pred|Args],
    append(Args, [Start], NewArgs),
    NewGoal =.. [Pred|NewArgs],
    call(NewGoal),
    Next is Start + 1,
    forall(Next, End, Goal).
forall(Start, End, _) :-
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
    retract(gameState(_, Rows, Columns,BlueLevel,RedLevel)),
    asserta(gameState(Board, Rows, Columns,BlueLevel,RedLevel)).

% between(+Low, +High, ?Value)
% True if Value is between Low and High, inclusive.
between(Low, High, Low) :- 
    Low =< High.
between(Low, High, Value) :- 
    Low < High, 
    NewLow is Low + 1, 
    between(NewLow, High, Value).



% max_in_list(+List, -Max)
% Finds the maximum element in List
max_list([X], X).
max_list([X|Xs], Max) :-
    max_list(Xs, MaxRest),
    Max is max(X, MaxRest).

% min(+Num1, +Num2, -Min)
% Finds the minimum between Num1 and Num2
min(Num1, Num2, Min) :-
    Num1 =< Num2,
    Min = Num1.
min(Num1, Num2, Min) :-
    Num2 < Num1,
    Min = Num2.


    % predsort(+Pred, +List, -Sorted)
% Sorts List into Sorted using Pred as the comparison predicate
predsort(_, [], []).
predsort(Pred, [X|Xs], Sorted) :-
    predsort(Pred, Xs, SortedXs),
    insert(Pred, X, SortedXs, Sorted).

% insert(+Pred, +X, +List, -Result)
% Inserts X into List (which is assumed to be sorted according to Pred)
% such that the resulting list Result is also sorted according to Pred
insert(Pred, X, [Y|Ys], [Y|Zs]) :-
    call(Pred, Order, X, Y),
    Order = '>',
    insert(Pred, X, Ys, Zs).
insert(Pred, X, [Y|Ys], [X,Y|Ys]) :-
    call(Pred, Order, X, Y),
    Order \= '>'.
insert(_, X, [], [X]).

clear_buffer:-
	repeat,
	get_char(C),
	C='\n',
	!.