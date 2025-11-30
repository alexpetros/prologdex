:- use_module('dex.pro').
:- use_module('draft.pro').

in_draft(Mon) :- draft(Mon, Points), Points > 1.

has_move(Team, Move) :-
  [Mon|Tail] = Team,
  (learns(Mon, Move); has_move(Tail, Move)).

point_value([], 0).
point_value(Team, Value) :-
  [Mon|Tail] = Team,
  draft(Mon, MonValue),
  point_value(Tail, TailValue),
  Value is MonValue + TailValue.
