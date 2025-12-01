:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module('dex.pro').
:- use_module('draft.pro').

in_draft(Mon) :- draft(Mon, Points), Points > 1.

has_move([], _) :- fail.
has_move(Team, Move) :-
  [Mon|Tail] = Team,
  (learns(Mon, Move); has_move(Tail, Move)).

has_type([], _) :- fail.
has_type(Team, Type) :-
  [Mon|Tail] = Team,
  (type(Mon, Type); has_type(Tail, Type)).

point_value([], 0).
point_value(Team, Value) :-
  [Mon|Tail] = Team,
  draft(Mon, MonValue),
  point_value(Tail, TailValue),
  Value is MonValue + TailValue.

% has_move(Team, 'stealthrock'), point_value(Team, 10).
is_sorted([]).
is_sorted([_]).
is_sorted([First|[Second | Rest]]) :-
  First @< Second,
  is_sorted(Rest).

only_mons([]).
only_mons([Head|Tail]) :- in_draft(Head), only_mons(Tail).

draft_team(Team, MaxPoints) :-
  point_value(Team, Value),
  Value =< MaxPoints,
  only_mons(Team).
