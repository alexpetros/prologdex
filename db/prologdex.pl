:- use_module(library(lists)).
:- use_module(library(debug)).

:- use_module('dex/pokemon.pl').
:- use_module('dex/learnsets.pl').
:- use_module('dex/moves.pl').

:- use_module('type-chart.pl').
:- use_module('stats.pl').
:- use_module('s6.pl').

is_sorted([]).
is_sorted([_]).
is_sorted([First|[Second | Rest]]) :-
  First @< Second,
  is_sorted(Rest).

only_mons([]).
only_mons([Head|Tail]) :- viable(Head), only_mons(Tail).

draft_team(Team, MaxPoints) :-
  point_value(Team, Value),
  Value =< MaxPoints,
  only_mons(Team).

learns_removal(Mon, Move) :-
  removal_move(Move),
  learns(Mon, Move).

learns_hazards(Mon, Move) :-
  removal_move(Move),
  learns(Mon, Move).

removal_move(rapidspin).
removal_move(defog).
removal_move(courtchange).
removal_move(tidyup).

hazard_move(stealthrock).
hazard_move(spikes).
hazard_move(toxicspikes).
hazard_move(stickyweb).

