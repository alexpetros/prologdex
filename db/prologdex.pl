:- use_module(library(lists)).
:- use_module(library(debug)).

:- use_module('dex/pokemon.pl').
:- use_module('dex/learnsets.pl').
:- use_module('dex/moves.pl').

:- use_module('type-chart.pl').
:- use_module('stats.pl').
:- use_module('s6.pl').
:- use_module('summary-sheet.pl').

is_sorted([]).
is_sorted([_]).
is_sorted([First|[Second | Rest]]) :-
  First @< Second,
  is_sorted(Rest).

learns_removal(Mon, Move) :-
  removal_move(Move),
  learns(Mon, Move).

learns_hazards(Mon, Move) :-
  hazard_move(Move),
  learns(Mon, Move).

damaging_move(Move) :-
  move_category(Move, special);
  move_category(Move, physical).

learns_priority(Mon, Move, Priority) :-
  learns(Mon, Move),
  damaging_move(Move),
  Move \= bide,
  move_priority(Move, Priority),
  Priority #> 0.


% Only unify once on multiple boosts
has_boost(Move) :- setof(_, X^Y^move_boost(Move, X, Y), _).
boosting_move(Move) :-
  move(Move),
  has_boost(Move),
  (
    move_target(Move, self);
    move_target(Move, all);
    move_target(Move, allies);
    move_target(Move, allyteam);
    move_target(Move, allyside)
  ).

removal_move(rapidspin).
removal_move(defog).
removal_move(courtchange).
removal_move(tidyup).

hazard_move(stealthrock).
hazard_move(spikes).
hazard_move(toxicspikes).
hazard_move(stickyweb).

