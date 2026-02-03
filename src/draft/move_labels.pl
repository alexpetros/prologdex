:- module(move_labels, [learns_removal/2, learns_hazards/2, learns_priority/3]).

:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(library(dif)).

:- use_module('../dex/pokemon.pl').
:- use_module('../dex/moves.pl').
:- use_module('../dex/learnsets.pl').

learns_removal(Mon, Move) :-
  removal_move_t(Move, true),
  learns(Mon, Move).
removal_move_t(Move, T) :- move(Move), removal_moves(Moves), memberd_t(Move, Moves, T).
removal_moves([rapidspin, defog, courtchange, tidyup]).

learns_hazards(Mon, Move) :-
  hazard_move_t(Move, true),
  learns(Mon, Move).
hazard_move_t(Move, T) :- move(Move), hazard_moves(Moves), memberd_t(Move, Moves, T).
hazard_moves([stealthrock, spikes, toxicspikes, stickyweb]).

damaging_move(Move) :-
  move_category(Move, special);
  move_category(Move, physical).

learns_priority(Mon, Move, Priority) :-
  learns(Mon, Move),
  doubles_move_t(Move, false),
  protection_move_t(Move, false),
  dif(Move, bide),
  move_priority(Move, BasePriority),
  move_category(Move, Category),
  if_(
    ','(pokemon_ability_t(Mon, prankster), =(Category, status)),
    Priority #= BasePriority + 1,
    Priority #= BasePriority
  ),
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

doubles_move_t(Move, T) :- move(Move), doubles_moves(Moves), memberd_t(Move, Moves, T).
doubles_moves([
  helpinghand,
  afteryou,
  quash,
  allyswitch,
  followme,
  ragepowder,
  aromaticmist,
  holdhands,
  spotlight,
  craftyshield,
  quickguard,
  wideguard
]).

protection_move_t(Move, T) :- protection_moves(Moves), memberd_t(Move, Moves, T).
protection_moves([
  endure,
  detect,
  protect,
  magiccoat,
  kingsshield,
  burningbulwark,
  spikyshield,
  banefulbunker
]).
