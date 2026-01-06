:- module('damage-calculator', [damage/7]).

:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module('dex/pokemon.pl').
:- use_module('dex/moves.pl').
:- use_module('type-chart.pl').

rolls([0.85, 0.86, 0.87, 0.88, 0.89, 0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1]).

poke_round(X, Res) :-
  Res is ceiling(X - 0.5).

round_mult(X, Y, Res) :-
  Raw is X * Y,
  poke_round(Raw, Res).

floor_mult(X, Y, Res) :- Res is floor(X * Y).

stab_mult(Mon, Move, 1.5) :-
  type(Mon, MonType),
  move_type(Move, MoveType),
  MonType = MoveType.
stab_mult(Mon, Move, 1) :- \+ stab_mult(Mon, Move, 1.5).

% Going per smogon here, not bulbapedia
% https://github.com/smogon/damage-calc/blob/4af13ef2c441074418f7fdd461c3758dbc0f033e/calc/src/mechanics/util.ts#L470
damage(Level, Attacker, Defender, A, D, Move, PossibleDamages):-
  move_type(Move, MoveType),
  base_calc(Level, A, D, Move, BaseCalc),
  stab_mult(Attacker, Move, StabMult),
  type_mult(Defender, MoveType, TypeMult),
  rolls(Rolls),
  % https://github.com/smogon/damage-calc/blob/4af13ef2c441074418f7fdd461c3758dbc0f033e/calc/src/mechanics/util.ts#L539
  maplist(floor_mult(BaseCalc), Rolls, PossibleRolls),
  maplist(floor_mult(StabMult), PossibleRolls, StabRolls),
  maplist(round_mult(TypeMult), StabRolls, PossibleDamages).

base_calc(Level, A, D, Move, BaseCalc) :-
  move_power(Move, Power),
  Top is floor(floor((2 * Level) / 5 + 2) * Power * (A/D)),
  BaseCalc is floor((Top / 50) + 2).

