:- module('type-chart', [super_effective/2, not_very_effective/2, no_damage/2,
                         weak_to/2, strong_against/2, immune/2, normal_damage/2,
                         weak/2, very_weak/2, strong/2, very_strong/2, normal/2,
                         type_matchup/7
                         ]).

:- use_module('dex.pl').
:- use_module(library(lists)).

% type_matchup(Mon) :-
%   type_matchup(Mon, Immune, VeryStrong, Strong, _, Weak, VeryWeak),
%   format("Mon: ~q\n", [Mon]),
%   format("Immune: ~q\n", [Immune]),
%   format("Very Strong: ~q\n", [VeryStrong]),
%   format("Strong: ~q\n", [Strong]),
%   format("Weak: ~q\n", [Weak]),
%   format("Very Weak: ~q\n", [VeryWeak]),
%   nl.

type_matchup(Mon, Immune, VeryStrong, Strong, Normal, Weak, VeryWeak) :-
  pokemon(Mon),
  immune(Mon, Immune),
  very_strong(Mon, VeryStrong),
  strong(Mon, Strong),
  normal(Mon, Normal),
  weak(Mon, Weak),
  very_weak(Mon, VeryWeak).

immune(Mon, Types) :- find_type_matchup(Mon, 0, Types).
very_strong(Mon, Types) :- find_type_matchup(Mon, 0.25, Types).
strong(Mon, Types) :- find_type_matchup(Mon, 0.5, Types).
normal(Mon, Types) :- find_type_matchup(Mon, 1, Types).
weak(Mon, Types) :- find_type_matchup(Mon, 2, Types).
very_weak(Mon, Types) :- find_type_matchup(Mon, 4, Types).

find_type_matchup(Mon, Modifier, Types) :-
  findall(Type, type(Mon, Type), MonTypes),
  findall(Attacker, (calc_modifier(MonTypes, Attacker, Modifier)), Types).

mult(L, S0, S) :- S is L * S0.
calc_modifier(Types, Attacker, Modifier) :-
  type(Attacker),
  maplist(type_modifier(Attacker), Types, AttackerModifiers),
  foldl(mult, AttackerModifiers, 1, Modifier).

type_modifier(Attacker, Defender, Modifier) :- super_effective(Attacker, Defender), Modifier is 2.
type_modifier(Attacker, Defender, Modifier) :- not_very_effective(Attacker, Defender), Modifier is 0.5.
type_modifier(Attacker, Defender, Modifier) :- no_damage(Attacker, Defender), Modifier is 0.
type_modifier(Attacker, Defender, Modifier) :- normal_damage(Attacker, Defender), Modifier is 1.

% strong(Mon) :- super_effective.
% very_strong(Mon) super_effective.

normal_damage(Attacking, Defending) :-
  \+ super_effective(Attacking, Defending),
  \+ not_very_effective(Attacking, Defending),
  \+ no_damage(Attacking, Defending).

% normal_to(defending, attacking) :- normal_against(attacking, defending).

weak_to(Defending, Attacking) :- super_effective(Attacking, Defending).
strong_against(Defending, Attacking) :- not_very_effective(Attacking, Defending).
immune_to(Defending, Attacking) :- no_damage(Attacking, Defending).

type(normal).
type(fighting).
type(flying).
type(poison).
type(ground).
type(rock).
type(bug).
type(ghost).
type(steel).
type(fire).
type(water).
type(grass).
type(electric).
type(psychic).
type(ice).
type(dragon).
type(dark).
type(fairy).

super_effective(fighting, normal).
super_effective(fighting, rock).
super_effective(fighting, steel).
super_effective(fighting, ice).
super_effective(fighting, dark).
super_effective(flying, fighting).
super_effective(flying, bug).
super_effective(flying, grass).
super_effective(poison, grass).
super_effective(poison, fairy).
super_effective(ground, poison).
super_effective(ground, rock).
super_effective(ground, steel).
super_effective(ground, fire).
super_effective(ground, electric).
super_effective(rock, flying).
super_effective(rock, bug).
super_effective(rock, fire).
super_effective(rock, ice).
super_effective(bug, grass).
super_effective(bug, psychic).
super_effective(bug, dark).
super_effective(ghost, ghost).
super_effective(ghost, psychic).
super_effective(steel, rock).
super_effective(steel, ice).
super_effective(steel, fairy).
super_effective(fire, bug).
super_effective(fire, steel).
super_effective(fire, grass).
super_effective(fire, ice).
super_effective(water, ground).
super_effective(water, rock).
super_effective(water, fire).
super_effective(grass, ground).
super_effective(grass, rock).
super_effective(grass, water).
super_effective(electric, flying).
super_effective(electric, water).
super_effective(psychic, fighting).
super_effective(psychic, poison).
super_effective(ice, flying).
super_effective(ice, ground).
super_effective(ice, grass).
super_effective(ice, dragon).
super_effective(dragon, dragon).
super_effective(dark, ghost).
super_effective(dark, psychic).
super_effective(fairy, fighting).
super_effective(fairy, dragon).
super_effective(fairy, dark).

not_very_effective(normal, rock).
not_very_effective(normal, steel).
not_very_effective(fighting, flying).
not_very_effective(fighting, poison).
not_very_effective(fighting, bug).
not_very_effective(fighting, psychic).
not_very_effective(fighting, fairy).
not_very_effective(flying, rock).
not_very_effective(flying, steel).
not_very_effective(flying, electric).
not_very_effective(poison, poison).
not_very_effective(poison, ground).
not_very_effective(poison, rock).
not_very_effective(poison, ghost).
not_very_effective(ground, bug).
not_very_effective(ground, grass).
not_very_effective(rock, fighting).
not_very_effective(rock, ground).
not_very_effective(rock, steel).
not_very_effective(bug, fighting).
not_very_effective(bug, flying).
not_very_effective(bug, poison).
not_very_effective(bug, ghost).
not_very_effective(bug, steel).
not_very_effective(bug, fire).
not_very_effective(bug, fairy).
not_very_effective(ghost, dark).
not_very_effective(steel, steel).
not_very_effective(steel, fire).
not_very_effective(steel, water).
not_very_effective(steel, electric).
not_very_effective(fire, rock).
not_very_effective(fire, fire).
not_very_effective(fire, water).
not_very_effective(fire, dragon).
not_very_effective(water, water).
not_very_effective(water, grass).
not_very_effective(water, dragon).
not_very_effective(grass, flying).
not_very_effective(grass, poison).
not_very_effective(grass, steel).
not_very_effective(grass, fire).
not_very_effective(grass, grass).
not_very_effective(grass, dragon).
not_very_effective(electric, grass).
not_very_effective(electric, electric).
not_very_effective(electric, dragon).
not_very_effective(psychic, steel).
not_very_effective(psychic, psychic).
not_very_effective(ice, steel).
not_very_effective(ice, fire).
not_very_effective(ice, water).
not_very_effective(ice, ice).
not_very_effective(dragon, steel).
not_very_effective(dark, fighting).
not_very_effective(dark, dark).
not_very_effective(dark, fairy).
not_very_effective(fairy, poison).
not_very_effective(fairy, steel).
not_very_effective(fairy, fire).

no_damage(normal, ghost).
no_damage(poison, steel).
no_damage(ground, flying).
no_damage(ghost, normal).
no_damage(electric, ground).
no_damage(psychic, dark).
no_damage(dragon, fairy).
