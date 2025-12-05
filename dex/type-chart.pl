:- module('type-chart', [normal_against/2, normal_to/2, weak_against/2, weak_to/2,
                         strong_against/2, strong_to/2, no_damage_against/2, no_damage_to/2]).

:- use_module('dex.pl').

normal_against(attacking, defending) :-
  \+ strong(attacking, defending),
  \+ weak(attacking, defending).
normal_to(defending, attacking) :- normal_against(attacking, defending).
weak_to(defending, attacking) :- strong_against(attacking, defending).
strong_to(defending, attacking) :- weak_against(attacking, defending).
no_damage_to(defending, attacking) :- no_damage_against(attacking, defending).

strong_against(fighting, normal).
strong_against(fighting, rock).
strong_against(fighting, steel).
strong_against(fighting, ice).
strong_against(fighting, dark).
strong_against(flying, fighting).
strong_against(flying, bug).
strong_against(flying, grass).
strong_against(poison, grass).
strong_against(poison, fairy).
strong_against(ground, poison).
strong_against(ground, rock).
strong_against(ground, steel).
strong_against(ground, fire).
strong_against(ground, electric).
strong_against(rock, flying).
strong_against(rock, bug).
strong_against(rock, fire).
strong_against(rock, ice).
strong_against(bug, grass).
strong_against(bug, psychic).
strong_against(bug, dark).
strong_against(ghost, ghost).
strong_against(ghost, psychic).
strong_against(steel, rock).
strong_against(steel, ice).
strong_against(steel, fairy).
strong_against(fire, bug).
strong_against(fire, steel).
strong_against(fire, grass).
strong_against(fire, ice).
strong_against(water, ground).
strong_against(water, rock).
strong_against(water, fire).
strong_against(grass, ground).
strong_against(grass, rock).
strong_against(grass, water).
strong_against(electric, flying).
strong_against(electric, water).
strong_against(psychic, fighting).
strong_against(psychic, poison).
strong_against(ice, flying).
strong_against(ice, ground).
strong_against(ice, grass).
strong_against(ice, dragon).
strong_against(dragon, dragon).
strong_against(dark, ghost).
strong_against(dark, psychic).
strong_against(fairy, fighting).
strong_against(fairy, dragon).
strong_against(fairy, dark).

weak_against(normal, rock).
weak_against(normal, steel).
weak_against(fighting, flying).
weak_against(fighting, poison).
weak_against(fighting, bug).
weak_against(fighting, psychic).
weak_against(fighting, fairy).
weak_against(flying, rock).
weak_against(flying, steel).
weak_against(flying, electric).
weak_against(poison, poison).
weak_against(poison, ground).
weak_against(poison, rock).
weak_against(poison, ghost).
weak_against(ground, bug).
weak_against(ground, grass).
weak_against(rock, fighting).
weak_against(rock, ground).
weak_against(rock, steel).
weak_against(bug, fighting).
weak_against(bug, flying).
weak_against(bug, poison).
weak_against(bug, ghost).
weak_against(bug, steel).
weak_against(bug, fire).
weak_against(bug, fairy).
weak_against(ghost, dark).
weak_against(steel, steel).
weak_against(steel, fire).
weak_against(steel, water).
weak_against(steel, electric).
weak_against(fire, rock).
weak_against(fire, fire).
weak_against(fire, water).
weak_against(fire, dragon).
weak_against(water, water).
weak_against(water, grass).
weak_against(water, dragon).
weak_against(grass, flying).
weak_against(grass, poison).
weak_against(grass, steel).
weak_against(grass, fire).
weak_against(grass, grass).
weak_against(grass, dragon).
weak_against(electric, grass).
weak_against(electric, electric).
weak_against(electric, dragon).
weak_against(psychic, steel).
weak_against(psychic, psychic).
weak_against(ice, steel).
weak_against(ice, fire).
weak_against(ice, water).
weak_against(ice, ice).
weak_against(dragon, steel).
weak_against(dark, fighting).
weak_against(dark, dark).
weak_against(dark, fairy).
weak_against(fairy, poison).
weak_against(fairy, steel).
weak_against(fairy, fire).

no_damage_against(normal, ghost).
no_damage_against(poison, steel).
no_damage_against(ground, flying).
no_damage_against(ghost, normal).
no_damage_against(electric, ground).
no_damage_against(psychic, dark).
no_damage_against(dragon, fairy).
