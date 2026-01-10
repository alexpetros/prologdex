:- use_module(library(debug)).


run :-
  use_module('db/prologdex.pl'),
  statistics(runtime, [LoadTimeMs, _]),
  LoadTimeS is LoadTimeMs / 1000,
  format("Prologdex load time: ~2f seconds~n", [LoadTimeS]),
  pokemon_exists,
  type_matchup_bulbasaur,
  format("~nall tests passed~n", []).

pokemon_exists :-
  pokemon(bulbasaur),
  format("existance test passed~n", []).

% Would probably fail with a different search strategy, which is not ideal
type_matchup_bulbasaur :-
  Mon = bulbasaur,
  findall(T, mon_type_matchup(Mon, Type, very_strong), [grass]),
  findall(T, mon_type_matchup(Mon, Type, strong), [fighting, water, electric, fairy]),
  findall(T, mon_type_matchup(Mon, Type, weak), [flying, fire, psychic, ice]),
  findall(T, mon_type_matchup(Mon, Type, very_weak), []),
  format("type matchup test passed~n", []).

