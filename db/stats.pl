% see: https://bulbapedia.bulbagarden.net/wiki/Stat
:- module('stats', [stat/7, max_speed/2, max_speed/3, mon_speed_tiers/3]).

:- use_module(library(debug)).
:- use_module('dex/pokemon.pl').

netural_nature(hardy).
netural_nature(docile).
netural_nature(serious).
netural_nature(bashful).
netural_nature(quirky).

boosting_nature(lonely, atk).
boosting_nature(brave, atk).
boosting_nature(adamant, atk).
boosting_nature(naughty, atk).
boosting_nature(bold, def).
boosting_nature(relaxed, def).
boosting_nature(impish, def).
boosting_nature(lax, def).
boosting_nature(timid, spe).
boosting_nature(hasty, spe).
boosting_nature(jolly, spe).
boosting_nature(naive, spe).
boosting_nature(modest, spa).
boosting_nature(mild, spa).
boosting_nature(quiet, spa).
boosting_nature(rash, spa).
boosting_nature(calm, spd).
boosting_nature(gentle, spd).
boosting_nature(sassy, spd).
boosting_nature(careful, spd).

negative_nature(lonely, def).
negative_nature(brave, spe).
negative_nature(adamant, spa).
negative_nature(naughty, spd).
negative_nature(bold, atk).
negative_nature(relaxed, spe).
negative_nature(impish, spa).
negative_nature(lax, spd).
negative_nature(timid, atk).
negative_nature(hasty, def).
negative_nature(jolly, spa).
negative_nature(naive, spd).
negative_nature(modest, atk).
negative_nature(mild, def).
negative_nature(quiet, spe).
negative_nature(rash, spd).
negative_nature(calm, atk).
negative_nature(gentle, def).
negative_nature(sassy, spe).
negative_nature(careful, spa).

nature_multiplier(negative, 0.9).
nature_multiplier(neutral, 1.0).
nature_multiplier(positive, 1.1).

stat(hp).
stat(atk).
stat(def).
stat(spa).
stat(spd).
stat(spe).

mon_speed_tiers(Mon, SpeedTier, Speed) :-
  stat_tier(SpeedTier),
  call(SpeedTier, Mon, spe, Speed).

stat_tier(max_positive_plus_one).
stat_tier(max_neutral_plus_one).
stat_tier(max_positive).
stat_tier(max_neutral).
stat_tier(default_stat).
stat_tier(min_stat).

% Relevant tiers
max_positive_plus_one(Mon, StatName, Stat) :-
  max_positive(Mon, StatName, MaxStat),
  Stat is floor(MaxStat * 1.5).
max_neutral_plus_one(Mon, StatName, Stat) :-
  max_neutral(Mon, StatName, MaxStat),
  Stat is floor(MaxStat * 1.5).
max_neutral(Mon, StatName, Stat) :- stat(Mon, StatName, 100, 252, 31, neutral, Stat).
max_positive(Mon, StatName, Stat) :- stat(Mon, StatName, 100, 252, 31, positive, Stat).
default_stat(Mon, StatName, Stat) :- stat(Mon, StatName, 100, 0, 31, neutral, Stat).
min_stat(Mon, StatName, Stat) :- stat(Mon, StatName, 100, 0, 0, negative, Stat).

stat(Mon, StatName, Level, EVs, IVs, Nature, Stat) :-
    stat(StatName),
    stat_calc(StatName, Calc),
    call(Calc, Mon, StatName, Level, EVs, IVs, Nature, Stat).

stat_calc(hp, hp_stat).
stat_calc(X, non_hp_stat) :- X \= hp, stat(X).

non_hp_stat(Mon, StatName, Level, EVs, IVs, Nature, Stat) :-
  atom_concat(pokemon_, StatName, StatAtom),
  call(StatAtom, Mon, BaseStat),
  nature_multiplier(Nature, NatureMult),
  LevelUpVals is floor(((2 * BaseStat + IVs + floor(EVs / 4)) * Level) / 100),
  Stat is floor((LevelUpVals + 5) * NatureMult).

hp_stat(Mon, _, Level, EVs, IVs, _, Stat) :-
  pokemon_hp(Mon, BaseStat),
  LevelUpVals is floor(((2 * BaseStat + IVs + floor(EVs / 4)) * Level) / 100),
  Stat is floor((LevelUpVals) + Level + 10).

max_speed(Mon, Spe) :- max_speed(Mon, Spe, positive).

max_speed(Mon, Spe, Nature) :-
  stat(Mon, spe, 100, 252, 31, Nature, Spe).

