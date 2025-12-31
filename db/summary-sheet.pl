:- module('summary-sheet', [speed_tiers_list/1, speed_tiers_list/2,
                            speed_tiers_chart/1,
                            print_type_chart/1, standings/0, standings/1]).

:- use_module(library(lists)).
:- use_module('dex/pokemon.pl').
:- use_module('s6.pl').
:- use_module('stats.pl').
:- use_module('type-chart.pl').

%%% Sandings
standings_key(Record, Key-Record) :-
  Record = [_, W, L, KD],
  RoundedKD is floor(KD * 100) / 100,
  Key is ((W - L) * 10000) + RoundedKD.

standings_line(_-Record, S) :-
  Record = [P, W, L, KD],
  phrase(format_("~a~t~10| ~d-~d (KD ~2f)", [P, W, L, KD]), S).

standings(S) :-
  findall([P, W, L, KD], record(P, W, L, KD), Records),
  maplist(standings_key, Records, RecordsKeyList),
  keysort(RecordsKeyList, Sorted),
  reverse(Sorted, RevSorted),
  maplist(standings_line, RevSorted, S).

standings :-
  standings(S),
  maplist(printline, S).

%%% Speed tiers list
speed_tier_str(max_positive_plus_one, "Max+ (+1)").
speed_tier_str(max_neutral_plus_one, "Max  (+1)").
speed_tier_str(max_positive, "Max+").
speed_tier_str(max_neutral, "Max").
speed_tier_str(default_stat, "Default").
speed_tier_str(min_stat, "Min").

speed_tiers_chart_line(Spe-Mon, Line) :-
  speed_tiers(Mon, min_stat, MS),
  speed_tiers(Mon, default_stat, DS),
  speed_tiers(Mon, max_neutral, MNS),
  speed_tiers(Mon, max_positive, MPS),
  speed_tiers(Mon, max_neutral_plus_one, MNPOS),
  speed_tiers(Mon, max_positive_plus_one, MPPOS),
  phrase(format_(
    "~a~t~20| ~d~t~25| ~d~t~30| ~d~t~35| ~d~t~40| ~d~t~45| ~d~t~50| ~d~t~55|",
    [Mon, Spe, MS, DS, MNS, MPS, MNPOS, MPPOS]
  ), Line).

speed_tiers_key(Mon, Spe-Mon) :- pokemon_spe(Mon, Spe).

speed_tiers_chart(Player) :-
  findall(Mon, team(Player, Mon), Mons),
  maplist(speed_tiers_key, Mons, KeyMons),
  keysort(KeyMons, SortedMons),
  reverse(SortedMons, RevSortedMons),
  maplist(speed_tiers_chart_line, RevSortedMons, Lines),
  format(
    "~s~t~20| ~s~t~25| ~s~t~30| ~s~t~35| ~s~t~40| ~s~t~45| ~s~t~50| ~s~t~55|~n",
    ["Pokemon", "Spe", "Min", "Def", "Max", "Max+", "1.5", "1.5+"]
  ),
  maplist(printline, Lines).

speed_tiers_list_key([Speed | Tail], Speed-Tail).
speed_tiers_list_line_(Speed-[Mon|[SpeedTier]], S) :-
  speed_tier_str(SpeedTier, STStr),
  phrase(format_("~d~t~3| | ~a~t~25| | ~s", [Speed, Mon, STStr]), S).

speed_tiers_list_(Player, Mon, SpeedTier, Speed) :-
  call(Player, Mon),
  speed_tiers(Mon, SpeedTier, Speed).

speed_tiers_list_(Player, AllTiers) :-
  findall([S, M, ST], speed_tiers_list_(Player, M, ST, S), Res),
  maplist(speed_tiers_list_key, Res, KeyList),
  keysort(KeyList, Sorted),
  reverse(Sorted, SR),
  maplist(speed_tiers_list_line_, SR, AllTiers).

speed_tiers_list(Player) :-
  speed_tiers_list_(Player, AllTiers),
  maplist(printline, AllTiers).

speed_tiers_list(Player1, Player2) :-
  speed_tiers_list_(Player1, Player1Tiers),
  speed_tiers_list_(Player2, Player2Tiers),
  append(Player1Tiers, Player2Tiers, AllTiers),
  maplist(printline, AllTiers).


%%% Type chart
print_type_chart(Player) :-
  team_list(Player, Team),
  findall(Str, print_weakness_count(Team, _, Str), Lines),
  format("~t ~10| 0x  0xA 1/4 1/2 1x  2x  4x~n", []),
  maplist(print_type_chart_line, Lines),
  !, fail.

print_weakness_count(Team, Type, Str) :-
  weakness_count(Team, Type, I, Ia, VS, S, N, W, VW),
  phrase(format_("~a~t~10| ~d   ~d   ~d   ~d   ~d   ~d   ~d", [Type, I, Ia, VS, S, N, W, VW]), Str).
print_type_chart_line(Line) :- format("~s", [Line]), nl.

matchup_count(Team, Type, Matchup, Num) :-
  type(Type),
  findall(Mon, (member(Mon, Team), mon_type_matchup(Mon, Type, Matchup)), Matchups),
  length(Matchups, Num).

weakness_count(Team, Type, I, Ia, VS, S, N, W, VW) :-
  type(Type),
  matchup_count(Team, Type, immune_via_ability, Ia),
  matchup_count(Team, Type, immune, I),
  matchup_count(Team, Type, very_strong, VS),
  matchup_count(Team, Type, strong, S),
  matchup_count(Team, Type, normal, N),
  matchup_count(Team, Type, weak, W),
  matchup_count(Team, Type, very_weak, VW).

%%% Utils
printline([]).
printline(S) :- format("~s~n", [S]).

