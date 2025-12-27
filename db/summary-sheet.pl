:- module('summary-sheet', [team_speed_tiers/1, team_speed_tiers/2,
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

%%% Speed tiers
speed_tier_str(max_positive_plus_one, "Max+ (+1)").
speed_tier_str(max_neutral_plus_one, "Max  (+1)").
speed_tier_str(max_positive, "Max+").
speed_tier_str(max_neutral, "Max").
speed_tier_str(default_stat, "Default").
speed_tier_str(min_stat, "Min").

team_speed_tiers_(Player, Mon, SpeedTier, Speed) :-
  call(Player, Mon),
  speed_tiers(Mon, SpeedTier, Speed).

team_speed_tiers_list_key([Speed | Tail], Speed-Tail).
team_speed_tiers_line_(Speed-[Mon|[SpeedTier]], S) :-
  speed_tier_str(SpeedTier, STStr),
  phrase(format_("~d~t~3| | ~a~t~25| | ~s", [Speed, Mon, STStr]), S).

team_speed_tiers_(Player, AllTiers) :-
  findall([S, M, ST], team_speed_tiers_(Player, M, ST, S), Res),
  maplist(team_speed_tiers_list_key, Res, KeyList),
  keysort(KeyList, Sorted),
  reverse(Sorted, SR),
  maplist(team_speed_tiers_line_, SR, AllTiers).

team_speed_tiers(Player) :-
  team_speed_tiers_(Player, AllTiers),
  maplist(printline, AllTiers).

team_speed_tiers(Player1, Player2) :-
  team_speed_tiers_(Player1, Player1Tiers),
  team_speed_tiers_(Player2, Player2Tiers),
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

