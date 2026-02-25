:- use_module(library(debug)).
:- use_module(library(time)).

:- use_module(library(os)).
:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(library(files)).

:- use_module('./autopsy/parser.pl').
:- use_module('./autopsy/stats.pl').
:- use_module('./autopsy/print-battle.pl').

% Top-levels
print :- argv(Fps), (maplist(print, Fps) -> halt(0); halt(1)).
unknown :- argv(Fps), (maplist(unknown, Fps) -> halt(0); halt(1)).

print(Fp) :- phrase_from_file(log_lines(Ls), Fp), phrase_to_stream(print_battle(Ls), user_output).
unknown(Fp) :- phrase_from_file(log_lines(Ls), Fp), phrase_to_stream(print_unknown_actions(Ls), user_output).

load_log(Fp, Ls) :- phrase_from_file(log_lines(Ls), Fp).

log_file_t(Ls, false) :- length(Ls, L), L #=< 4.
log_file_t(Ls, T) :- length(L1, 4), append(_, [_|L1], Ls), =(L1, ".log", T).

append_log(Fp, Lss0, Lss) :-
  phrase_from_file(log_lines(Ls1), Fp),
  append(Lss0, [Ls1], Lss).

directory_log_files(Dp, LogFiles) :-
  directory_files(Dp, Files),
  tfilter(log_file_t, Files, RelativeLogFiles),
  append(Dp, "/", Base),
  maplist(append(Base), RelativeLogFiles, LogFiles).
all_logs(Dp, Lss) :- directory_log_files(Dp, LogFiles), foldl(append_log, LogFiles, [], Lss).

summary_line(kd(S, K, D), Str) :- phrase(format_("~s - ~d|~d", [S, K, D]), Str).
game_summary(Ls, S) :-
  phrase(stats(Ls), [[]], [Stats]),
  maplist(get_kd, Stats, Kds),
  maplist(summary_line, Kds, S).
print_game_summary(Ls) :-
  Ls = [action(joined, [P1]), action(joined, [P2])| _],
  format("~s vs ~s~n", [P1, P2]),
  game_summary(Ls, S),
  maplist(printline, S).

players(Ls, P1, P2) :- Ls = [action(joined, [['☆'|P1]]), action(joined, [['☆'|P2]])| _].
log_player_opp(Fp, P, Opp) :-
  log_file(Fp),
  load_log(Fp, Ls),
  ( players(Ls, P, Opp) ; players(Ls, Opp, P) ).

log_file(Fp) :-
  all_log_files("./logs", LogFiles),
  member(Fp, LogFiles).

all_stats(Lss, Statss) :- maplist(get_stats, Lss, Statss).

% term_expansion(load_game(Fp), Terms) :-
%   phrase_from_file(log_lines(Ls), Fp),
%   sort(Ls, Terms).

% load_game("./logs/gen9natdexdraft-2522811785.log").

