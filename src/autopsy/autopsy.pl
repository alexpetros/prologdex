:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(library(os)).
:- use_module(library(dif)).
:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).

:- use_module('./parser.pl').
:- use_module('./print-battle.pl').

% Top-levels
print :- argv(Fps), (maplist(print, Fps) -> halt(0); halt(1)).
unknown :- argv(Fps), (maplist(unknown, Fps) -> halt(0); halt(1)).
% moves :- argv(Fps), (maplist(moves, Fps) -> halt(0); halt(1)).

print(Fp) :- phrase_from_file(log_lines(Ls), Fp), phrase_to_stream(print_battle(Ls), user_output).
unknown(Fp) :- phrase_from_file(log_lines(Ls), Fp), phrase_to_stream(print_unknown_actions(Ls), user_output).

load_log(Fp, Ls) :- phrase_from_file(log_lines(Ls), Fp).

move_stats(Fp, Move, NumU, HitPct) :-
  setof(Result, move(Fp, Move, none), Hits),
  setof(Result, move(Fp, Move, miss), Misses),
  length(Hits, NumH),
  length(Misses, NumM),
  NumU #= NumH + NumM,
  HitPct is NumH/NumU.

move(Fp, Move, Result) :-
  phrase_from_file(log_lines(Ls), Fp),
  member(move(_, Move, _, Result), Ls).

miss_rate(Ls, Move, R) :-
  move_usages(Ls, Move, U),
  move_misses(Ls, Move, M),
  R is M/U.

sorted([], []).
sorted([], []).

term_expansion(load_game(Fp), Terms) :-
  phrase_from_file(log_lines(Ls), Fp),
  sort(Ls, Terms).

% load_game("./logs/gen9natdexdraft-2522811785.log").

