:- use_module(library(debug)).
:- use_module(library(time)).

:- use_module(library(os)).
:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(library(files)).

:- use_module('./parser.pl').
:- use_module('./stats.pl').
:- use_module('./print-battle.pl').

% Top-levels
print :- argv(Fps), (maplist(print, Fps) -> halt(0); halt(1)).
unknown :- argv(Fps), (maplist(unknown, Fps) -> halt(0); halt(1)).
% moves :- argv(Fps), (maplist(moves, Fps) -> halt(0); halt(1)).

print(Fp) :- phrase_from_file(log_lines(Ls), Fp), phrase_to_stream(print_battle(Ls), user_output).
unknown(Fp) :- phrase_from_file(log_lines(Ls), Fp), phrase_to_stream(print_unknown_actions(Ls), user_output).

load_log(Fp, Ls) :- phrase_from_file(log_lines(Ls), Fp).

log_file_t(Ls, false) :- length(Ls, L), L #=< 4.
log_file_t(Ls, T) :- length(L1, 4), append(_, [_|L1], Ls), =(L1, ".log", T).

append_log(Fp, Ls0, Ls) :-
  append("./logs/", Fp, RelativePath),
  phrase_from_file(log_lines(Ls1), RelativePath),
  append(Ls0, Ls1, Ls).

all_logs(Dp, Ls) :-
  directory_files(Dp, Files),
  tfilter(log_file_t, Files, LogFiles),
  foldl(append_log, LogFiles, [], Ls).

% term_expansion(load_game(Fp), Terms) :-
%   phrase_from_file(log_lines(Ls), Fp),
%   sort(Ls, Terms).

% load_game("./logs/gen9natdexdraft-2522811785.log").

