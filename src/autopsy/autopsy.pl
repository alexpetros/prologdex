:- use_module(library(clpz)).
:- use_module(library(os)).
:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(reif)).
:- use_module(library(files)).

:- use_module('./parser.pl').
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

all_log_files(Dp, LogFiles) :-
  directory_files(Dp, Files),
  tfilter(log_file_t, Files, LogFiles).

all_logs(Dp, Ls) :-
  all_log_files(Dp, LogFiles),
  foldl(append_log, LogFiles, [], Ls).

% With a lot of help from:
% https://stackoverflow.com/questions/10776759/how-to-count-number-of-element-occurrences-in-a-list-in-prolog
% count_and_remove/5 builds a new list without the Target, while counting the occurences of Target
count_and_remove([], _, [], N, N).
count_and_remove([E|Es], Target, Ls0, N0, N) :-
  if_(dif(E, Target),
    (Ls0 = [E|Ls], N1 #= N0),
    (Ls0 = Ls, N1 #= N0 + 1)
  ),
  count_and_remove(Es, Target, Ls, N1, N).
% count/2 relates a list to a list of E-N terms, where N is the number of occurences of E
count([], []).
count([E|Es], [E-N|Ls]) :-
  count_and_remove(Es, E, Es0, 1, N),
  count(Es0, Ls).

is_move(action(A, _), T) :- =(A, move, T).
hit_or_missed_move(action(move, [_, _, _, Res]), T) :- memberd_t(Res, [none, miss], T).
missed_move(action(move, [_, _, _, Res]), T) :- =(Res, miss, T).
extract_move(action(move, [_, Move, _, _]), Move).

miss_pct(UsedMoves, Move-Misses, move_acc(Move, Usages, Misses, Pct)) :-
  select(Move-Usages, UsedMoves, _),
  Pct is 100 * (1 - (Misses / Usages)).

get_moves_with_filter(Ls, Filter, MovesCount) :-
  tfilter(is_move, Ls, Actions),
  tfilter(Filter, Actions, FilteredMoves),
  maplist(extract_move, FilteredMoves, ExtractedMoves),
  count(ExtractedMoves, MovesCount).

moves_acc(Ls, Moves) :-
  get_moves_with_filter(Ls, missed_move, MissedMoves),
  get_moves_with_filter(Ls, hit_or_missed_move, UsedMoves),
  maplist(miss_pct(UsedMoves), MissedMoves, Moves).

move(Fp, Move, Result) :-
  phrase_from_file(log_lines(Ls), Fp),
  member(move(_, Move, _, Result), Ls).

moves_acc_key(M, Pct-M) :- M = move_acc(_, _, _, Pct).
moves_acc_print(Pct-move_acc(Move, Usages, Misses, Pct)) :-
  Hits #= Usages - Misses,
  format("~s ~2f% (~d/~d)~n", [Move, Pct, Hits, Usages]).
moves_acc_summary :-
  all_logs("./logs", Ls),
  moves_acc(Ls, Moves),
  maplist(moves_acc_key, Moves, KeyList),
  keysort(KeyList, KeyListSorted),
  reverse(KeyListSorted, List),
  maplist(moves_acc_print, List).

% term_expansion(load_game(Fp), Terms) :-
%   phrase_from_file(log_lines(Ls), Fp),
%   sort(Ls, Terms).

% load_game("./logs/gen9natdexdraft-2522811785.log").

