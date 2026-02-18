:- use_module(library(clpz)).
:- use_module(library(os)).
:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(reif)).

:- use_module('./parser.pl').
:- use_module('./print-battle.pl').

% Top-levels
print :- argv(Fps), (maplist(print, Fps) -> halt(0); halt(1)).
unknown :- argv(Fps), (maplist(unknown, Fps) -> halt(0); halt(1)).
% moves :- argv(Fps), (maplist(moves, Fps) -> halt(0); halt(1)).

print(Fp) :- phrase_from_file(log_lines(Ls), Fp), phrase_to_stream(print_battle(Ls), user_output).
unknown(Fp) :- phrase_from_file(log_lines(Ls), Fp), phrase_to_stream(print_unknown_actions(Ls), user_output).

load_log(Fp, Ls) :- phrase_from_file(log_lines(Ls), Fp).

is_move(action(A, _), T) :- =(A, move, T).
missed_move(action(move, [_, _, _, Res]), T) :- =(Res, miss, T).
extract_move(action(move, [_, Move, _, _]), Move).

count([], []).
count([E|Es], Ls) :- count_([E|Es], [], Ls).
count_([], Ls, Ls).
count_([E|Es], Ls0, Ls) :-
  if_(
    memberd_t(E-C0, Ls0),
    ( select(E-C0, Ls0, Ls1), C #= C0 + 1, append(Ls1, [E-C], Ls2) ),
    append(Ls0, [E-1], Ls2)
  ),
  count_(Es, Ls2, Ls).

% count_([E|Es], Ls0, Ls) :-
%   if_(
%     memberd_t(E-C0, Ls0),
%     ( select(E-C0, Ls0, Ls1), C #= C0 + 1, append(Ls1, [E-C], Ls2) ),
%     append(Ls0, [E-1], Ls2)
%   ),
%   count_(Es, Ls2, Ls).

moves(Fp, Moves) :-
  phrase_from_file(log_lines(Ls), Fp),
  tfilter(is_move, Ls, Actions),
  tfilter(missed_move, Actions, MissedMoves),
  maplist(extract_move, MissedMoves, Moves)
  .

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

