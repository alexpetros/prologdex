:- use_module(library(dcgs)).
:- use_module('../utils.pl').


% mon(Id, Name, Player).

state(S0, S), [S] --> [S0].
stats([action(poke, [P,Id])|Ls]) -->
  state(S0, S),
  {
    M = mon(Id, none, P, 0, 0),
    append(S0, [M], S)
  },
  stats(Ls).
stats([action(detailschange, [mon(P,N), Id])|Ls]) -->
  state(S0, S),
  {
    append(S1, [mon(_, N, P, K, D)|S2], S0),
    append(S1, [mon(Id, N, P, K, D)|S2], S)
  },
  stats(Ls).
stats([action(switch, [mon(P,N), Id, _])|Ls]) -->
  state(S0, S),
  {
    % format("Switched in ~s (~w)~n", [N, Id]),
    append(S1, [mon(Id, _, P, K, 0)|S2], S0),
    append(S1, [mon(Id, N, P, K, 0)|S2], S)
  },
  stats(Ls).
stats([action(faint, [mon(P, N)])|Ls]) -->
  state(S0, S),
  {
    append(S1, [mon(Id, N, P, K, 0)|S2], S0),
    append(S1, [mon(Id, N, P, K, 1)|S2], S)
  },
  stats(Ls).

stats([action(A, _)|Ls]) --> { memberd_t(A, [poke, switch, detailschange, faint], false) }, stats(Ls).
stats([]) --> [].

%% Move accuracy
hit_or_missed_move(action(A, _), false) :- dif(A, move).
hit_or_missed_move(action(move, [_,_,_,Res]), T) :- memberd_t(Res, [none, miss], T).
extract_move(action(move, [_,Move,_,Res]), Move-Res).

miss_pct(UsedMoves, Move-Misses, move_acc(Move, Usages, Misses, Pct)) :-
  select(Move-Usages, UsedMoves, _),
  Pct is 100 * (1 - (Misses / Usages)).
remove_part(M-_, M).
missed(_-R, T) :- =(R, miss, T).

moves_acc(Ls, MoveAccs) :-
  tfilter(hit_or_missed_move, Ls, HitOrMissedMoves),
  maplist(extract_move, HitOrMissedMoves, Moves),
  maplist(remove_part, Moves, UsedMoves),
  count(UsedMoves, UsedMovesC), % the slowness is all here
  filtermap(missed, remove_part, Moves, MissedMoves),
  count(MissedMoves, MissedMovesC),
  maplist(miss_pct(UsedMovesC), MissedMovesC, MoveAccs).

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


