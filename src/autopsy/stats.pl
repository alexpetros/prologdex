:- use_module('../utils.pl').

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


