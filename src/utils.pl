:- use_module(library(dif)).

%%% Utils
printline([]).
printline(S) :- format("~s~n", [S]).

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

filtermap(Filter, Map, Ls0, Ls) :-
  tfilter(Filter, Ls0, Ls1),
  maplist(Map, Ls1, Ls).
