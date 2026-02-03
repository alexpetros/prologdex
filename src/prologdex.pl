:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(time)).

:- use_module('dex/pokemon.pl').
:- use_module('dex/learnsets.pl').
:- use_module('dex/moves.pl').

:- use_module('draft/move_labels.pl').
:- use_module('calc/type-chart.pl').
:- use_module('calc/stats.pl').
:- use_module('tpl/s6.pl').
:- use_module('tpl/summary-sheet.pl').

is_sorted([]).
is_sorted([_]).
is_sorted([First|[Second | Rest]]) :-
  First @< Second,
  is_sorted(Rest).
