:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(time)).

:- use_module('dex/pokemon.pl').
:- use_module('dex/learnsets.pl').
:- use_module('dex/moves.pl').

:- use_module('draft/move_labels.pl').

:- use_module('type-chart.pl').
:- use_module('stats.pl').
:- use_module('s6.pl').
:- use_module('summary-sheet.pl').

is_sorted([]).
is_sorted([_]).
is_sorted([First|[Second | Rest]]) :-
  First @< Second,
  is_sorted(Rest).
