:- use_module('dex.pro').

learns_removal(Mon) :-
  learns(Mon, rapidspin);
  learns(Mon, defog);
  learns(Mon, courtchange);
  learns(Mon, tidyup).
