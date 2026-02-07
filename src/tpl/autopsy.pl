% https://github.com/smogon/pokemon-showdown/blob/df367633bce4d5d20516da8a98e648c508b3767f/sim/SIM-PROTOCOL.md
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(dif)).
:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(http/http_open)).

run :-
  argv([Fp|_]),
  phrase_from_file(log_lines(Ls), Fp),
  phrase_to_stream(print_battle(Ls), user_output).

print_battle([A|As]) --> print_action(A), !, print_battle(As).
print_battle([])     --> [].

print_action(join(PHandle)) --> format_("~s joined~n", [PHandle]).
print_action(switch(P, Nick, Mon, HP)) --> format_("Player ~d switched in ~s (~s) at ~s HP~n", [P, Nick, Mon, HP]).
print_action(move(_, Nick, Move, _)) --> format_("~s used ~s~n", [Nick, Move]).
print_action(_) --> [].

log_lines([L|Ls]) --> log(L), log_lines(Ls).
log_lines([])     --> [].

% Keep an eye out for a purer way to express this
log(L)               --> action(L), !.
log(u_action(A, Cs)) --> "|", rest(A), ("|", line(Cs) | "\n", { Cs = [] }).

% Meta
action(join(PHandle))                 --> "|j|", line(PHandle).
action(chat(PHandle, Message))        --> "|c|", rest(PHandle), "|", rest(Message), line(_).
action(player(P, PHandle))            --> "|player|", player(P), "|", rest(PHandle), line(_).
action(poke(P, Mon))                  --> "|poke|", rest(P), "|", rest(Mon), line(_).
% Battle actions
action(teampreview)                   --> "|teampreview\n".
action(start)                         --> "|start\n".
action(turn(Timestamp))               --> "|t:|", line(Timestamp).
action(switch(P, Nick, Mon, HP))      --> "|switch|", battle_mon(P, Nick), "|", rest(Mon), "|", rest(HP), line(_).
action(move(P, Nick, Move, Target))   --> "|move|", battle_mon(P, Nick), "|", rest(Move), "|", battle_mon(P, Nick), line(_).
% Control
action(clearpoke)                     --> "|clearpoke\n".
action(space)                         --> "|\n".

battle_mon(P, Nick) --> pos(P, _), ": ", rest(Nick).

player(1) --> "p1".
player(2) --> "p2".
% More positions can be added to support doubles
pos(1, a) --> "p1a".
pos(2, a) --> "p2a".

lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).
line([])      --> ( "\n" | call(eos) ), !.
line([C|Cs])  --> [C], line(Cs).

rest([C|Cs]) --> [C], { [C] \= "|", [C] \= "\n" }, rest(Cs).
rest([])     --> [].

eos([], []).
