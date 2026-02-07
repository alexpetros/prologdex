% https://github.com/smogon/pokemon-showdown/blob/df367633bce4d5d20516da8a98e648c508b3767f/sim/SIM-PROTOCOL.md
:- use_module(library(lists)).
:- use_module(library(dif)).
:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(http/http_open)).

run :-
  phrase_from_file(log_lines(Ls), "./logs/gen9natdexdraft-2501138619.log"),
  phrase_to_stream(print_logs(Ls), user_output).

print_logs([L|Ls]) --> format_("~q~n", [L]), print_logs(Ls).
print_logs([]) --> [].

log_lines([L|Ls]) --> log(L), log_lines(Ls).
log_lines([]) --> [].

log(L) --> action(L), !.
log(u_action(A, Cs)) --> "|", rest(A), ("|", line(Cs) | "\n", { Cs = [] }).

action(join(PHandle)) --> "|j|", line(PHandle).
action(chat(PHandle, Message)) --> "|c|", rest(PHandle), "|", rest(Message), line(_).
action(player(P, PHandle)) --> "|player|", rest(P), "|", rest(PHandle), line(_).
action(poke(P, Mon)) --> "|poke|", rest(P), "|", rest(Mon), line(_).
action(clearpoke) --> "|clearpoke\n".
action(space) --> "|\n".
action(teampreview) --> "|teampreview\n".

lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).
line([])     --> ( "\n" | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

rest([C|Cs]) --> [C], { [C] \= "|", [C] \= "\n" }, rest(Cs).
rest([]) --> [].

eos([], []).
