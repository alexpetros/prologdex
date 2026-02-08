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
  run(Fp).

run(Fp) :-
  phrase_from_file(log_lines(Ls), Fp),
  phrase_to_stream(print_unknown_actions(Ls), user_output).


log_lines([L|Ls]) --> log(L), log_lines(Ls).
log_lines([])     --> [].

% Keep an eye out for a purer way to express this
log(L)               --> action(L), !.
log(u_action(A, Cs)) --> "|", rest(A), ("|", line(Cs) | "\n", { Cs = [] }).

%% All the actions
% Meta
action(joined(PHandle))                      --> "|j|", line(PHandle).
action(left(PHandle))                        --> "|l|", line(PHandle).
action(chat(PHandle, Message))               --> "|c|", rest(PHandle), "|", chat_message(Message).
action(player(P, PHandle))                   --> "|player|", player(P), "|", rest(PHandle), line(_).
action(poke(P, Mon))                         --> "|poke|", rest(P), "|", id_mon(Mon, _), line(_).
% Battle actions
action(teampreview)                          --> "|teampreview\n".
action(start)                                --> "|start\n".
action(turn(T))                              --> "|turn|", line(TS), { number_chars(T, TS) }.
action(timestamp(Timestamp))                 --> "|t:|", line(Timestamp).
action(switch(P, Nick, Mon, HP))             --> "|switch|", battle_mon(P, Nick), "|", id_mon(Mon, _), rest(HP), line(_).
action(move(P, Nick, Move, _))               --> "|move|", battle_mon(P, Nick), "|", rest(Move), "|", battle_mon(_, _), line(_).
% Control
action(clearpoke)                            --> "|clearpoke\n".
action(space)                                --> "|\n".

chat_message(html(Message))  --> "/raw ", line(Message).
chat_message(plain(Message)) --> seq(A), { length(A, 5), A \= "/raw " }, rest(Message), "\n".

%% Protocol sub-predicates
id_mon(Mon, Details) -->
    (to_comma_or_sep(Mon), "|", { Details = [] })
  | (to_comma_or_sep(Mon), ",", rest(Details), "|").
battle_mon(P, Nick) --> pos(P, _), ": ", rest(Nick).

player(1) --> "p1".
player(2) --> "p2".
% More positions can be added to support doubles
pos(1, a) --> "p1a".
pos(2, a) --> "p2a".

% Log parsing predicated
to_comma_or_sep([C|Cs]) --> [C], { [C] \= "\n", [C] \= "," }, to_comma_or_sep(Cs).
to_comma_or_sep([])     --> [].
rest([C|Cs]) --> [C], { [C] \= "|", [C] \= "\n" }, rest(Cs).
rest([])     --> [].

%% General parsing predicates
lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).
line([])      --> ( "\n" | call(eos) ), !.
line([C|Cs])  --> [C], line(Cs).
eos([], []).

%% Playback Predicates
print_battle([A|As]) --> print_action(A), !, print_battle(As).
print_battle([])     --> [].

print_action(joined(PHandle))            --> format_("~s joined~n", [PHandle]).
print_action(left(PHandle))              --> format_("~s left~n", [PHandle]).
print_action(turn(T))                    --> format_("~nTurn ~d~n", [T]).
print_action(move(_, Nick, Move, _))     --> format_("~s used ~s~n", [Nick, Move]).
print_action(switch(P, Mon, Mon, HP))    --> format_("P~d switched in ~s at ~s HP~n", [P, Mon, HP]).
print_action(switch(P, Nick, Mon, HP))   -->
  { dif(Nick, Mon) },
  format_("Player ~d switched in ~s (~s) at ~s HP~n", [P, Nick, Mon, HP]).
print_action(_)                          --> [].

print_unknown_actions([A|As]) --> print_unknown_action(A), !, print_unknown_actions(As).
print_unknown_actions([])     --> [].
print_unknown_action(u_action(A, _))  --> format_("~s~n", [A]).
print_unknown_action(_)               --> [].
