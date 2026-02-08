% https://github.com/smogon/pokemon-showdown/blob/df367633bce4d5d20516da8a98e648c508b3767f/sim/SIM-PROTOCOL.md
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(dif)).
:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(http/http_open)).

battle :- argv([Fp|_]), battle(Fp).
battle(Fp) :- phrase_from_file(log_lines(Ls), Fp), phrase_to_stream(print_battle(Ls), user_output).
unknown :- argv([Fp|_]), unknown(Fp).
unknown(Fp) :- phrase_from_file(log_lines(Ls), Fp), phrase_to_stream(print_unknown_actions(Ls), user_output).

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
action(poke(P, Mon))                         --> "|poke|", rest(P), "|", mon_id(Mon, _), line(_).
% Battle actions
action(teampreview)                          --> "|teampreview\n".
action(start)                                --> "|start\n".
action(turn(T))                              --> "|turn|", line(TS), { number_chars(T, TS) }.
action(timestamp(Timestamp))                 --> "|t:|", line(Timestamp).
action(switch(mon(P,N), Id, HP))             --> "|switch|", mon(P, N), "|", mon_id(Id, _), hp(HP), line(_).
action(move(mon(P, N), Move, T))             --> "|move|", mon(P, N), "|", rest(Move), "|", target(T), line(_).
% Control
action(clearpoke)                            --> "|clearpoke\n".
action(space)                                --> "|\n".

% Minor actions
action(damage(P, Name, HP))   --> "|-damage|", mon(P, Name), "|", hp(HP), "\n".
action(damage(P, Name, HP, From)) --> "|-damage|", mon(P, Name), "|", hp(HP), "|", rest(From), line(_).

%% Protocol sub-predicates
mon_id(Mon, Details) -->
    (to_comma_or_sep(Mon), "|", { Details = [] })
  | (to_comma_or_sep(Mon), ",", rest(Details), "|").
mon(P, Name) --> pos(P, _), ": ", rest(Name). % p1a: Glimmora

target(mon(P, Name)) --> mon(P, Name).
target(none) --> "[notarget]".

chat_message(html(Message))  --> "/raw ", line(Message).
chat_message(plain(Message)) --> seq_len(A, 5), { A \= "/raw " }, line(Message).

hp(Pct) --> int_seq(Pct), "/", "100".
hp("0") --> "0 fnt".

player(1) --> "p1".
player(2) --> "p2".
% More positions can be added to support doubles
pos(1, a) --> "p1a".
pos(2, a) --> "p2a".
% Position "no" is when there's no target
pos(1, no) --> "p1".
pos(2, no) --> "p2".

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

seq_len(Cs, L)  --> seq(Cs), { length(Cs, L) }.
int_seq([C|Cs]) --> [C], { char_type(C, numeric) }, int_seq(Cs).
int_seq([])     --> [].

%% Playback Predicates
print_battle([A|As]) --> print_action(A), !, print_battle(As).
print_battle([])     --> [].

print_action(joined(PHandle))                                 --> format_("~s joined~n", [PHandle]).
print_action(left(PHandle))                                   --> format_("~s left~n", [PHandle]).
print_action(turn(T))                                         --> format_("~nTurn ~d~n", [T]).
print_action(move(mon(_, Name), Move, target(mon(_, _))))     --> format_("~s used ~s~n", [Name, Move]).
print_action(move(mon(_, Name), Move, target(none)))          --> format_("~s used ~s, but there was no target~n", [Name, Move]).
print_action(damage(_, Mon, HP))                              --> format_("~s took damage, now has ~s% HP~n", [Mon, HP]).
print_action(damage(_, Mon, HP, From))                        --> format_("~s took damage ~s, now has ~s% HP~n", [Mon, From, HP]).
print_action(switch(mon(P, Mon), Mon, HP))                    --> format_("P~d switched in ~s at ~s HP~n", [P, Mon, HP]).
print_action(switch(mon(P, Name), Mon, HP))                   -->
  { dif(Name, Mon) },
  format_("Player ~d switched in ~s (~s) at ~s HP~n", [P, Name, Mon, HP]).
print_action(_)                                               --> [].

print_unknown_actions([A|As])                      --> print_unknown_action_with_message(A), !, print_unknown_actions(As).
print_unknown_actions([])                          --> [].
print_unknown_action(u_action(A, _))               --> format_("~s~n", [A]).
print_unknown_action(_)                            --> [].
print_unknown_action_with_message(u_action(A, M))  --> format_("~s: ~s~n", [A, M]).
print_unknown_action_with_message(_)               --> [].
