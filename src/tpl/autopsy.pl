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
% https://github.com/smogon/pokemon-showdown/blob/df367633bce4d5d20516da8a98e648c508b3767f/sim/SIM-PROTOCOL.md

% Meta
action(joined(PHandle))                      --> "|j|", line(PHandle).
action(left(PHandle))                        --> "|l|", line(PHandle).
action(chat(PHandle, Message))               --> "|c|", rest(PHandle), "|", chat_message(Message).
action(player(P, PHandle))                   --> "|player|", player(P), "|", rest(PHandle), line(_).
action(poke(P, Mon))                         --> "|poke|", rest(P), "|", mon_id(Mon, _), line(_).
action(rule(R))                              --> "|rule|", rest(R), line(_).
action(win(PHandle))                         --> "|win|", rest(PHandle), line(_).
% Battle actions
action(teampreview)                          --> "|teampreview\n".
action(start)                                --> "|start\n".
action(upkeep)                               --> "|upkeep\n".
action(turn(T))                              --> "|turn|", line(TS), { number_chars(T, TS) }.
action(timestamp(Timestamp))                 --> "|t:|", line(Timestamp).
action(switch(mon(P,N), Id, HP))             --> "|switch|", mon(P, N), "|", mon_id(Id, _), hp_status(HP, _), line(_).
action(move(mon(P, N), Move, T, notarget))   --> "|move|", mon(P, N), "|", rest(Move), "|", target(T), "|[notarget]", line(_).
action(move(mon(P, N), Move, T))             --> "|move|", mon(P, N), "|", rest(Move), "|", target(T), line(_).
action(faint(mon(P, N)))                     --> "|faint|", mon(P, N), line(_).
% Control
action(space)                                --> "|\n".
action(clearpoke)                            --> "|clearpoke\n".

% Minor actions
action(heal(P, Name, HP))        --> "|-heal|", mon(P, Name), "|", hp_status(HP, _), "\n".
action(damage(P, Name, HP))      --> "|-damage|", mon(P, Name), "|", hp_status(HP, _), "\n".
action(damage(P, Name, HP, F))   --> "|-damage|", mon(P, Name), "|", hp_status(HP, _), "|", from(F), line(_).
action(supereffective(mon(P,N))) --> "|-supereffective|", mon(P, N), line(_).
action(resisted(mon(P,N)))       --> "|-resisted|", mon(P, N), line(_).

action(ability(mon(P,N), A))          --> "|-ability|", mon(P,N), "|", rest(A), line(_).
action(status(mon(P,N), S))           --> "|-status|", mon(P,N), "|", rest(S), line(_).
action(curestatus(mon(P,N), S))       --> "|-curestatus|", mon(P,N), "|", rest(S), line(_).
action(activate(mon(P,N), A))         --> "|-activate|", mon(P,N), "|", rest(A), line(_).
action(start(mon(P,N), A))            --> "|-start|", mon(P,N), "|", rest(A), line(_).

action(weather(W))                    --> "|-weather|", rest(W), line(_). % TODO handle upkeep, chilly
action(fieldstart(Cond))              --> "|-fieldstart|", rest(Cond), line(_). % There's a little more to this
action(fieldend(Cond))                --> "|-fieldend|", rest(Cond), line(_).
action(sidestart(side(P,H), Cond))    --> "|-sidestart|", side(P,H), "|", rest(Cond), line(_).
action(sidend(side(P,H), Cond))       --> "|-sideend|", side(P,H), "|", rest(Cond), line(_).

action(boost(mon(P,N), Stat, Stages))   --> "|-boost|", mon(P, N), "|", rest(Stat), "|", rest(Stages), line(_).
action(unboost(mon(P,N), Stat, Stages)) --> "|-unboost|", mon(P, N), "|", rest(Stat), "|", rest(Stages), line(_).

%% Protocol sub-predicates
mon_id(Mon, Details)  -->
    (to_comma_or_sep(Mon), "|", { Details = [] })
  | (to_comma_or_sep(Mon), ",", rest(Details), "|").
mon(P, Name)          --> pos(P, _), ": ", rest(Name). % p1a: Glimmora
side(Player, PHandle) --> player(Player), ": ", rest(PHandle).

from(F)                      --> "[from] ", rest(F).
target(mon(P, Name))         --> mon(P, Name).
chat_message(html(Message))  --> "/raw ", line(Message).
chat_message(plain(Message)) --> seq_len(A, 5), { A \= "/raw " }, line(Message).

hp_status(Pct, none) --> int_seq(Pct), "/", "100".
hp_status(Pct, S) --> int_seq(Pct), "/", "100", rest(S).
hp_status("0", fnt) --> "0 fnt".

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

print_action(joined(PHandle))                            --> format_("~s joined~n", [PHandle]).
print_action(left(PHandle))                              --> format_("~s left~n", [PHandle]).
print_action(win(PHandle))                               --> format_("~n~s won!~n~n", [PHandle]).
print_action(turn(T))                                    --> format_("~nTurn ~d~n", [T]).
print_action(status(mon(_, Name), S))                    --> format_("~s got status ~s~n", [Name, S]).
print_action(curestatus(mon(_, Name), S))                --> format_("~s lost status ~s~n", [Name, S]).
print_action(move(mon(_, Name), Move, _))                --> format_("~s used ~s~n", [Name, Move]).
print_action(move(mon(_, Name), Move, _, notarget))      --> format_("~s used ~s, but there was no target~n", [Name, Move]).
print_action(damage(_, Mon, HP))                         --> format_("~s took damage, now has ~s% HP~n", [Mon, HP]).
print_action(damage(_, Mon, HP, From))                   --> format_("~s took damage from ~s, now has ~s% HP~n", [Mon, From, HP]).
print_action(heal(_, Mon, HP))                           --> format_("~s healed, now has ~s% HP~n", [Mon, HP]).
print_action(supereffective(_))                          --> "It's super effective!\n".
print_action(resisted(_))                                --> "It's not very effective...".
print_action(faint(mon(_, N)))                           --> format_("~s fainted.~n", [N]).
print_action(weather(C))                                 --> format_("The weather changed to: ~s~n", [C]).

print_action(boost(mon(_, N), Stat, Stages))             --> format_("~s's ~s was raised by ~s.~n", [N, Stat, Stages]).
print_action(unboost(mon(_, N), Stat, Stages))           --> format_("~s's ~s fell by ~s.~n", [N, Stat, Stages]).
print_action(switch(mon(P, Mon), Mon, HP))               --> format_("P~d switched in ~s at ~s% HP~n", [P, Mon, HP]).
print_action(switch(mon(P, N), Mon, HP))                 -->
  { dif(N, Mon) },
  format_("Player ~d switched in ~s (~s) at ~s HP~n", [P, N, Mon, HP]).
print_action(_)                                          --> [].

print_unknown_actions([A|As])                      --> print_unknown_action_with_message(A), !, print_unknown_actions(As).
print_unknown_actions([])                          --> [].
print_unknown_action(u_action(A, _))               --> format_("~s~n", [A]).
print_unknown_action(_)                            --> [].
print_unknown_action_with_message(u_action(A, M))  --> format_("~s: ~s~n", [A, M]).
print_unknown_action_with_message(_)               --> [].
