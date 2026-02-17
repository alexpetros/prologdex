:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(dif)).
:- use_module(library(charsio)).

log_lines([L|Ls]) --> log(L), log_lines(Ls).
log_lines([])     --> [].

% Keep an eye out for a purer way to express this
log(L)               --> action(L), !.
log(u_action(A, Cs)) --> "|", rest(A), ("|", line(Cs) | "\n", { Cs = [] }).

%% All the actions
% https://github.com/smogon/pokemon-showdown/blob/df367633bce4d5d20516da8a98e648c508b3767f/sim/SIM-PROTOCOL.md

% Meta
action(player(P, PHandle))                   --> "|player|", player(P), "|", rest(PHandle), line(_).
action(teamsize(Player, Num))                --> "|teamsize|", rest(Player), "|", rest(Num), line(_).
action(poke(P, Mon))                         --> "|poke|", rest(P), "|", mon_id(Mon, _), line(_).
action(rule(R))                              --> "|rule|", rest(R), line(_).
action(win(PHandle))                         --> "|win|", rest(PHandle), line(_).
action(tier(T))                              --> "|tier|", rest(T), line(_).
action(gen(G))                               --> "|gen|", rest(G), line(_).
action(gametype(T))                          --> "|gametype|", rest(T), line(_).
% Battle actions
action(teampreview)                          --> "|teampreview\n".
action(start)                                --> "|start\n".
action(upkeep)                               --> "|upkeep\n".
action(turn(T))                              --> "|turn|", line(TS), { number_chars(T, TS) }.
action(timestamp(Timestamp))                 --> "|t:|", line(Timestamp).
action(switch(mon(P,N), Id, HP))             --> "|switch|", mon(P, N), "|", mon_id(Id, _), hp_status(HP, _), line(_).
action(drag(mon(P,N), Id, HP))               --> "|drag|", mon(P, N), "|", mon_id(Id, _), hp_status(HP, _), line(_).
action(replace(mon(P,N), Id))                --> "|replace|", mon(P, N), "|", mon_id(Id, _), line(_).
action(move(mon(P, N), Move, T, miss))       --> "|move|", mon(P, N), "|", rest(Move), "|", target(T), "|[miss]", line(_).
action(move(mon(P, N), Move, T, notarget))   --> "|move|", mon(P, N), "|", rest(Move), "|", target(T), "|[notarget]", line(_).
action(move(mon(P, N), Move, none, still))   --> "|move|", mon(P, N), "|", rest(Move), "||[still]", line(_).
action(move(mon(P, N), Move, T, none))       --> "|move|", mon(P, N), "|", rest(Move), "|", target(T), line(_).
action(faint(mon(P, N)))                     --> "|faint|", mon(P, N), line(_).
action(detailschange(mon(P,N), To))          --> "|detailschange|", mon(P,N), "|", rest(To), line(_).
action(cant(mon(P,N), Why))                  --> "|cant|", mon(P, N), "|", rest(Why), line(_).
action(cant(mon(P,N), Why, What))            --> "|cant|", mon(P, N), "|", rest(Why), "|", rest(What), line(_).
% Control
action(inactive(Msg))                        --> "|inactive|", rest(Msg), line(_).
action(inactiveoff(Msg))                     --> "|inactiveoff|", rest(Msg), line(_).
action(space)                                --> "|\n".
action(clearpoke)                            --> "|clearpoke\n".
% Chat
action(joined(PHandle))                      --> "|j|", line(PHandle).
action(left(PHandle))                        --> "|l|", line(PHandle).
action(name(PHandle, A))                     --> "|n|", rest(PHandle), "|", rest(A), line(_). % idk
action(chat(PHandle, Message))               --> "|c|", rest(PHandle), "|", chat_message(Message).

% Minor actions
action(heal(P, Name, HP))        --> "|-heal|", mon(P, Name), "|", hp_status(HP, _), line(_).
action(damage(P, Name, HP))      --> "|-damage|", mon(P, Name), "|", hp_status(HP, _), "\n".
action(damage(P, Name, HP, F))   --> "|-damage|", mon(P, Name), "|", hp_status(HP, _), "|", from(F), line(_).
action(sethp(P, Name, HP, F))    --> "|-sethp|", mon(P, Name), "|", hp_status(HP, _), "|", from(F), line(_).
action(supereffective(mon(P,N))) --> "|-supereffective|", mon(P, N), line(_).
action(resisted(mon(P,N)))       --> "|-resisted|", mon(P, N), line(_).
action(crit(mon(P,N)))           --> "|-crit|", mon(P, N), line(_).
action(miss(mon(P,N)))           --> "|-miss|", mon(P, N), "|", target(_), line(_).
action(hitcount(mon(P,N), Num))  --> "|-hitcount|", mon(P, N), "|", rest(Num), line(_).
action(prepare(mon(P,N), Move))  --> "|-prepare|", mon(P, N), "|", rest(Move), line(_).
action(anim(mon(P,N), Move, T))  --> "|-anim|", mon(P, N), "|", rest(Move), "|", target(T), line(_).
action(fail(mon(P,N)))           --> "|-fail|", mon(P, N), line(_).
action(item(mon(P,N), I))        --> "|-item|", mon(P, N), "|", rest(I), line(_). % TODO [From]; also frisk
action(singlemove(mon(P,N), Move))  --> "|-singlemove|", mon(P, N), "|", rest(Move), line(_). %idk what this does

action(ability(mon(P,N), A))          --> "|-ability|", mon(P,N), "|", rest(A), line(_).
action(status(mon(P,N), S))           --> "|-status|", mon(P,N), "|", rest(S), line(_).
action(curestatus(mon(P,N), S))       --> "|-curestatus|", mon(P,N), "|", rest(S), line(_).
action(immune(mon(P,N)))              --> "|-immune|", mon(P,N), line(_). % TODO [From]
action(singleturn(mon(P,N)))          --> "|-singleturn|", mon(P,N), line(_). % TODO [From]
action(activate(mon(P,N), Effect))    --> "|-activate|", mon(P,N), "|", rest(Effect), line(_).
action(start(mon(P,N), A))            --> "|-start|", mon(P,N), "|", rest(A), line(_).
action(mega(mon(P,N), Species, Item)) --> "|-mega|", mon(P,N), "|", rest(Species), "|", rest(Item), line(_).

action(end(mon(P,N), Effect))         --> "|-end|", mon(P,N), "|", rest(Effect), line(_).
action(enditem(mon(P,N), Item))       --> "|-enditem|", mon(P,N), "|", rest(Item), line(_). % TODO why
action(weather(W))                    --> "|-weather|", rest(W), line(_). % TODO handle upkeep, chilly
action(fieldstart(Cond))              --> "|-fieldstart|", rest(Cond), line(_). % There's a little more to this
action(fieldend(Cond))                --> "|-fieldend|", rest(Cond), line(_).
action(sidestart(side(P,H), Cond))    --> "|-sidestart|", side(P,H), "|", rest(Cond), line(_).
action(sidend(side(P,H), Cond))       --> "|-sideend|", side(P,H), "|", rest(Cond), line(_).

action(boost(mon(P,N), Stat, Stages))   --> "|-boost|", mon(P, N), "|", rest(Stat), "|", rest(Stages), line(_).
action(unboost(mon(P,N), Stat, Stages)) --> "|-unboost|", mon(P, N), "|", rest(Stat), "|", rest(Stages), line(_).
action(clearboost(mon(P,N)))            --> "|-clearboost|", mon(P, N), line(_).

action(hint(Msg))                            --> "|-hint|", rest(Msg), line(_).

%% Protocol sub-predicates
mon_id(Mon, Details)  -->
    (to_comma_or_sep(Mon), "|", { Details = [] })
  | (to_comma_or_sep(Mon), ",", rest(Details), ("|" | "\n")).
mon(P, Name)          --> pos(P, _), ": ", rest(Name). % p1a: Glimmora
side(Player, PHandle) --> player(Player), ": ", rest(PHandle).

from(F)                      --> "[from] ", rest(F).
target(mon(P, Name))         --> mon(P, Name).
target(none)                 --> "". % e.g. failure or two-turn move like Solar Beam

chat_message(html(Message))  --> "/raw ", line(Message).
chat_message(plain(Message)) -->
  line(Message),
  {
    length(Message, L),
    (
      L #=< 5
    ; L #> 5, length(Start, 5), append(Start, _, Message), dif(Start, "/raw ")
    )
  }.

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


