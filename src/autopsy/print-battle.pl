%% Playback Predicates
print_battle([A|As]) --> print_action(A), !, print_battle(As).
print_battle([])     --> [].

print_action(joined(PHandle))                            --> format_("~s joined~n", [PHandle]).
print_action(left(PHandle))                              --> format_("~s left~n", [PHandle]).
print_action(win(PHandle))                               --> format_("~n~s won!~n~n", [PHandle]).
print_action(turn(T))                                    --> format_("~nTurn ~d~n", [T]).
print_action(status(mon(_, Name), S))                    --> format_("~s got status ~s~n", [Name, S]).
print_action(curestatus(mon(_, Name), S))                --> format_("~s lost status ~s~n", [Name, S]).
print_action(move(mon(_, Name), Move, _, none))          --> format_("~s used ~s~n", [Name, Move]).
print_action(move(mon(_, Name), Move, _, miss))          --> format_("~s used ~s, but it missed.~n", [Name, Move]).
print_action(move(mon(_, Name), Move, _, still))         --> format_("~s used ~s...~n", [Name, Move]).
print_action(move(mon(_, Name), Move, _, notarget))      --> format_("~s used ~s, but there was no target~n", [Name, Move]).
print_action(damage(_, Mon, HP))                         --> format_("~s took damage, now has ~s% HP~n", [Mon, HP]).
print_action(damage(_, Mon, HP, From))                   --> format_("~s took damage from ~s, now has ~s% HP~n", [Mon, From, HP]).
print_action(heal(_, Mon, HP))                           --> format_("~s healed, now has ~s% HP~n", [Mon, HP]).
print_action(supereffective(_))                          --> "It's super effective!\n".
print_action(resisted(_))                                --> "It's not very effective...".
print_action(crit(_))                                    --> "A critical hit!\n".
print_action(faint(mon(_, N)))                           --> format_("~s fainted.~n", [N]).
print_action(weather(C))                                 --> format_("The weather changed to: ~s~n", [C]).
print_action(prepare(mon(_,N), Move))                    --> format_("~s is preparing ~s~n", [N, Move]).
print_action(anim(mon(_,N), Move, _))                    --> format_("~s used ~s~n", [N, Move]).

print_action(boost(mon(_, N), Stat, Stages))             --> format_("~s's ~s was raised by ~s.~n", [N, Stat, Stages]).
print_action(unboost(mon(_, N), Stat, Stages))           --> format_("~s's ~s fell by ~s.~n", [N, Stat, Stages]).
print_action(drag(mon(_, Mon), _, HP))                   --> format_("~s was dragged out at ~s% HP~n", [Mon, HP]).
print_action(switch(mon(P, Mon), Mon, HP))               --> format_("P~d switched in ~s at ~s% HP~n", [P, Mon, HP]).
print_action(switch(mon(P, N), Mon, HP))                 -->
  { dif(N, Mon) },
  format_("Player ~d switched in ~s (~s) at ~s HP~n", [P, N, Mon, HP]).
print_action(_)                                          --> [].

print_unknown_actions([A|As])                      --> print_unknown_action_with_message(A), !, print_unknown_actions(As).
print_unknown_actions([])                          --> [].
print_unknown_action(u_action(A, _))               --> format_("~s~n", [A]).
print_unknown_action(_)                            --> [].
print_unknown_action_with_message(u_action(A, M))  --> format_("~s|~s~n", [A, M]).
print_unknown_action_with_message(_)               --> [].


