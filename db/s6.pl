:- module('s6', [points/2, players/1, remaining_points/2, team_points/2,
                 draft_status/0, team/2, viable/1, undrafted/1, team_list/2,
                 drafted/1, record/4, win/3, loss/3, george/1, nic/1, bird/1,
                 pat/1, justin/1, zack/1, alex/1, andrew/1, mason/1, morry/1, kirk/1, kevin/1,
                 game/3, match/3, result/3]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(dif)).
:- use_module(library(clpz)).

:- use_module('dex/pokemon.pl').
:- use_module('draft/natdex.pl').

record(Player, Wins, Losses, KD) :-
  player(Player),
  findall(Week, win(Week, Player, _), WinsList),
  findall(Week, loss(Week, Player, _), LossesList),
  length(WinsList, Wins),
  length(LossesList, Losses),
  findall(Kills, match_result(Week, Player, Kills, Deaths), KillsList),
  findall(Deaths, match_result(Week, Player, Kills, Deaths), DeathsList),
  sum_list(KillsList, Kills),
  sum_list(DeathsList, Deaths),
  KD is Kills / (Deaths + 0). % You can make this 1 to avoid a divide-by-zero possibility

draft_status :- players(Names), maplist(draft_status_, Names), !.
draft_status_(Player) :-
  remaining_points(Player, Points),
  format("~a: ~d", [Player, Points]),
  nl.

% Obviously some 1-pt mons are viable, but this is a pretty good heuristic
viable(Mon) :- points(Mon, Points), Points #> 1.
undrafted(Mon) :- viable(Mon), \+ drafted(Mon).
drafted(Mon) :- team(_, Mon).

remaining_points(Player, Points) :-
  team_points(Player, Total),
  Points #= 90 - Total.

team_points(Player, Total) :-
  team_list(Player, Team),
  maplist(points, Team, Points),
  sum_list(Points, Total).

team_list(Player, Team) :-
  players(Players),
  member(Player, Players),
  findall(Mon, call(Player, Mon), Team).

team(Player, Mon) :-
  players(Players),
  member(Player, Players),
  call(Player, Mon).

points(Mon, Points) :- natdexdraft(Mon, Points).
points(Mon, 1) :-
  pokemon(Mon),
  findall(BoardMon, natdexdraft(BoardMon, _), BoardMons),
  maplist(dif(Mon), BoardMons).

% Draft facts
players(Ls) :- findall(Player, player(Player), Ls).
player(george).
player(nic).
player(bird).
player(pat).
player(justin).
player(zack).
player(alex).
player(mason).
player(kirk).
player(kevin).
player(andrew).
player(morry).

george(garchomp).
george(tyranitar).
george(zoroarkhisui).
george(rotomheat).
george(ferrothorn).
george(noivern).
george(beedrillmega).
george(gastrodon).
george(passimian).

nic(tapukoko).
nic(ironbundle).
nic(gliscor).
nic(ironhands).
nic(slowbromega).
nic(corviknight).
nic(blissey).
nic(shedinja).

bird(greattusk).
bird(latiosmega).
bird(ragingbolt).
bird(amoonguss).
bird(quagsire).
bird(heatran).
bird(ninetales).
bird(enamorustherian).

pat(ironvaliant).
pat(irontreads).
pat(rotomwash).
pat(hydrapple).
pat(talonflame).
pat(banettemega).
pat(obstagoon).
pat(glimmora).

justin(charizardmegay).
justin(terapagosterastal).
justin(alomomola).
justin(gougingfire).
justin(scizor).
justin(regieleki).
justin(runerigus).
justin(screamtail).
justin(trapinch).

zack(dianciemega).
zack(ogerponhearthflame).
zack(latias).
zack(ursalunabloodmoon).
zack(araquanid).
zack(forretress).
zack(bisharp).
zack(annihilape).
zack(wyrdeer).
zack(boltund).

alex(meowscarada).
alex(weezinggalar).
alex(swampertmega).
alex(latios).
alex(volcarona).
alex(tornadus).
alex(politoed).
alex(archaludon).
alex(beartic).
alex(dusclops).

mason(landorustherian).
mason(melmetal).
mason(slowking).
mason(pidgeotmega).
mason(infernape).
mason(basculegion).
mason(comfey).
mason(dragalge).
mason(raichu).

kirk(gholdengo).
kirk(tapulele).
kirk(kingambit).
kirk(lopunnymega).
kirk(hippowdon).
kirk(tornadustherian).
kirk(goodra).
kirk(glimmet).

kevin(toxapex).
kevin(tinglu).
kevin(blacephalon).
kevin(garganacl).
kevin(kartana).
kevin(venusaurmega).
kevin(articunogalar).
kevin(dondozo).
kevin(rotomfan).
kevin(glastrier).

andrew(scizormega).
andrew(landorus).
andrew(kyurem).
andrew(slowkinggalar).
andrew(taurospaldeaaqua).
andrew(grimmsnarl).
andrew(salazzle).
andrew(togetic).
andrew(aurorus).

morry(mawilemega).
morry(walkingwake).
morry(zapdos).
morry(ursaluna).
morry(lokix).
morry(alakazam).
morry(skarmory).
morry(froslass).
morry(carbink).
morry(dipplin).

match(Week, Self, Other) :-
  week(Week),
  ( game(Week, Self, Other); game(Week, Other, Self) ).

match_result(Week, Player, Kills, Deaths) :-
  ( win(Week, Player, Deaths), Kills #= 6 );
  ( loss(Week, Player, Kills), Deaths #= 6 ).

win(Week, Player, Deaths) :-
  result(Week, Player, Remaining),
  Deaths #= 6 - Remaining.

loss(Week, Player, Kills) :-
  match(Week, Player, Opp),
  result(Week, Opp, OppRemaining),
  Kills #= 6 - OppRemaining.

week(1).
week(2).
week(3).
week(4).
week(5).
week(6).
week(7).
week(8).
week(9).
week(10).
week(11).

game(1, justin, andrew).
game(1, zack, alex).
game(1, morry, pat).
game(1, bird, kevin).
game(1, kirk, mason).
game(1, nic, george).

game(2, alex, andrew).
game(2, george, justin).
game(2, morry, nic).
game(2, kirk, bird).
game(2, kevin, pat).
game(2, mason, zack).

game(3, andrew, mason).
game(3, pat, kirk).
game(3, nic, kevin).
game(3, justin, alex).
game(3, george, morry).
game(3, zack, bird).

game(4, andrew, bird).
game(4, kevin, george).
game(4, kirk, nic).
game(4, morry, justin).
game(4, mason, alex).
game(4, zack, pat).

game(5, justin, mason).
game(5, morry, kevin).
game(5, nic, zack).
game(5, pat, andrew).
game(5, bird, alex).
game(5, george, kirk).

game(6, kirk, morry).
game(6, andrew, nic).
game(6, kevin, justin).
game(6, zack, george).
game(6, mason, bird).
game(6, alex, pat).

game(7, nic, alex).
game(7, george, andrew).
game(7, morry, zack).
game(7, pat, mason).
game(7, bird, justin).
game(7, kevin, kirk).

game(8, justin, kirk).
game(8, zack, kevin).
game(8, alex, george).
game(8, mason, nic).
game(8, andrew, morry).
game(8, bird, pat).

game(9, george, mason).
game(9, pat, justin).
game(9, kevin, andrew).
game(9, kirk, zack).
game(9, nic, bird).
game(9, alex, morry).

game(10, mason, morry).
game(10, bird, george).
game(10, pat, nic).
game(10, andrew, kirk).
game(10, justin, zack).
game(10, alex, kevin).

game(11, andrew, zack).
game(11, mason, kevin).
game(11, kirk, alex).
game(11, bird, morry).
game(11, nic, justin).
game(11, george, pat).

result(1, justin, 6).
result(1, alex, 3).
result(1, pat, 3).
result(1, kevin, 4).
result(1, mason, 1).
result(1, nic, 2).

result(2, justin, 5).
result(2, alex, 6).
result(2, morry, 2).
result(2, zack, 5).
result(2, pat, 4).
result(2, kirk, 3).

result(3, kirk, 2).
result(3, nic, 6).
result(3, justin, 3).
result(3, zack, 3). 
result(3, mason, 2). 