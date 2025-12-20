:- module('s6', [points/2, players/1, remaining_points/2, team_points/2,
                 draft_status/0, team/2, viable/1, undrafted/1, team_list/2,
                 drafted/1, george/1, nic/1, bird/1, pat/1, justin/1, zack/1,
                 alex/1, andrew/1, mason/1, morry/1, kirk/1, kevin/1]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(dif)).
:- use_module(library(clpz)).

:- use_module('dex/pokemon.pl').
:- use_module('draft/natdex.pl').

draft_status :- players(Names), maplist(draft_status_, Names), !.
draft_status_(Player) :-
  remaining_points(Player, Points),
  format("~a: ~d", [Player, Points]),
  nl.

% Obviously some 1-pt mons are viable, but this is a pretty good heuristic
viable(Mon) :- points(Mon, Points), Points #> 1.
undrafted(Mon) :- viable(Mon), \+ drafted(Mon).
drafted(Mon) :- george(Mon); nic(Mon); bird(Mon); pat(Mon); justin(Mon); zack(Mon);
  alex(Mon); mason(Mon); kirk(Mon); kevin(Mon); andrew(Mon); morry(Mon).

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
players([george, nic, bird, pat, justin, zack, alex, mason, kirk, kevin, andrew, morry]).

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
justin(terapagos).
justin(alomomola).
justin(gougingfire).
justin(scizor).
justin(regieleki).
justin(runerigus).
justin(screamtail).
justin(rampardos).

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
alex(nidoking).
alex(swampertmega).
alex(latios).
alex(ribombee).
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
