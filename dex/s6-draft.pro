:- module('s6-draft', [s6_available/1, s6_taken/1]).

george(garchomp).
george(tyranitar).
george(zoroarkhisui).

nic(tapukoko).
nic(ironbundle).
nic(gliscor).

bird(greattusk).
bird(latiosmega).
bird(ragingbolt).

pat(ironvaliant).
pat(irontreads).
pat(rotomwash).

justin(charizardmegay).
justin(terapagos).

zack(dianciemega).
zack(ogerponhearthflame).

alex(meowscarada).
alex(nidoking).

mason(landorustherian).
mason(melmetal).

kirk(gholdengo).
kirk(tapulele).

kevin(toxapex).
kevin(tinglu).

andrew(scizormega).
andrew(landorustherian).

morry(mawilemega).
morry(walkingwake).

s6_available(Mon) :- \+ s6_taken(Mon).

s6_taken(Mon) :- george(Mon); nic(Mon); bird(Mon); pat(Mon); justin(Mon); zack(Mon); alex(Mon);
  mason(Mon); kirk(Mon); kevin(Mon); andrew(Mon); morry(Mon).

