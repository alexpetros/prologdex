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
zack(latias).

alex(meowscarada).
alex(nidoking).
alex(silvally).
alex(latios).

mason(landorustherian).
mason(melmetal).
mason(slowking).
mason(pidgeotmega).

kirk(gholdengo).
kirk(tapulele).
kirk(kingambit).
kirk(lopunnymega).

kevin(toxapex).
kevin(tinglu).
kevin(blacephalon).
kevin(garganacl).

andrew(scizormega).
andrew(landorustherian).
andrew(kyurem).
andrew(slowkinggalar).

morry(mawilemega).
morry(walkingwake).
morry(zapdos).
morry(ursaluna).

s6_available(Mon) :- \+ s6_taken(Mon).

s6_taken(Mon) :- george(Mon); nic(Mon); bird(Mon); pat(Mon); justin(Mon); zack(Mon); alex(Mon);
  mason(Mon); kirk(Mon); kevin(Mon); andrew(Mon); morry(Mon).

