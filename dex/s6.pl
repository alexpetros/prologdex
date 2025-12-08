:- module('s6', [points/2, players/1, remaining_points/2, total_points/2,
                 team/2, draft_status/0, viable/1, undrafted/1, drafted/1,
                 george/1, nic/1, bird/1, pat/1, justin/1, zack/1,
                 alex/1, andrew/1, mason/1, morry/1, kirk/1, kevin/1]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(dif)).
:- use_module('dex.pl').


draft_status :- players(Names), maplist(print_remaining, Names), !.
% Obviously some 1-pt mons are viable, but this is a pretty good heuristic
viable(Mon) :- points(Mon, Points), Points > 1.

undrafted(Mon) :- viable(Mon), \+ drafted(Mon).
drafted(Mon) :- george(Mon); nic(Mon); bird(Mon); pat(Mon); justin(Mon); zack(Mon);
  alex(Mon); mason(Mon); kirk(Mon); kevin(Mon); andrew(Mon); morry(Mon).

print_remaining(Player) :-
  remaining_points(Player, Points),
  format("~a: ~d", [Player, Points]),
  nl.

remaining_points(Player, Points) :-
  total_points(Player, Total),
  Points is 90 - Total.

total_points(Player, Total) :-
  team(Player, Team),
  maplist(points, Team, Points),
  sum_list(Points, Total).

team(Player, Team) :-
  players(Players),
  member(Player, Players),
  findall(Mon, call(Player, Mon), Team).

players([george, nic, bird, pat, justin, zack, alex, mason, kirk, kevin, andrew, morry, alex1]).

george(garchomp).
george(tyranitar).
george(zoroarkhisui).
george(rotomheat).
george(ferrothorn).
george(noivern).
george(beedrillmega).

nic(tapukoko).
nic(ironbundle).
nic(gliscor).
nic(ironhands).
nic(slowbromega).
nic(corviknight).
nic(blissey).

bird(greattusk).
bird(latiosmega).
bird(ragingbolt).
bird(amoonguss).
bird(quagsire).
bird(heatran).
bird(ninetales).

pat(ironvaliant).
pat(irontreads).
pat(rotomwash).
pat(hydrapple).
pat(talonflame).
pat(banettemega).
pat(obstagoon).

justin(charizardmegay).
justin(terapagos).
justin(alomomola).
justin(gougingfire).
justin(scizor).
justin(regieleki).
justin(runerigus).
justin(screamtail).

zack(dianciemega).
zack(ogerponhearthflame).
zack(latias).
zack(ursalunabloodmoon).
zack(araquanid).
zack(forretress).
zack(bisharp).
zack(annihilape).

alex(meowscarada).
alex(nidoking).
alex(silvally).
alex(latios).
alex(ribombee).
alex(tornadus).
alex(politoed).
alex(archaludon).

alex1(meowscarada).
alex1(weezinggalar).
alex1(silvally).
alex1(archaludon).
alex1(politoed).
alex1(swampertmega).
alex1(ribombee).
alex1(latios).
alex1(beartic).

mason(landorustherian).
mason(melmetal).
mason(slowking).
mason(pidgeotmega).
mason(infernape).
mason(basculegion).
mason(comfey).
mason(dragalge).

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

andrew(scizormega).
andrew(landorus).
andrew(kyurem).
andrew(slowkinggalar).
andrew(taurospaldeaaqua).
andrew(grimmsnarl).
andrew(salazzle).
andrew(togetic).

morry(mawilemega).
morry(walkingwake).
morry(zapdos).
morry(ursaluna).
morry(lokix).
morry(alakazam).
morry(skarmory).
morry(froslass).

points(Mon, Points) :- natdexdraft(Mon, Points).
points(Mon, 1) :- \+ natdexdraft(Mon, _).

% Nat Dex Draft Board
natdexdraft('greattusk', 19).
natdexdraft('ironvaliant', 19).
natdexdraft('gallademega', 19).

natdexdraft('deoxysspeed', 18).
natdexdraft('garchomp', 18).
natdexdraft('kyurem', 18).
natdexdraft('landorustherian', 18).
natdexdraft('dianciemega', 18).
natdexdraft('mawilemega', 18).
natdexdraft('scizormega', 18).
natdexdraft('meowscarada', 18).
natdexdraft('tapukoko', 18).

natdexdraft('darkrai', 17).
natdexdraft('gougingfire', 17).
natdexdraft('greninja', 17).
natdexdraft('ironbundle', 17).
natdexdraft('latiosmega', 17).
natdexdraft('medichammega', 17).
natdexdraft('palafin', 17).
natdexdraft('tornadustherian', 17).
natdexdraft('zeraora', 17).

natdexdraft('slowkinggalar', 16).
natdexdraft('gholdengo', 16).
natdexdraft('irontreads', 16).
natdexdraft('kartana', 16).
natdexdraft('landorus', 16).
natdexdraft('mew', 16).
natdexdraft('necrozma', 16).
natdexdraft('roaringmoon', 16).
natdexdraft('sneasler', 16).
natdexdraft('terapagos', 16).
natdexdraft('tinglu', 16).
natdexdraft('zapdos', 16).

natdexdraft('baxcalibur', 15).
natdexdraft('chiyu', 15).
natdexdraft('dragonite', 15).
natdexdraft('enamorus', 15).
natdexdraft('samurotthisui', 15).
natdexdraft('latios', 15).
natdexdraft('aerodactylmega', 15).
natdexdraft('charizardmegax', 15).
natdexdraft('garchompmega', 15).
natdexdraft('lopunnymega', 15).
natdexdraft('melmetal', 15).
natdexdraft('ogerponwellspring', 15).
natdexdraft('slowbro', 15).
natdexdraft('tapufini', 15).
natdexdraft('urshifusinglestrike', 15).
natdexdraft('victini', 15).

natdexdraft('aegislash', 14).
natdexdraft('annihilape', 14).
natdexdraft('celesteela', 14).
natdexdraft('cinderace', 14).
natdexdraft('excadrill', 14).
natdexdraft('gliscor', 14).
natdexdraft('hydreigon', 14).
natdexdraft('ironhands', 14).
natdexdraft('ironmoth', 14).
natdexdraft('gardevoirmega', 14).
natdexdraft('ragingbolt', 14).
natdexdraft('rillaboom', 14).
natdexdraft('rotomwash', 14).
natdexdraft('scizor', 14).
natdexdraft('tapulele', 14).
natdexdraft('ursalunabloodmoon', 14).
natdexdraft('urshifurapidstrike', 14).
natdexdraft('walkingwake', 14).
natdexdraft('weavile', 14).

natdexdraft('clefable', 13).
natdexdraft('dracovish', 13).
natdexdraft('ferrothorn', 13).
natdexdraft('glimmora', 13).
natdexdraft('heatran', 13).
natdexdraft('jirachi', 13).
natdexdraft('keldeo', 13).
natdexdraft('kingambit', 13).
natdexdraft('latias', 13).
natdexdraft('charizardmegay', 13).
natdexdraft('nidoking', 13).
natdexdraft('nidoqueen', 13).
natdexdraft('quaquaval', 13).
natdexdraft('zarude', 13).
natdexdraft('zygarde', 13).

natdexdraft('archaludon', 12).
natdexdraft('blacephalon', 12).
natdexdraft('blaziken', 12).
natdexdraft('buzzwole', 12).
natdexdraft('darmanitangalar', 12).
natdexdraft('hawlucha', 12).
natdexdraft('ironboulder', 12).
natdexdraft('krookodile', 12).
natdexdraft('manaphy', 12).
natdexdraft('heracrossmega', 12).
natdexdraft('tyranitarmega', 12).
natdexdraft('metagross', 12).
natdexdraft('nihilego', 12).
natdexdraft('ogerponcornerstone', 12).
natdexdraft('pecharunt', 12).
natdexdraft('primarina', 12).
natdexdraft('thundurus', 12).
natdexdraft('thundurustherian', 12).
natdexdraft('togekiss', 12).
natdexdraft('tyranitar', 12).
natdexdraft('ursaluna', 12).

natdexdraft('azelf', 11).
natdexdraft('azumarill', 11).
natdexdraft('ceruledge', 11).
natdexdraft('gothitelle', 11).
natdexdraft('hatterene', 11).
natdexdraft('arcaninehisui', 11).
natdexdraft('lilliganthisui', 11).
natdexdraft('ironcrown', 11).
natdexdraft('klefki', 11).
natdexdraft('altariamega', 11).
natdexdraft('beedrillmega', 11).
natdexdraft('gyaradosmega', 11).
natdexdraft('pidgeotmega', 11).
natdexdraft('swampertmega', 11).
natdexdraft('venusaurmega', 11).
natdexdraft('ogerpon', 11).
natdexdraft('ogerponhearthflame', 11).
natdexdraft('pelipper', 11).
natdexdraft('sandyshocks', 11).
natdexdraft('scolipede', 11).
natdexdraft('screamtail', 11).
natdexdraft('slowking', 11).
natdexdraft('torkoal', 11).
natdexdraft('uxie', 11).

natdexdraft('blastoise', 10).
natdexdraft('cobalion', 10).
natdexdraft('corviknight', 10).
natdexdraft('cresselia', 10).
natdexdraft('crobat', 10).
natdexdraft('deoxysdefense', 10).
natdexdraft('donphan', 10).
natdexdraft('empoleon', 10).
natdexdraft('moltresgalar', 10).
natdexdraft('weezinggalar', 10).
natdexdraft('lycanrocdusk', 10).
natdexdraft('sableyemega', 10).
natdexdraft('sharpedomega', 10).
natdexdraft('slowbromega', 10).
natdexdraft('mienshao', 10).
natdexdraft('raikou', 10).
natdexdraft('roserade', 10).
natdexdraft('salamence', 10).
natdexdraft('silvally', 10).
natdexdraft('skarmory', 10).
natdexdraft('swampert', 10).
natdexdraft('tapubulu', 10).
natdexdraft('terrakion', 10).
natdexdraft('tinkaton', 10).
natdexdraft('volcarona', 10).

natdexdraft('alolanmuk', 9).
natdexdraft('barraskewda', 9).
natdexdraft('basculegion', 9).
natdexdraft('bronzong', 9).
natdexdraft('diggersby', 9).
natdexdraft('dugtrio', 9).
natdexdraft('entei', 9).
natdexdraft('zapdosgalar', 9).
natdexdraft('gardevoir', 9).
natdexdraft('gengar', 9).
natdexdraft('goodrahisui', 9).
natdexdraft('zoroarkhisui', 9).
natdexdraft('hoopaunbound', 9).
natdexdraft('hydrapple', 9).
natdexdraft('infernape', 9).
natdexdraft('ironjugulis', 9).
natdexdraft('kommoo', 9).
natdexdraft('mamoswine', 9).
natdexdraft('mandibuzz', 9).
natdexdraft('mesprit', 9).
natdexdraft('moltres', 9).
natdexdraft('overqwil', 9).
natdexdraft('porygon2', 9).
natdexdraft('rotomheat', 9).
natdexdraft('slitherwing', 9).
natdexdraft('starmie', 9).
natdexdraft('talonflame', 9).
natdexdraft('tentacruel', 9).
natdexdraft('toxapex', 9).
natdexdraft('volcanion', 9).

natdexdraft('alakazam', 8).
natdexdraft('alolanninetales', 8).
natdexdraft('alomomola', 8).
natdexdraft('arcanine', 8).
natdexdraft('clodsire', 8).
natdexdraft('conkeldurr', 8).
natdexdraft('cyclizar', 8).
natdexdraft('drapion', 8).
natdexdraft('garganacl', 8).
natdexdraft('haxorus', 8).
natdexdraft('aggronmega', 8).
natdexdraft('pinsirmega', 8).
natdexdraft('milotic', 8).
natdexdraft('ninetales', 8).
natdexdraft('noivern', 8).
natdexdraft('pawmot', 8).
natdexdraft('revavroom', 8).
natdexdraft('seismitoad', 8).
natdexdraft('shaymin', 8).
natdexdraft('sinistcha', 8).
natdexdraft('skeledirge', 8).
natdexdraft('suicune', 8).
natdexdraft('sylveon', 8).
natdexdraft('tangrowth', 8).

natdexdraft('amoonguss', 7).
natdexdraft('armarouge', 7).
natdexdraft('breloom', 7).
natdexdraft('comfey', 7).
natdexdraft('diancie', 7).
natdexdraft('dondozo', 7).
natdexdraft('dracozolt', 7).
natdexdraft('dragalge', 7).
natdexdraft('dudunsparce', 7).
natdexdraft('enamorustherian', 7).
natdexdraft('fezandipiti', 7).
natdexdraft('florges', 7).
natdexdraft('gallade', 7).
natdexdraft('gigalith', 7).
natdexdraft('goodra', 7).
natdexdraft('grimmsnarl', 7).
natdexdraft('gyarados', 7).
natdexdraft('heracross', 7).
natdexdraft('hippowdon', 7).
natdexdraft('braviaryhisui', 7).
natdexdraft('kilowattrel', 7).
natdexdraft('steelixmega', 7).
natdexdraft('meloetta', 7).
natdexdraft('mimikyu', 7).
natdexdraft('obstagoon', 7).
natdexdraft('okidogi', 7).
natdexdraft('taurospaldeaaqua', 7).
natdexdraft('regieleki', 7).
natdexdraft('registeel', 7).
natdexdraft('reuniclus', 7).
natdexdraft('rhyperior', 7).
natdexdraft('rotommow', 7).
natdexdraft('serperior', 7).
natdexdraft('snorlax', 7).
natdexdraft('steelix', 7).
natdexdraft('toxtricity', 7).
natdexdraft('tsareena', 7).
natdexdraft('vaporeon', 7).
natdexdraft('venusaur', 7).
natdexdraft('whimsicott', 7).
natdexdraft('xurkitree', 7).

natdexdraft('abomasnow', 6).
natdexdraft('aerodactyl', 6).
natdexdraft('araquanid', 6).
natdexdraft('arctozolt', 6).
natdexdraft('bellibolt', 6).
natdexdraft('bewear', 6).
natdexdraft('blissey', 6).
natdexdraft('braviary', 6).
natdexdraft('celebi', 6).
natdexdraft('cloyster', 6).
natdexdraft('darmanitan', 6).
natdexdraft('espathra', 6).
natdexdraft('forretress', 6).
natdexdraft('slowbrogalar', 6).
natdexdraft('gastrodon', 6).
natdexdraft('qwilfishhisui', 6).
natdexdraft('incineroar', 6).
natdexdraft('ironleaves', 6).
natdexdraft('jellicent', 6).
natdexdraft('kingdra', 6).
natdexdraft('lokix', 6).
natdexdraft('lucario', 6).
natdexdraft('magnezone', 6).
natdexdraft('sceptilemega', 6).
natdexdraft('miltank', 6).
natdexdraft('mismagius', 6).
natdexdraft('munkidori', 6).
natdexdraft('taurospaldeablaze', 6).
natdexdraft('porygonz', 6).
natdexdraft('qwilfish', 6).
natdexdraft('ribombee', 6).
natdexdraft('rotomfrost', 6).
natdexdraft('salazzle', 6).
natdexdraft('scrafty', 6).
natdexdraft('stakataka', 6).
natdexdraft('swellow', 6).
natdexdraft('tornadus', 6).
natdexdraft('umbreon', 6).
natdexdraft('zygarde10', 6).

natdexdraft('bisharp', 5).
natdexdraft('brambleghast', 5).
natdexdraft('cetitan', 5).
natdexdraft('chandelure', 5).
natdexdraft('chesnaught', 5).
natdexdraft('cofagrigus', 5).
natdexdraft('decidueye', 5).
natdexdraft('delphox', 5).
natdexdraft('dhelmise', 5).
natdexdraft('eelektross', 5).
natdexdraft('feraligatr', 5).
natdexdraft('flygon', 5).
natdexdraft('gligar', 5).
natdexdraft('hariyama', 5).
natdexdraft('electrodehisui', 5).
natdexdraft('hitmonlee', 5).
natdexdraft('inteleon', 5).
natdexdraft('kleavor', 5).
natdexdraft('lanturn', 5).
natdexdraft('lickilicky', 5).
natdexdraft('mudsdale', 5).
natdexdraft('muk', 5).
natdexdraft('piloswine', 5).
natdexdraft('politoed', 5).
natdexdraft('quagsire', 5).
natdexdraft('regidrago', 5).
natdexdraft('rotom', 5).
natdexdraft('sableye', 5).
natdexdraft('sharpedo', 5).
natdexdraft('slurpuff', 5).
natdexdraft('tyrantrum', 5).
natdexdraft('ursaring', 5).
natdexdraft('venomoth', 5).
natdexdraft('vikavolt', 5).
natdexdraft('vileplume', 5).
natdexdraft('weezing', 5).
natdexdraft('yanmega', 5).
natdexdraft('zoroark', 5).

natdexdraft('accelgor', 4).
natdexdraft('alolanmarowak', 4).
natdexdraft('alolanpersian', 4).
natdexdraft('alolanraichu', 4).
natdexdraft('arboliva', 4).
natdexdraft('aromatisse', 4).
natdexdraft('brutebonnet', 4).
natdexdraft('chansey', 4).
natdexdraft('cinccino', 4).
natdexdraft('crawdaunt', 4).
natdexdraft('drednaw', 4).
natdexdraft('drifblim', 4).
natdexdraft('druddigon', 4).
natdexdraft('emboar', 4).
natdexdraft('escavalier', 4).
natdexdraft('espeon', 4).
natdexdraft('floatzel', 4).
natdexdraft('froslass', 4).
natdexdraft('galvantula', 4).
natdexdraft('garbodor', 4).
natdexdraft('glastrier', 4).
natdexdraft('golurk', 4).
natdexdraft('gothorita', 4).
natdexdraft('gourgeist', 4).
natdexdraft('granbull', 4).
natdexdraft('heliolisk', 4).
natdexdraft('hitmontop', 4).
natdexdraft('ironthorns', 4).
natdexdraft('jolteon', 4).
natdexdraft('machamp', 4).
natdexdraft('maushold', 4).
natdexdraft('medicham', 4).
natdexdraft('houndoommega', 4).
natdexdraft('oricorio', 4).
natdexdraft('palossand', 4).
natdexdraft('pangoro', 4).
natdexdraft('passimian', 4).
natdexdraft('polteageist', 4).
natdexdraft('rotomfan', 4).
natdexdraft('sigilyph', 4).
natdexdraft('skuntank', 4).
natdexdraft('staraptor', 4).
natdexdraft('stunfisk', 4).
natdexdraft('tangela', 4).
natdexdraft('toedscruel', 4).
natdexdraft('torterra', 4).
natdexdraft('vanilluxe', 4).
natdexdraft('wochien', 4).
natdexdraft('xatu', 4).

natdexdraft('ambipom', 3).
natdexdraft('appletun', 3).
natdexdraft('archeops', 3).
natdexdraft('arctovish', 3).
natdexdraft('audino', 3).
natdexdraft('bouffalant', 3).
natdexdraft('charizard', 3).
natdexdraft('claydol', 3).
natdexdraft('copperajah', 3).
natdexdraft('crustle', 3).
natdexdraft('cryogonal', 3).
natdexdraft('ditto', 3).
natdexdraft('doublade', 3).
natdexdraft('drampa', 3).
natdexdraft('duraludon', 3).
natdexdraft('durant', 3).
natdexdraft('dusknoir', 3).
natdexdraft('eldegoss', 3).
natdexdraft('exploud', 3).
natdexdraft('flamigo', 3).
natdexdraft('articunogalar', 3).
natdexdraft('golisopod', 3).
natdexdraft('grafaiai', 3).
natdexdraft('guzzlord', 3).
natdexdraft('decidueyehisui', 3).
natdexdraft('typhlosionhisui', 3).
natdexdraft('hitmonchan', 3).
natdexdraft('hoopa', 3).
natdexdraft('houndstone', 3).
natdexdraft('indeedeefemale', 3).
natdexdraft('ludicolo', 3).
natdexdraft('lycanrocmidday', 3).
natdexdraft('magneton', 3).
natdexdraft('abomasnowmega', 3).
natdexdraft('banettemega', 3).
natdexdraft('glaliemega', 3).
natdexdraft('manectricmega', 3).
natdexdraft('pincurchin', 3).
natdexdraft('raichu', 3).
natdexdraft('roselia', 3).
natdexdraft('runerigus', 3).
natdexdraft('sandaconda', 3).
natdexdraft('sceptile', 3).
natdexdraft('scyther', 3).
natdexdraft('smeargle', 3).
natdexdraft('spiritomb', 3).
natdexdraft('tatsugiri', 3).
natdexdraft('tauros', 3).
natdexdraft('typenull', 3).
natdexdraft('victreebel', 3).
natdexdraft('virizion', 3).

natdexdraft('aggron', 2).
natdexdraft('alolanexeggutor', 2).
natdexdraft('alolanraticate', 2).
natdexdraft('alolansandslash', 2).
natdexdraft('altaria', 2).
natdexdraft('armaldo', 2).
natdexdraft('articuno', 2).
natdexdraft('avalugg', 2).
natdexdraft('barbaracle', 2).
natdexdraft('boltund', 2).
natdexdraft('bruxish', 2).
natdexdraft('carracosta', 2).
natdexdraft('centiskorch', 2).
natdexdraft('coalossal', 2).
natdexdraft('cursola', 2).
natdexdraft('dachsbun', 2).
natdexdraft('dunsparce', 2).
natdexdraft('dusclops', 2).
natdexdraft('electrode', 2).
natdexdraft('farigiraf', 2).
natdexdraft('flapple', 2).
natdexdraft('flareon', 2).
natdexdraft('frosmoth', 2).
natdexdraft('corsolagalar', 2).
natdexdraft('golbat', 2).
natdexdraft('gorebyss', 2).
natdexdraft('gurdurr', 2).
natdexdraft('haunter', 2).
natdexdraft('sneaselhisui', 2).
natdexdraft('houndoom', 2).
natdexdraft('huntail', 2).
natdexdraft('illumise', 2).
natdexdraft('kabutops', 2).
natdexdraft('kangaskhan', 2).
natdexdraft('kecleon', 2).
natdexdraft('kingler', 2).
natdexdraft('klawf', 2).
natdexdraft('klinklang', 2).
natdexdraft('lapras', 2).
natdexdraft('leafeon', 2).
natdexdraft('leavanny', 2).
natdexdraft('linoone', 2).
natdexdraft('lurantis', 2).
natdexdraft('mabosstiff', 2).
natdexdraft('magmortar', 2).
natdexdraft('manectric', 2).
natdexdraft('mantine', 2).
natdexdraft('masquerain', 2).
natdexdraft('absolmega', 2).
natdexdraft('ampharosmega', 2).
natdexdraft('audinomega', 2).
natdexdraft('cameruptmega', 2).
natdexdraft('meganium', 2).
natdexdraft('meowstic', 2).
natdexdraft('minior', 2).
natdexdraft('misdreavus', 2).
natdexdraft('omastar', 2).
natdexdraft('orbeetle', 2).
natdexdraft('orthworm', 2).
natdexdraft('taurospaldeacombat', 2).
natdexdraft('poliwrath', 2).
natdexdraft('primeape', 2).
natdexdraft('regice', 2).
natdexdraft('regigigas', 2).
natdexdraft('regirock', 2).
natdexdraft('rhydon', 2).
natdexdraft('sandslash', 2).
natdexdraft('sawk', 2).
natdexdraft('shiftry', 2).
natdexdraft('shuckle', 2).
natdexdraft('simipour', 2).
natdexdraft('simisage', 2).
natdexdraft('simisear', 2).
natdexdraft('sirfetch\'d', 2).
natdexdraft('sneasel', 2).
natdexdraft('stoutland', 2).
natdexdraft('throh', 2).
natdexdraft('thwackey', 2).
natdexdraft('togedemaru', 2).
natdexdraft('toxicroak', 2).
natdexdraft('trevenant', 2).
natdexdraft('turtonator', 2).
natdexdraft('typhlosion', 2).
natdexdraft('veluza', 2).
natdexdraft('wishiwashi', 2).
natdexdraft('zangoose', 2).
