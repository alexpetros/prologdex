:- module('s6', [points/2, print_remaining/1, remaining_points/2, total_points/2,
                 players/1, team/2, s6_status/0, s6_status/1, s6_available/1,
                 s6_taken/1, george/1, nic/1, bird/1, pat/1, justin/1, zack/1,
                 alex/1, andrew/1, mason/1]).

:- use_module(library(format)).
:- use_module(library(lists)).

s6_available(Mon) :- \+ s6_taken(Mon).
s6_taken(Mon) :- george(Mon); nic(Mon); bird(Mon); pat(Mon); justin(Mon); zack(Mon);
  alex(Mon); mason(Mon); kirk(Mon); kevin(Mon); andrew(Mon); morry(Mon).

players([george, nic, bird, pat, justin, zack, alex, mason, kirk, kevin, andrew, morry]).

s6_status([]).
s6_status([Head|Tail]) :- print_remaining(Head), s6_status(Tail).
s6_status :- players(Names), s6_status(Names).

print_remaining(Name) :-
  remaining_points(Name, Points),
  format("~a: ~d", [Name, Points]),
  nl.

% print_remaining(Name) :-
%   format("~a: d", [Name]),
%   nl.

remaining_points(Name, Points) :-
  total_points(Name, Total),
  Points is 90 - Total.

total_points(Name, Total) :-
  team(Name, Team),
  maplist(points, Team, Points),
  sum_list(Points, Total).

team(Name, Team) :-
  findall(Mon, call(Name, Mon), Team).

george(garchomp).
george(tyranitar).
george(zoroarkhisui).
george(rotomheat).
george(ferrothorn).

nic(tapukoko).
nic(ironbundle).
nic(gliscor).
nic(ironhands).
nic(slowbromega).

bird(greattusk).
bird(latiosmega).
bird(ragingbolt).
bird(amoonguss).
bird(quagsire).

pat(ironvaliant).
pat(irontreads).
pat(rotomwash).
pat(hydrapple).
pat(talonflame).

justin(charizardmegay).
justin(terapagos).
justin(alomomola).
justin(gougingfire).
justin(scizor).

zack(dianciemega).
zack(ogerponhearthflame).
zack(latias).
zack(ursalunabloodmoon).
zack(araquanid).

alex(meowscarada).
alex(nidoking).
alex(silvally).
alex(latios).
alex(ribombee).

mason(landorustherian).
mason(melmetal).
mason(slowking).
mason(pidgeotmega).
mason(infernape).

kirk(gholdengo).
kirk(tapulele).
kirk(kingambit).
kirk(lopunnymega).

kevin(toxapex).
kevin(tinglu).
kevin(blacephalon).
kevin(garganacl).

andrew(scizormega).
andrew(landorusincarnate).
andrew(kyurem).
andrew(slowkinggalar).

morry(mawilemega).
morry(walkingwake).
morry(zapdos).
morry(ursaluna).


% Nat Dex Draft Board
points('greattusk', 19).
points('ironvaliant', 19).
points('gallademega', 19).

points('deoxysspeed', 18).
points('garchomp', 18).
points('kyurem', 18).
points('landorustherian', 18).
points('dianciemega', 18).
points('mawilemega', 18).
points('scizormega', 18).
points('meowscarada', 18).
points('tapukoko', 18).

points('darkrai', 17).
points('gougingfire', 17).
points('greninja', 17).
points('ironbundle', 17).
points('latiosmega', 17).
points('medichammega', 17).
points('palafin', 17).
points('tornadustherian', 17).
points('zeraora', 17).

points('slowkinggalar', 16).
points('gholdengo', 16).
points('irontreads', 16).
points('kartana', 16).
points('landorusincarnate', 16).
points('mew', 16).
points('necrozma', 16).
points('roaringmoon', 16).
points('sneasler', 16).
points('terapagos', 16).
points('tinglu', 16).
points('zapdos', 16).

points('baxcalibur', 15).
points('chiyu', 15).
points('dragonite', 15).
points('enamorusincarnate', 15).
points('samurotthisui', 15).
points('latios', 15).
points('aerodactylmega', 15).
points('charizardmegax', 15).
points('garchompmega', 15).
points('lopunnymega', 15).
points('melmetal', 15).
points('ogerponwellspring', 15).
points('slowbro', 15).
points('tapufini', 15).
points('urshifusinglestrike', 15).
points('victini', 15).

points('aegislash', 14).
points('annihilape', 14).
points('celesteela', 14).
points('cinderace', 14).
points('excadrill', 14).
points('gliscor', 14).
points('hydreigon', 14).
points('ironhands', 14).
points('ironmoth', 14).
points('gardevoirmega', 14).
points('ragingbolt', 14).
points('rillaboom', 14).
points('rotomwash', 14).
points('scizor', 14).
points('tapulele', 14).
points('ursalunabloodmoon', 14).
points('urshifurapidstrike', 14).
points('walkingwake', 14).
points('weavile', 14).

points('clefable', 13).
points('dracovish', 13).
points('ferrothorn', 13).
points('glimmora', 13).
points('heatran', 13).
points('jirachi', 13).
points('keldeo', 13).
points('kingambit', 13).
points('latias', 13).
points('charizardmegay', 13).
points('nidoking', 13).
points('nidoqueen', 13).
points('quaquaval', 13).
points('zarude', 13).
points('zygarde', 13).

points('archaludon', 12).
points('blacephalon', 12).
points('blaziken', 12).
points('buzzwole', 12).
points('darmanitangalar', 12).
points('hawlucha', 12).
points('ironboulder', 12).
points('krookodile', 12).
points('manaphy', 12).
points('heracrossmega', 12).
points('tyranitarmega', 12).
points('metagross', 12).
points('nihilego', 12).
points('ogerponcornerstone', 12).
points('pecharunt', 12).
points('primarina', 12).
points('thundurusincarnate', 12).
points('thundurustherian', 12).
points('togekiss', 12).
points('tyranitar', 12).
points('ursaluna', 12).

points('azelf', 11).
points('azumarill', 11).
points('ceruledge', 11).
points('gothitelle', 11).
points('hatterene', 11).
points('arcaninehisui', 11).
points('lilliganthisui', 11).
points('ironcrown', 11).
points('klefki', 11).
points('altariamega', 11).
points('beedrillmega', 11).
points('gyaradosmega', 11).
points('pidgeotmega', 11).
points('swampertmega', 11).
points('venusaurmega', 11).
points('ogerpon', 11).
points('ogerponhearthflame', 11).
points('pelipper', 11).
points('sandyshocks', 11).
points('scolipede', 11).
points('screamtail', 11).
points('slowking', 11).
points('torkoal', 11).
points('uxie', 11).

points('blastoise', 10).
points('cobalion', 10).
points('corviknight', 10).
points('cresselia', 10).
points('crobat', 10).
points('deoxysdefense', 10).
points('donphan', 10).
points('empoleon', 10).
points('moltresgalar', 10).
points('weezinggalar', 10).
points('lycanrocdusk', 10).
points('sableyemega', 10).
points('sharpedomega', 10).
points('slowbromega', 10).
points('mienshao', 10).
points('raikou', 10).
points('roserade', 10).
points('salamence', 10).
points('silvally', 10).
points('skarmory', 10).
points('swampert', 10).
points('tapubulu', 10).
points('terrakion', 10).
points('tinkaton', 10).
points('volcarona', 10).

points('alolanmuk', 9).
points('barraskewda', 9).
points('basculegion', 9).
points('bronzong', 9).
points('diggersby', 9).
points('dugtrio', 9).
points('entei', 9).
points('zapdosgalar', 9).
points('gardevoir', 9).
points('gengar', 9).
points('goodrahisui', 9).
points('zoroarkhisui', 9).
points('hoopaunbound', 9).
points('hydrapple', 9).
points('infernape', 9).
points('ironjugulis', 9).
points('kommoo', 9).
points('mamoswine', 9).
points('mandibuzz', 9).
points('mesprit', 9).
points('moltres', 9).
points('overqwil', 9).
points('porygon2', 9).
points('rotomheat', 9).
points('slitherwing', 9).
points('starmie', 9).
points('talonflame', 9).
points('tentacruel', 9).
points('toxapex', 9).
points('volcanion', 9).

points('alakazam', 8).
points('alolanninetales', 8).
points('alomomola', 8).
points('arcanine', 8).
points('clodsire', 8).
points('conkeldurr', 8).
points('cyclizar', 8).
points('drapion', 8).
points('garganacl', 8).
points('haxorus', 8).
points('aggronmega', 8).
points('pinsirmega', 8).
points('milotic', 8).
points('ninetales', 8).
points('noivern', 8).
points('pawmot', 8).
points('revavroom', 8).
points('seismitoad', 8).
points('shaymin', 8).
points('sinistcha', 8).
points('skeledirge', 8).
points('suicune', 8).
points('sylveon', 8).
points('tangrowth', 8).

points('amoonguss', 7).
points('armarouge', 7).
points('breloom', 7).
points('comfey', 7).
points('diancie', 7).
points('dondozo', 7).
points('dracozolt', 7).
points('dragalge', 7).
points('dudunsparce', 7).
points('enamorustherian', 7).
points('fezandipiti', 7).
points('florges', 7).
points('gallade', 7).
points('gigalith', 7).
points('goodra', 7).
points('grimmsnarl', 7).
points('gyarados', 7).
points('heracross', 7).
points('hippowdon', 7).
points('braviaryhisui', 7).
points('kilowattrel', 7).
points('steelixmega', 7).
points('meloetta', 7).
points('mimikyu', 7).
points('obstagoon', 7).
points('okidogi', 7).
points('paldeantaurosaqua', 7).
points('regieleki', 7).
points('registeel', 7).
points('reuniclus', 7).
points('rhyperior', 7).
points('rotommow', 7).
points('serperior', 7).
points('snorlax', 7).
points('steelix', 7).
points('toxtricity', 7).
points('tsareena', 7).
points('vaporeon', 7).
points('venusaur', 7).
points('whimsicott', 7).
points('xurkitree', 7).

points('abomasnow', 6).
points('aerodactyl', 6).
points('araquanid', 6).
points('arctozolt', 6).
points('bellibolt', 6).
points('bewear', 6).
points('blissey', 6).
points('braviary', 6).
points('celebi', 6).
points('cloyster', 6).
points('darmanitan', 6).
points('espathra', 6).
points('forretress', 6).
points('slowbrogalar', 6).
points('gastrodon', 6).
points('qwilfishhisui', 6).
points('incineroar', 6).
points('ironleaves', 6).
points('jellicent', 6).
points('kingdra', 6).
points('lokix', 6).
points('lucario', 6).
points('magnezone', 6).
points('sceptilemega', 6).
points('miltank', 6).
points('mismagius', 6).
points('munkidori', 6).
points('paldeantaurosblaze', 6).
points('porygonz', 6).
points('qwilfish', 6).
points('ribombee', 6).
points('rotomfrost', 6).
points('salazzle', 6).
points('scrafty', 6).
points('stakataka', 6).
points('swellow', 6).
points('tornadusincarnate', 6).
points('umbreon', 6).
points('zygarde10', 6).

points('bisharp', 5).
points('brambleghast', 5).
points('cetitan', 5).
points('chandelure', 5).
points('chesnaught', 5).
points('cofagrigus', 5).
points('decidueye', 5).
points('delphox', 5).
points('dhelmise', 5).
points('eelektross', 5).
points('feraligatr', 5).
points('flygon', 5).
points('gligar', 5).
points('hariyama', 5).
points('electrodehisui', 5).
points('hitmonlee', 5).
points('inteleon', 5).
points('kleavor', 5).
points('lanturn', 5).
points('lickilicky', 5).
points('mudsdale', 5).
points('muk', 5).
points('piloswine', 5).
points('politoed', 5).
points('quagsire', 5).
points('regidrago', 5).
points('rotom', 5).
points('sableye', 5).
points('sharpedo', 5).
points('slurpuff', 5).
points('tyrantrum', 5).
points('ursaring', 5).
points('venomoth', 5).
points('vikavolt', 5).
points('vileplume', 5).
points('weezing', 5).
points('yanmega', 5).
points('zoroark', 5).

points('accelgor', 4).
points('alolanmarowak', 4).
points('alolanpersian', 4).
points('alolanraichu', 4).
points('arboliva', 4).
points('aromatisse', 4).
points('brutebonnet', 4).
points('chansey', 4).
points('cinccino', 4).
points('crawdaunt', 4).
points('drednaw', 4).
points('drifblim', 4).
points('druddigon', 4).
points('emboar', 4).
points('escavalier', 4).
points('espeon', 4).
points('floatzel', 4).
points('froslass', 4).
points('galvantula', 4).
points('garbodor', 4).
points('glastrier', 4).
points('golurk', 4).
points('gothorita', 4).
points('gourgeist', 4).
points('granbull', 4).
points('heliolisk', 4).
points('hitmontop', 4).
points('ironthorns', 4).
points('jolteon', 4).
points('machamp', 4).
points('maushold', 4).
points('medicham', 4).
points('houndoommega', 4).
points('oricorio', 4).
points('palossand', 4).
points('pangoro', 4).
points('passimian', 4).
points('polteageist', 4).
points('rotomfan', 4).
points('sigilyph', 4).
points('skuntank', 4).
points('staraptor', 4).
points('stunfisk', 4).
points('tangela', 4).
points('toedscruel', 4).
points('torterra', 4).
points('vanilluxe', 4).
points('wochien', 4).
points('xatu', 4).

points('ambipom', 3).
points('appletun', 3).
points('archeops', 3).
points('arctovish', 3).
points('audino', 3).
points('bouffalant', 3).
points('charizard', 3).
points('claydol', 3).
points('copperajah', 3).
points('crustle', 3).
points('cryogonal', 3).
points('ditto', 3).
points('doublade', 3).
points('drampa', 3).
points('duraludon', 3).
points('durant', 3).
points('dusknoir', 3).
points('eldegoss', 3).
points('exploud', 3).
points('flamigo', 3).
points('articunogalar', 3).
points('golisopod', 3).
points('grafaiai', 3).
points('guzzlord', 3).
points('decidueyehisui', 3).
points('typhlosionhisui', 3).
points('hitmonchan', 3).
points('hoopa', 3).
points('houndstone', 3).
points('indeedeefemale', 3).
points('ludicolo', 3).
points('lycanrocmidday', 3).
points('magneton', 3).
points('abomasnowmega', 3).
points('banettemega', 3).
points('glaliemega', 3).
points('manectricmega', 3).
points('pincurchin', 3).
points('raichu', 3).
points('roselia', 3).
points('runerigus', 3).
points('sandaconda', 3).
points('sceptile', 3).
points('scyther', 3).
points('smeargle', 3).
points('spiritomb', 3).
points('tatsugiri', 3).
points('tauros', 3).
points('typenull', 3).
points('victreebel', 3).
points('virizion', 3).

points('aggron', 2).
points('alolanexeggutor', 2).
points('alolanraticate', 2).
points('alolansandslash', 2).
points('altaria', 2).
points('armaldo', 2).
points('articuno', 2).
points('avalugg', 2).
points('barbaracle', 2).
points('boltund', 2).
points('bruxish', 2).
points('carracosta', 2).
points('centiskorch', 2).
points('coalossal', 2).
points('cursola', 2).
points('dachsbun', 2).
points('dunsparce', 2).
points('dusclops', 2).
points('electrode', 2).
points('farigiraf', 2).
points('flapple', 2).
points('flareon', 2).
points('frosmoth', 2).
points('corsolagalar', 2).
points('golbat', 2).
points('gorebyss', 2).
points('gurdurr', 2).
points('haunter', 2).
points('sneaselhisui', 2).
points('houndoom', 2).
points('huntail', 2).
points('illumise', 2).
points('kabutops', 2).
points('kangaskhan', 2).
points('kecleon', 2).
points('kingler', 2).
points('klawf', 2).
points('klinklang', 2).
points('lapras', 2).
points('leafeon', 2).
points('leavanny', 2).
points('linoone', 2).
points('lurantis', 2).
points('mabosstiff', 2).
points('magmortar', 2).
points('manectric', 2).
points('mantine', 2).
points('masquerain', 2).
points('absolmega', 2).
points('ampharosmega', 2).
points('audinomega', 2).
points('cameruptmega', 2).
points('meganium', 2).
points('meowstic', 2).
points('minior', 2).
points('misdreavus', 2).
points('omastar', 2).
points('orbeetle', 2).
points('orthworm', 2).
points('paldeantauros', 2).
points('poliwrath', 2).
points('primeape', 2).
points('regice', 2).
points('regigigas', 2).
points('regirock', 2).
points('rhydon', 2).
points('sandslash', 2).
points('sawk', 2).
points('shiftry', 2).
points('shuckle', 2).
points('simipour', 2).
points('simisage', 2).
points('simisear', 2).
points('sirfetch\'d', 2).
points('sneasel', 2).
points('stoutland', 2).
points('throh', 2).
points('thwackey', 2).
points('togedemaru', 2).
points('toxicroak', 2).
points('trevenant', 2).
points('turtonator', 2).
points('typhlosion', 2).
points('veluza', 2).
points('wishiwashi', 2).
points('zangoose', 2).
