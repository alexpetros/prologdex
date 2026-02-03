% GENERATED FILE - do not modify directly
% see create-dex.js
:- module(dex, [pokemon/1, type/2, pokemon_ability/2, pokemon_ability_t/3, pokemon_hp/2, pokemon_atk/2, pokemon_def/2, pokemon_spa/2, pokemon_spd/2, pokemon_spe/2]).

:- use_module(library(reif)).

pokemon_ability(Mon, Ability) :- pokemon_ability_t(Mon, Ability, true).
pokemon_ability_t(Mon, Ability, T) :- pokemon_abilities(Mon, Abilities), memberd_t(Ability, Abilities, T).

pokemon('bulbasaur').
pokemon('ivysaur').
pokemon('venusaur').
pokemon('venusaurmega').
pokemon('venusaurgmax').
pokemon('charmander').
pokemon('charmeleon').
pokemon('charizard').
pokemon('charizardmegax').
pokemon('charizardmegay').
pokemon('charizardgmax').
pokemon('squirtle').
pokemon('wartortle').
pokemon('blastoise').
pokemon('blastoisemega').
pokemon('blastoisegmax').
pokemon('caterpie').
pokemon('metapod').
pokemon('butterfree').
pokemon('butterfreegmax').
pokemon('weedle').
pokemon('kakuna').
pokemon('beedrill').
pokemon('beedrillmega').
pokemon('pidgey').
pokemon('pidgeotto').
pokemon('pidgeot').
pokemon('pidgeotmega').
pokemon('rattata').
pokemon('rattataalola').
pokemon('raticate').
pokemon('raticatealola').
pokemon('raticatealolatotem').
pokemon('spearow').
pokemon('fearow').
pokemon('ekans').
pokemon('arbok').
pokemon('pikachu').
pokemon('pikachucosplay').
pokemon('pikachurockstar').
pokemon('pikachubelle').
pokemon('pikachupopstar').
pokemon('pikachuphd').
pokemon('pikachulibre').
pokemon('pikachuoriginal').
pokemon('pikachuhoenn').
pokemon('pikachusinnoh').
pokemon('pikachuunova').
pokemon('pikachukalos').
pokemon('pikachualola').
pokemon('pikachupartner').
pokemon('pikachustarter').
pokemon('pikachugmax').
pokemon('pikachuworld').
pokemon('raichu').
pokemon('raichualola').
pokemon('sandshrew').
pokemon('sandshrewalola').
pokemon('sandslash').
pokemon('sandslashalola').
pokemon('nidoranf').
pokemon('nidorina').
pokemon('nidoqueen').
pokemon('nidoranm').
pokemon('nidorino').
pokemon('nidoking').
pokemon('clefairy').
pokemon('clefable').
pokemon('clefablemega').
pokemon('vulpix').
pokemon('vulpixalola').
pokemon('ninetales').
pokemon('ninetalesalola').
pokemon('jigglypuff').
pokemon('wigglytuff').
pokemon('zubat').
pokemon('golbat').
pokemon('oddish').
pokemon('gloom').
pokemon('vileplume').
pokemon('paras').
pokemon('parasect').
pokemon('venonat').
pokemon('venomoth').
pokemon('diglett').
pokemon('diglettalola').
pokemon('dugtrio').
pokemon('dugtrioalola').
pokemon('meowth').
pokemon('meowthalola').
pokemon('meowthgalar').
pokemon('meowthgmax').
pokemon('persian').
pokemon('persianalola').
pokemon('psyduck').
pokemon('golduck').
pokemon('mankey').
pokemon('primeape').
pokemon('growlithe').
pokemon('growlithehisui').
pokemon('arcanine').
pokemon('arcaninehisui').
pokemon('poliwag').
pokemon('poliwhirl').
pokemon('poliwrath').
pokemon('abra').
pokemon('kadabra').
pokemon('alakazam').
pokemon('alakazammega').
pokemon('machop').
pokemon('machoke').
pokemon('machamp').
pokemon('machampgmax').
pokemon('bellsprout').
pokemon('weepinbell').
pokemon('victreebel').
pokemon('victreebelmega').
pokemon('tentacool').
pokemon('tentacruel').
pokemon('geodude').
pokemon('geodudealola').
pokemon('graveler').
pokemon('graveleralola').
pokemon('golem').
pokemon('golemalola').
pokemon('ponyta').
pokemon('ponytagalar').
pokemon('rapidash').
pokemon('rapidashgalar').
pokemon('slowpoke').
pokemon('slowpokegalar').
pokemon('slowbro').
pokemon('slowbromega').
pokemon('slowbrogalar').
pokemon('magnemite').
pokemon('magneton').
pokemon('farfetchd').
pokemon('farfetchdgalar').
pokemon('doduo').
pokemon('dodrio').
pokemon('seel').
pokemon('dewgong').
pokemon('grimer').
pokemon('grimeralola').
pokemon('muk').
pokemon('mukalola').
pokemon('shellder').
pokemon('cloyster').
pokemon('gastly').
pokemon('haunter').
pokemon('gengar').
pokemon('gengarmega').
pokemon('gengargmax').
pokemon('onix').
pokemon('drowzee').
pokemon('hypno').
pokemon('krabby').
pokemon('kingler').
pokemon('kinglergmax').
pokemon('voltorb').
pokemon('voltorbhisui').
pokemon('electrode').
pokemon('electrodehisui').
pokemon('exeggcute').
pokemon('exeggutor').
pokemon('exeggutoralola').
pokemon('cubone').
pokemon('marowak').
pokemon('marowakalola').
pokemon('marowakalolatotem').
pokemon('hitmonlee').
pokemon('hitmonchan').
pokemon('lickitung').
pokemon('koffing').
pokemon('weezing').
pokemon('weezinggalar').
pokemon('rhyhorn').
pokemon('rhydon').
pokemon('chansey').
pokemon('tangela').
pokemon('kangaskhan').
pokemon('kangaskhanmega').
pokemon('horsea').
pokemon('seadra').
pokemon('goldeen').
pokemon('seaking').
pokemon('staryu').
pokemon('starmie').
pokemon('starmiemega').
pokemon('mrmime').
pokemon('mrmimegalar').
pokemon('scyther').
pokemon('jynx').
pokemon('electabuzz').
pokemon('magmar').
pokemon('pinsir').
pokemon('pinsirmega').
pokemon('tauros').
pokemon('taurospaldeacombat').
pokemon('taurospaldeablaze').
pokemon('taurospaldeaaqua').
pokemon('magikarp').
pokemon('gyarados').
pokemon('gyaradosmega').
pokemon('lapras').
pokemon('laprasgmax').
pokemon('ditto').
pokemon('eevee').
pokemon('eeveestarter').
pokemon('eeveegmax').
pokemon('vaporeon').
pokemon('jolteon').
pokemon('flareon').
pokemon('porygon').
pokemon('omanyte').
pokemon('omastar').
pokemon('kabuto').
pokemon('kabutops').
pokemon('aerodactyl').
pokemon('aerodactylmega').
pokemon('snorlax').
pokemon('snorlaxgmax').
pokemon('articuno').
pokemon('articunogalar').
pokemon('zapdos').
pokemon('zapdosgalar').
pokemon('moltres').
pokemon('moltresgalar').
pokemon('dratini').
pokemon('dragonair').
pokemon('dragonite').
pokemon('dragonitemega').
pokemon('mewtwo').
pokemon('mewtwomegax').
pokemon('mewtwomegay').
pokemon('mew').
pokemon('chikorita').
pokemon('bayleef').
pokemon('meganium').
pokemon('meganiummega').
pokemon('cyndaquil').
pokemon('quilava').
pokemon('typhlosion').
pokemon('typhlosionhisui').
pokemon('totodile').
pokemon('croconaw').
pokemon('feraligatr').
pokemon('feraligatrmega').
pokemon('sentret').
pokemon('furret').
pokemon('hoothoot').
pokemon('noctowl').
pokemon('ledyba').
pokemon('ledian').
pokemon('spinarak').
pokemon('ariados').
pokemon('crobat').
pokemon('chinchou').
pokemon('lanturn').
pokemon('pichu').
pokemon('pichuspikyeared').
pokemon('cleffa').
pokemon('igglybuff').
pokemon('togepi').
pokemon('togetic').
pokemon('natu').
pokemon('xatu').
pokemon('mareep').
pokemon('flaaffy').
pokemon('ampharos').
pokemon('ampharosmega').
pokemon('bellossom').
pokemon('marill').
pokemon('azumarill').
pokemon('sudowoodo').
pokemon('politoed').
pokemon('hoppip').
pokemon('skiploom').
pokemon('jumpluff').
pokemon('aipom').
pokemon('sunkern').
pokemon('sunflora').
pokemon('yanma').
pokemon('wooper').
pokemon('wooperpaldea').
pokemon('quagsire').
pokemon('espeon').
pokemon('umbreon').
pokemon('murkrow').
pokemon('slowking').
pokemon('slowkinggalar').
pokemon('misdreavus').
pokemon('unown').
pokemon('wobbuffet').
pokemon('girafarig').
pokemon('pineco').
pokemon('forretress').
pokemon('dunsparce').
pokemon('gligar').
pokemon('steelix').
pokemon('steelixmega').
pokemon('snubbull').
pokemon('granbull').
pokemon('qwilfish').
pokemon('qwilfishhisui').
pokemon('scizor').
pokemon('scizormega').
pokemon('shuckle').
pokemon('heracross').
pokemon('heracrossmega').
pokemon('sneasel').
pokemon('sneaselhisui').
pokemon('teddiursa').
pokemon('ursaring').
pokemon('slugma').
pokemon('magcargo').
pokemon('swinub').
pokemon('piloswine').
pokemon('corsola').
pokemon('corsolagalar').
pokemon('remoraid').
pokemon('octillery').
pokemon('delibird').
pokemon('mantine').
pokemon('skarmory').
pokemon('skarmorymega').
pokemon('houndour').
pokemon('houndoom').
pokemon('houndoommega').
pokemon('kingdra').
pokemon('phanpy').
pokemon('donphan').
pokemon('porygon2').
pokemon('stantler').
pokemon('smeargle').
pokemon('tyrogue').
pokemon('hitmontop').
pokemon('smoochum').
pokemon('elekid').
pokemon('magby').
pokemon('miltank').
pokemon('blissey').
pokemon('raikou').
pokemon('entei').
pokemon('suicune').
pokemon('larvitar').
pokemon('pupitar').
pokemon('tyranitar').
pokemon('tyranitarmega').
pokemon('lugia').
pokemon('hooh').
pokemon('celebi').
pokemon('treecko').
pokemon('grovyle').
pokemon('sceptile').
pokemon('sceptilemega').
pokemon('torchic').
pokemon('combusken').
pokemon('blaziken').
pokemon('blazikenmega').
pokemon('mudkip').
pokemon('marshtomp').
pokemon('swampert').
pokemon('swampertmega').
pokemon('poochyena').
pokemon('mightyena').
pokemon('zigzagoon').
pokemon('zigzagoongalar').
pokemon('linoone').
pokemon('linoonegalar').
pokemon('wurmple').
pokemon('silcoon').
pokemon('beautifly').
pokemon('cascoon').
pokemon('dustox').
pokemon('lotad').
pokemon('lombre').
pokemon('ludicolo').
pokemon('seedot').
pokemon('nuzleaf').
pokemon('shiftry').
pokemon('taillow').
pokemon('swellow').
pokemon('wingull').
pokemon('pelipper').
pokemon('ralts').
pokemon('kirlia').
pokemon('gardevoir').
pokemon('gardevoirmega').
pokemon('surskit').
pokemon('masquerain').
pokemon('shroomish').
pokemon('breloom').
pokemon('slakoth').
pokemon('vigoroth').
pokemon('slaking').
pokemon('nincada').
pokemon('ninjask').
pokemon('shedinja').
pokemon('whismur').
pokemon('loudred').
pokemon('exploud').
pokemon('makuhita').
pokemon('hariyama').
pokemon('azurill').
pokemon('nosepass').
pokemon('skitty').
pokemon('delcatty').
pokemon('sableye').
pokemon('sableyemega').
pokemon('mawile').
pokemon('mawilemega').
pokemon('aron').
pokemon('lairon').
pokemon('aggron').
pokemon('aggronmega').
pokemon('meditite').
pokemon('medicham').
pokemon('medichammega').
pokemon('electrike').
pokemon('manectric').
pokemon('manectricmega').
pokemon('plusle').
pokemon('minun').
pokemon('volbeat').
pokemon('illumise').
pokemon('roselia').
pokemon('gulpin').
pokemon('swalot').
pokemon('carvanha').
pokemon('sharpedo').
pokemon('sharpedomega').
pokemon('wailmer').
pokemon('wailord').
pokemon('numel').
pokemon('camerupt').
pokemon('cameruptmega').
pokemon('torkoal').
pokemon('spoink').
pokemon('grumpig').
pokemon('spinda').
pokemon('trapinch').
pokemon('vibrava').
pokemon('flygon').
pokemon('cacnea').
pokemon('cacturne').
pokemon('swablu').
pokemon('altaria').
pokemon('altariamega').
pokemon('zangoose').
pokemon('seviper').
pokemon('lunatone').
pokemon('solrock').
pokemon('barboach').
pokemon('whiscash').
pokemon('corphish').
pokemon('crawdaunt').
pokemon('baltoy').
pokemon('claydol').
pokemon('lileep').
pokemon('cradily').
pokemon('anorith').
pokemon('armaldo').
pokemon('feebas').
pokemon('milotic').
pokemon('castform').
pokemon('castformsunny').
pokemon('castformrainy').
pokemon('castformsnowy').
pokemon('kecleon').
pokemon('shuppet').
pokemon('banette').
pokemon('banettemega').
pokemon('duskull').
pokemon('dusclops').
pokemon('tropius').
pokemon('chimecho').
pokemon('absol').
pokemon('absolmega').
pokemon('wynaut').
pokemon('snorunt').
pokemon('glalie').
pokemon('glaliemega').
pokemon('spheal').
pokemon('sealeo').
pokemon('walrein').
pokemon('clamperl').
pokemon('huntail').
pokemon('gorebyss').
pokemon('relicanth').
pokemon('luvdisc').
pokemon('bagon').
pokemon('shelgon').
pokemon('salamence').
pokemon('salamencemega').
pokemon('beldum').
pokemon('metang').
pokemon('metagross').
pokemon('metagrossmega').
pokemon('regirock').
pokemon('regice').
pokemon('registeel').
pokemon('latias').
pokemon('latiasmega').
pokemon('latios').
pokemon('latiosmega').
pokemon('kyogre').
pokemon('kyogreprimal').
pokemon('groudon').
pokemon('groudonprimal').
pokemon('rayquaza').
pokemon('rayquazamega').
pokemon('jirachi').
pokemon('deoxys').
pokemon('deoxysattack').
pokemon('deoxysdefense').
pokemon('deoxysspeed').
pokemon('turtwig').
pokemon('grotle').
pokemon('torterra').
pokemon('chimchar').
pokemon('monferno').
pokemon('infernape').
pokemon('piplup').
pokemon('prinplup').
pokemon('empoleon').
pokemon('starly').
pokemon('staravia').
pokemon('staraptor').
pokemon('bidoof').
pokemon('bibarel').
pokemon('kricketot').
pokemon('kricketune').
pokemon('shinx').
pokemon('luxio').
pokemon('luxray').
pokemon('budew').
pokemon('roserade').
pokemon('cranidos').
pokemon('rampardos').
pokemon('shieldon').
pokemon('bastiodon').
pokemon('burmy').
pokemon('burmysandy').
pokemon('burmytrash').
pokemon('wormadam').
pokemon('wormadamsandy').
pokemon('wormadamtrash').
pokemon('mothim').
pokemon('combee').
pokemon('vespiquen').
pokemon('pachirisu').
pokemon('buizel').
pokemon('floatzel').
pokemon('cherubi').
pokemon('cherrim').
pokemon('cherrimsunshine').
pokemon('shellos').
pokemon('shelloseast').
pokemon('gastrodon').
pokemon('gastrodoneast').
pokemon('ambipom').
pokemon('drifloon').
pokemon('drifblim').
pokemon('buneary').
pokemon('lopunny').
pokemon('lopunnymega').
pokemon('mismagius').
pokemon('honchkrow').
pokemon('glameow').
pokemon('purugly').
pokemon('chingling').
pokemon('stunky').
pokemon('skuntank').
pokemon('bronzor').
pokemon('bronzong').
pokemon('bonsly').
pokemon('mimejr').
pokemon('happiny').
pokemon('chatot').
pokemon('spiritomb').
pokemon('gible').
pokemon('gabite').
pokemon('garchomp').
pokemon('garchompmega').
pokemon('munchlax').
pokemon('riolu').
pokemon('lucario').
pokemon('lucariomega').
pokemon('hippopotas').
pokemon('hippowdon').
pokemon('skorupi').
pokemon('drapion').
pokemon('croagunk').
pokemon('toxicroak').
pokemon('carnivine').
pokemon('finneon').
pokemon('lumineon').
pokemon('mantyke').
pokemon('snover').
pokemon('abomasnow').
pokemon('abomasnowmega').
pokemon('weavile').
pokemon('magnezone').
pokemon('lickilicky').
pokemon('rhyperior').
pokemon('tangrowth').
pokemon('electivire').
pokemon('magmortar').
pokemon('togekiss').
pokemon('yanmega').
pokemon('leafeon').
pokemon('glaceon').
pokemon('gliscor').
pokemon('mamoswine').
pokemon('porygonz').
pokemon('gallade').
pokemon('gallademega').
pokemon('probopass').
pokemon('dusknoir').
pokemon('froslass').
pokemon('froslassmega').
pokemon('rotom').
pokemon('rotomheat').
pokemon('rotomwash').
pokemon('rotomfrost').
pokemon('rotomfan').
pokemon('rotommow').
pokemon('uxie').
pokemon('mesprit').
pokemon('azelf').
pokemon('dialga').
pokemon('dialgaorigin').
pokemon('palkia').
pokemon('palkiaorigin').
pokemon('heatran').
pokemon('regigigas').
pokemon('giratina').
pokemon('giratinaorigin').
pokemon('cresselia').
pokemon('phione').
pokemon('manaphy').
pokemon('darkrai').
pokemon('shaymin').
pokemon('shayminsky').
pokemon('arceus').
pokemon('arceusbug').
pokemon('arceusdark').
pokemon('arceusdragon').
pokemon('arceuselectric').
pokemon('arceusfairy').
pokemon('arceusfighting').
pokemon('arceusfire').
pokemon('arceusflying').
pokemon('arceusghost').
pokemon('arceusgrass').
pokemon('arceusground').
pokemon('arceusice').
pokemon('arceuspoison').
pokemon('arceuspsychic').
pokemon('arceusrock').
pokemon('arceussteel').
pokemon('arceuswater').
pokemon('victini').
pokemon('snivy').
pokemon('servine').
pokemon('serperior').
pokemon('tepig').
pokemon('pignite').
pokemon('emboar').
pokemon('emboarmega').
pokemon('oshawott').
pokemon('dewott').
pokemon('samurott').
pokemon('samurotthisui').
pokemon('patrat').
pokemon('watchog').
pokemon('lillipup').
pokemon('herdier').
pokemon('stoutland').
pokemon('purrloin').
pokemon('liepard').
pokemon('pansage').
pokemon('simisage').
pokemon('pansear').
pokemon('simisear').
pokemon('panpour').
pokemon('simipour').
pokemon('munna').
pokemon('musharna').
pokemon('pidove').
pokemon('tranquill').
pokemon('unfezant').
pokemon('blitzle').
pokemon('zebstrika').
pokemon('roggenrola').
pokemon('boldore').
pokemon('gigalith').
pokemon('woobat').
pokemon('swoobat').
pokemon('drilbur').
pokemon('excadrill').
pokemon('excadrillmega').
pokemon('audino').
pokemon('audinomega').
pokemon('timburr').
pokemon('gurdurr').
pokemon('conkeldurr').
pokemon('tympole').
pokemon('palpitoad').
pokemon('seismitoad').
pokemon('throh').
pokemon('sawk').
pokemon('sewaddle').
pokemon('swadloon').
pokemon('leavanny').
pokemon('venipede').
pokemon('whirlipede').
pokemon('scolipede').
pokemon('scolipedemega').
pokemon('cottonee').
pokemon('whimsicott').
pokemon('petilil').
pokemon('lilligant').
pokemon('lilliganthisui').
pokemon('basculin').
pokemon('basculinbluestriped').
pokemon('basculinwhitestriped').
pokemon('sandile').
pokemon('krokorok').
pokemon('krookodile').
pokemon('darumaka').
pokemon('darumakagalar').
pokemon('darmanitan').
pokemon('darmanitanzen').
pokemon('darmanitangalar').
pokemon('darmanitangalarzen').
pokemon('maractus').
pokemon('dwebble').
pokemon('crustle').
pokemon('scraggy').
pokemon('scrafty').
pokemon('scraftymega').
pokemon('sigilyph').
pokemon('yamask').
pokemon('yamaskgalar').
pokemon('cofagrigus').
pokemon('tirtouga').
pokemon('carracosta').
pokemon('archen').
pokemon('archeops').
pokemon('trubbish').
pokemon('garbodor').
pokemon('garbodorgmax').
pokemon('zorua').
pokemon('zoruahisui').
pokemon('zoroark').
pokemon('zoroarkhisui').
pokemon('minccino').
pokemon('cinccino').
pokemon('gothita').
pokemon('gothorita').
pokemon('gothitelle').
pokemon('solosis').
pokemon('duosion').
pokemon('reuniclus').
pokemon('ducklett').
pokemon('swanna').
pokemon('vanillite').
pokemon('vanillish').
pokemon('vanilluxe').
pokemon('deerling').
pokemon('deerlingsummer').
pokemon('deerlingautumn').
pokemon('deerlingwinter').
pokemon('sawsbuck').
pokemon('emolga').
pokemon('karrablast').
pokemon('escavalier').
pokemon('foongus').
pokemon('amoonguss').
pokemon('frillish').
pokemon('jellicent').
pokemon('alomomola').
pokemon('joltik').
pokemon('galvantula').
pokemon('ferroseed').
pokemon('ferrothorn').
pokemon('klink').
pokemon('klang').
pokemon('klinklang').
pokemon('tynamo').
pokemon('eelektrik').
pokemon('eelektross').
pokemon('eelektrossmega').
pokemon('elgyem').
pokemon('beheeyem').
pokemon('litwick').
pokemon('lampent').
pokemon('chandelure').
pokemon('chandeluremega').
pokemon('axew').
pokemon('fraxure').
pokemon('haxorus').
pokemon('cubchoo').
pokemon('beartic').
pokemon('cryogonal').
pokemon('shelmet').
pokemon('accelgor').
pokemon('stunfisk').
pokemon('stunfiskgalar').
pokemon('mienfoo').
pokemon('mienshao').
pokemon('druddigon').
pokemon('golett').
pokemon('golurk').
pokemon('pawniard').
pokemon('bisharp').
pokemon('bouffalant').
pokemon('rufflet').
pokemon('braviary').
pokemon('braviaryhisui').
pokemon('vullaby').
pokemon('mandibuzz').
pokemon('heatmor').
pokemon('durant').
pokemon('deino').
pokemon('zweilous').
pokemon('hydreigon').
pokemon('larvesta').
pokemon('volcarona').
pokemon('cobalion').
pokemon('terrakion').
pokemon('virizion').
pokemon('tornadus').
pokemon('tornadustherian').
pokemon('thundurus').
pokemon('thundurustherian').
pokemon('reshiram').
pokemon('zekrom').
pokemon('landorus').
pokemon('landorustherian').
pokemon('kyurem').
pokemon('kyuremblack').
pokemon('kyuremwhite').
pokemon('keldeo').
pokemon('keldeoresolute').
pokemon('meloetta').
pokemon('meloettapirouette').
pokemon('genesect').
pokemon('genesectdouse').
pokemon('genesectshock').
pokemon('genesectburn').
pokemon('genesectchill').
pokemon('chespin').
pokemon('quilladin').
pokemon('chesnaught').
pokemon('chesnaughtmega').
pokemon('fennekin').
pokemon('braixen').
pokemon('delphox').
pokemon('delphoxmega').
pokemon('froakie').
pokemon('frogadier').
pokemon('greninja').
pokemon('greninjabond').
pokemon('greninjaash').
pokemon('greninjamega').
pokemon('bunnelby').
pokemon('diggersby').
pokemon('fletchling').
pokemon('fletchinder').
pokemon('talonflame').
pokemon('scatterbug').
pokemon('spewpa').
pokemon('vivillon').
pokemon('vivillonicysnow').
pokemon('vivillonpolar').
pokemon('vivillontundra').
pokemon('vivilloncontinental').
pokemon('vivillongarden').
pokemon('vivillonelegant').
pokemon('vivillonmodern').
pokemon('vivillonmarine').
pokemon('vivillonarchipelago').
pokemon('vivillonhighplains').
pokemon('vivillonsandstorm').
pokemon('vivillonriver').
pokemon('vivillonmonsoon').
pokemon('vivillonsavanna').
pokemon('vivillonsun').
pokemon('vivillonocean').
pokemon('vivillonjungle').
pokemon('vivillonfancy').
pokemon('vivillonpokeball').
pokemon('litleo').
pokemon('pyroar').
pokemon('pyroarmega').
pokemon('flabebe').
pokemon('floette').
pokemon('floetteeternal').
pokemon('floettemega').
pokemon('florges').
pokemon('skiddo').
pokemon('gogoat').
pokemon('pancham').
pokemon('pangoro').
pokemon('furfrou').
pokemon('espurr').
pokemon('meowstic').
pokemon('meowsticf').
pokemon('honedge').
pokemon('doublade').
pokemon('aegislash').
pokemon('aegislashblade').
pokemon('spritzee').
pokemon('aromatisse').
pokemon('swirlix').
pokemon('slurpuff').
pokemon('inkay').
pokemon('malamar').
pokemon('malamarmega').
pokemon('binacle').
pokemon('barbaracle').
pokemon('barbaraclemega').
pokemon('skrelp').
pokemon('dragalge').
pokemon('dragalgemega').
pokemon('clauncher').
pokemon('clawitzer').
pokemon('helioptile').
pokemon('heliolisk').
pokemon('tyrunt').
pokemon('tyrantrum').
pokemon('amaura').
pokemon('aurorus').
pokemon('sylveon').
pokemon('hawlucha').
pokemon('hawluchamega').
pokemon('dedenne').
pokemon('carbink').
pokemon('goomy').
pokemon('sliggoo').
pokemon('sliggoohisui').
pokemon('goodra').
pokemon('goodrahisui').
pokemon('klefki').
pokemon('phantump').
pokemon('trevenant').
pokemon('pumpkaboo').
pokemon('pumpkaboosmall').
pokemon('pumpkaboolarge').
pokemon('pumpkaboosuper').
pokemon('gourgeist').
pokemon('gourgeistsmall').
pokemon('gourgeistlarge').
pokemon('gourgeistsuper').
pokemon('bergmite').
pokemon('avalugg').
pokemon('avalugghisui').
pokemon('noibat').
pokemon('noivern').
pokemon('xerneas').
pokemon('xerneasneutral').
pokemon('yveltal').
pokemon('zygarde').
pokemon('zygarde10').
pokemon('zygardecomplete').
pokemon('zygardemega').
pokemon('diancie').
pokemon('dianciemega').
pokemon('hoopa').
pokemon('hoopaunbound').
pokemon('volcanion').
pokemon('rowlet').
pokemon('dartrix').
pokemon('decidueye').
pokemon('decidueyehisui').
pokemon('litten').
pokemon('torracat').
pokemon('incineroar').
pokemon('popplio').
pokemon('brionne').
pokemon('primarina').
pokemon('pikipek').
pokemon('trumbeak').
pokemon('toucannon').
pokemon('yungoos').
pokemon('gumshoos').
pokemon('gumshoostotem').
pokemon('grubbin').
pokemon('charjabug').
pokemon('vikavolt').
pokemon('vikavolttotem').
pokemon('crabrawler').
pokemon('crabominable').
pokemon('oricorio').
pokemon('oricoriopompom').
pokemon('oricoriopau').
pokemon('oricoriosensu').
pokemon('cutiefly').
pokemon('ribombee').
pokemon('ribombeetotem').
pokemon('rockruff').
pokemon('rockruffdusk').
pokemon('lycanroc').
pokemon('lycanrocmidnight').
pokemon('lycanrocdusk').
pokemon('wishiwashi').
pokemon('wishiwashischool').
pokemon('mareanie').
pokemon('toxapex').
pokemon('mudbray').
pokemon('mudsdale').
pokemon('dewpider').
pokemon('araquanid').
pokemon('araquanidtotem').
pokemon('fomantis').
pokemon('lurantis').
pokemon('lurantistotem').
pokemon('morelull').
pokemon('shiinotic').
pokemon('salandit').
pokemon('salazzle').
pokemon('salazzletotem').
pokemon('stufful').
pokemon('bewear').
pokemon('bounsweet').
pokemon('steenee').
pokemon('tsareena').
pokemon('comfey').
pokemon('oranguru').
pokemon('passimian').
pokemon('wimpod').
pokemon('golisopod').
pokemon('sandygast').
pokemon('palossand').
pokemon('pyukumuku').
pokemon('typenull').
pokemon('silvally').
pokemon('silvallybug').
pokemon('silvallydark').
pokemon('silvallydragon').
pokemon('silvallyelectric').
pokemon('silvallyfairy').
pokemon('silvallyfighting').
pokemon('silvallyfire').
pokemon('silvallyflying').
pokemon('silvallyghost').
pokemon('silvallygrass').
pokemon('silvallyground').
pokemon('silvallyice').
pokemon('silvallypoison').
pokemon('silvallypsychic').
pokemon('silvallyrock').
pokemon('silvallysteel').
pokemon('silvallywater').
pokemon('minior').
pokemon('miniororange').
pokemon('minioryellow').
pokemon('miniorgreen').
pokemon('miniorblue').
pokemon('miniorindigo').
pokemon('miniorviolet').
pokemon('miniormeteor').
pokemon('komala').
pokemon('turtonator').
pokemon('togedemaru').
pokemon('togedemarutotem').
pokemon('mimikyu').
pokemon('mimikyubusted').
pokemon('mimikyutotem').
pokemon('mimikyubustedtotem').
pokemon('bruxish').
pokemon('drampa').
pokemon('drampamega').
pokemon('dhelmise').
pokemon('jangmoo').
pokemon('hakamoo').
pokemon('kommoo').
pokemon('kommoototem').
pokemon('tapukoko').
pokemon('tapulele').
pokemon('tapubulu').
pokemon('tapufini').
pokemon('cosmog').
pokemon('cosmoem').
pokemon('solgaleo').
pokemon('lunala').
pokemon('nihilego').
pokemon('buzzwole').
pokemon('pheromosa').
pokemon('xurkitree').
pokemon('celesteela').
pokemon('kartana').
pokemon('guzzlord').
pokemon('necrozma').
pokemon('necrozmaduskmane').
pokemon('necrozmadawnwings').
pokemon('necrozmaultra').
pokemon('magearna').
pokemon('magearnaoriginal').
pokemon('marshadow').
pokemon('poipole').
pokemon('naganadel').
pokemon('stakataka').
pokemon('blacephalon').
pokemon('zeraora').
pokemon('meltan').
pokemon('melmetal').
pokemon('melmetalgmax').
pokemon('grookey').
pokemon('thwackey').
pokemon('rillaboom').
pokemon('rillaboomgmax').
pokemon('scorbunny').
pokemon('raboot').
pokemon('cinderace').
pokemon('cinderacegmax').
pokemon('sobble').
pokemon('drizzile').
pokemon('inteleon').
pokemon('inteleongmax').
pokemon('skwovet').
pokemon('greedent').
pokemon('rookidee').
pokemon('corvisquire').
pokemon('corviknight').
pokemon('corviknightgmax').
pokemon('blipbug').
pokemon('dottler').
pokemon('orbeetle').
pokemon('orbeetlegmax').
pokemon('nickit').
pokemon('thievul').
pokemon('gossifleur').
pokemon('eldegoss').
pokemon('wooloo').
pokemon('dubwool').
pokemon('chewtle').
pokemon('drednaw').
pokemon('drednawgmax').
pokemon('yamper').
pokemon('boltund').
pokemon('rolycoly').
pokemon('carkol').
pokemon('coalossal').
pokemon('coalossalgmax').
pokemon('applin').
pokemon('flapple').
pokemon('flapplegmax').
pokemon('appletun').
pokemon('appletungmax').
pokemon('silicobra').
pokemon('sandaconda').
pokemon('sandacondagmax').
pokemon('cramorant').
pokemon('cramorantgulping').
pokemon('cramorantgorging').
pokemon('arrokuda').
pokemon('barraskewda').
pokemon('toxel').
pokemon('toxtricity').
pokemon('toxtricitylowkey').
pokemon('toxtricitygmax').
pokemon('toxtricitylowkeygmax').
pokemon('sizzlipede').
pokemon('centiskorch').
pokemon('centiskorchgmax').
pokemon('clobbopus').
pokemon('grapploct').
pokemon('sinistea').
pokemon('sinisteaantique').
pokemon('polteageist').
pokemon('polteageistantique').
pokemon('hatenna').
pokemon('hattrem').
pokemon('hatterene').
pokemon('hatterenegmax').
pokemon('impidimp').
pokemon('morgrem').
pokemon('grimmsnarl').
pokemon('grimmsnarlgmax').
pokemon('obstagoon').
pokemon('perrserker').
pokemon('cursola').
pokemon('sirfetchd').
pokemon('mrrime').
pokemon('runerigus').
pokemon('milcery').
pokemon('alcremie').
pokemon('alcremierubycream').
pokemon('alcremiematchacream').
pokemon('alcremiemintcream').
pokemon('alcremielemoncream').
pokemon('alcremierubyswirl').
pokemon('alcremiecaramelswirl').
pokemon('alcremierainbowswirl').
pokemon('alcremiegmax').
pokemon('falinks').
pokemon('falinksmega').
pokemon('pincurchin').
pokemon('snom').
pokemon('frosmoth').
pokemon('stonjourner').
pokemon('eiscue').
pokemon('eiscuenoice').
pokemon('indeedee').
pokemon('indeedeef').
pokemon('morpeko').
pokemon('morpekohangry').
pokemon('cufant').
pokemon('copperajah').
pokemon('copperajahgmax').
pokemon('dracozolt').
pokemon('arctozolt').
pokemon('dracovish').
pokemon('arctovish').
pokemon('duraludon').
pokemon('duraludongmax').
pokemon('dreepy').
pokemon('drakloak').
pokemon('dragapult').
pokemon('zacian').
pokemon('zaciancrowned').
pokemon('zamazenta').
pokemon('zamazentacrowned').
pokemon('eternatus').
pokemon('eternatuseternamax').
pokemon('kubfu').
pokemon('urshifu').
pokemon('urshifurapidstrike').
pokemon('urshifugmax').
pokemon('urshifurapidstrikegmax').
pokemon('zarude').
pokemon('zarudedada').
pokemon('regieleki').
pokemon('regidrago').
pokemon('glastrier').
pokemon('spectrier').
pokemon('calyrex').
pokemon('calyrexice').
pokemon('calyrexshadow').
pokemon('wyrdeer').
pokemon('kleavor').
pokemon('ursaluna').
pokemon('ursalunabloodmoon').
pokemon('basculegion').
pokemon('basculegionf').
pokemon('sneasler').
pokemon('overqwil').
pokemon('enamorus').
pokemon('enamorustherian').
pokemon('sprigatito').
pokemon('floragato').
pokemon('meowscarada').
pokemon('fuecoco').
pokemon('crocalor').
pokemon('skeledirge').
pokemon('quaxly').
pokemon('quaxwell').
pokemon('quaquaval').
pokemon('lechonk').
pokemon('oinkologne').
pokemon('oinkolognef').
pokemon('tarountula').
pokemon('spidops').
pokemon('nymble').
pokemon('lokix').
pokemon('pawmi').
pokemon('pawmo').
pokemon('pawmot').
pokemon('tandemaus').
pokemon('maushold').
pokemon('mausholdfour').
pokemon('fidough').
pokemon('dachsbun').
pokemon('smoliv').
pokemon('dolliv').
pokemon('arboliva').
pokemon('squawkabilly').
pokemon('squawkabillyblue').
pokemon('squawkabillyyellow').
pokemon('squawkabillywhite').
pokemon('nacli').
pokemon('naclstack').
pokemon('garganacl').
pokemon('charcadet').
pokemon('armarouge').
pokemon('ceruledge').
pokemon('tadbulb').
pokemon('bellibolt').
pokemon('wattrel').
pokemon('kilowattrel').
pokemon('maschiff').
pokemon('mabosstiff').
pokemon('shroodle').
pokemon('grafaiai').
pokemon('bramblin').
pokemon('brambleghast').
pokemon('toedscool').
pokemon('toedscruel').
pokemon('klawf').
pokemon('capsakid').
pokemon('scovillain').
pokemon('rellor').
pokemon('rabsca').
pokemon('flittle').
pokemon('espathra').
pokemon('tinkatink').
pokemon('tinkatuff').
pokemon('tinkaton').
pokemon('wiglett').
pokemon('wugtrio').
pokemon('bombirdier').
pokemon('finizen').
pokemon('palafin').
pokemon('palafinhero').
pokemon('varoom').
pokemon('revavroom').
pokemon('cyclizar').
pokemon('orthworm').
pokemon('glimmet').
pokemon('glimmora').
pokemon('greavard').
pokemon('houndstone').
pokemon('flamigo').
pokemon('cetoddle').
pokemon('cetitan').
pokemon('veluza').
pokemon('dondozo').
pokemon('tatsugiri').
pokemon('tatsugiridroopy').
pokemon('tatsugiristretchy').
pokemon('annihilape').
pokemon('clodsire').
pokemon('farigiraf').
pokemon('dudunsparce').
pokemon('dudunsparcethreesegment').
pokemon('kingambit').
pokemon('greattusk').
pokemon('screamtail').
pokemon('brutebonnet').
pokemon('fluttermane').
pokemon('slitherwing').
pokemon('sandyshocks').
pokemon('irontreads').
pokemon('ironbundle').
pokemon('ironhands').
pokemon('ironjugulis').
pokemon('ironmoth').
pokemon('ironthorns').
pokemon('frigibax').
pokemon('arctibax').
pokemon('baxcalibur').
pokemon('gimmighoul').
pokemon('gimmighoulroaming').
pokemon('gholdengo').
pokemon('wochien').
pokemon('chienpao').
pokemon('tinglu').
pokemon('chiyu').
pokemon('roaringmoon').
pokemon('ironvaliant').
pokemon('koraidon').
pokemon('miraidon').
pokemon('walkingwake').
pokemon('ironleaves').
pokemon('dipplin').
pokemon('poltchageist').
pokemon('poltchageistartisan').
pokemon('sinistcha').
pokemon('sinistchamasterpiece').
pokemon('okidogi').
pokemon('munkidori').
pokemon('fezandipiti').
pokemon('ogerpon').
pokemon('ogerponwellspring').
pokemon('ogerponhearthflame').
pokemon('ogerponcornerstone').
pokemon('ogerpontealtera').
pokemon('ogerponwellspringtera').
pokemon('ogerponhearthflametera').
pokemon('ogerponcornerstonetera').
pokemon('archaludon').
pokemon('hydrapple').
pokemon('gougingfire').
pokemon('ragingbolt').
pokemon('ironboulder').
pokemon('ironcrown').
pokemon('terapagos').
pokemon('terapagosterastal').
pokemon('terapagosstellar').
pokemon('pecharunt').
pokemon('missingno').
pokemon('ramnarok').
pokemon('ramnarokradiant').
pokemon('pokestarsmeargle').
pokemon('pokestarufo').
pokemon('pokestarufo2').
pokemon('pokestarbrycenman').
pokemon('pokestarmt').
pokemon('pokestarmt2').
pokemon('pokestartransport').
pokemon('pokestargiant').
pokemon('pokestarhumanoid').
pokemon('pokestarmonster').
pokemon('pokestarf00').
pokemon('pokestarf002').
pokemon('pokestarspirit').
pokemon('pokestarblackdoor').
pokemon('pokestarwhitedoor').
pokemon('pokestarblackbelt').
pokemon('pokestarufopropu2').
pokemon_hp('bulbasaur', 45).
pokemon_hp('ivysaur', 60).
pokemon_hp('venusaur', 80).
pokemon_hp('venusaurmega', 80).
pokemon_hp('venusaurgmax', 80).
pokemon_hp('charmander', 39).
pokemon_hp('charmeleon', 58).
pokemon_hp('charizard', 78).
pokemon_hp('charizardmegax', 78).
pokemon_hp('charizardmegay', 78).
pokemon_hp('charizardgmax', 78).
pokemon_hp('squirtle', 44).
pokemon_hp('wartortle', 59).
pokemon_hp('blastoise', 79).
pokemon_hp('blastoisemega', 79).
pokemon_hp('blastoisegmax', 79).
pokemon_hp('caterpie', 45).
pokemon_hp('metapod', 50).
pokemon_hp('butterfree', 60).
pokemon_hp('butterfreegmax', 60).
pokemon_hp('weedle', 40).
pokemon_hp('kakuna', 45).
pokemon_hp('beedrill', 65).
pokemon_hp('beedrillmega', 65).
pokemon_hp('pidgey', 40).
pokemon_hp('pidgeotto', 63).
pokemon_hp('pidgeot', 83).
pokemon_hp('pidgeotmega', 83).
pokemon_hp('rattata', 30).
pokemon_hp('rattataalola', 30).
pokemon_hp('raticate', 55).
pokemon_hp('raticatealola', 75).
pokemon_hp('raticatealolatotem', 75).
pokemon_hp('spearow', 40).
pokemon_hp('fearow', 65).
pokemon_hp('ekans', 35).
pokemon_hp('arbok', 60).
pokemon_hp('pikachu', 35).
pokemon_hp('pikachucosplay', 35).
pokemon_hp('pikachurockstar', 35).
pokemon_hp('pikachubelle', 35).
pokemon_hp('pikachupopstar', 35).
pokemon_hp('pikachuphd', 35).
pokemon_hp('pikachulibre', 35).
pokemon_hp('pikachuoriginal', 35).
pokemon_hp('pikachuhoenn', 35).
pokemon_hp('pikachusinnoh', 35).
pokemon_hp('pikachuunova', 35).
pokemon_hp('pikachukalos', 35).
pokemon_hp('pikachualola', 35).
pokemon_hp('pikachupartner', 35).
pokemon_hp('pikachustarter', 45).
pokemon_hp('pikachugmax', 35).
pokemon_hp('pikachuworld', 35).
pokemon_hp('raichu', 60).
pokemon_hp('raichualola', 60).
pokemon_hp('sandshrew', 50).
pokemon_hp('sandshrewalola', 50).
pokemon_hp('sandslash', 75).
pokemon_hp('sandslashalola', 75).
pokemon_hp('nidoranf', 55).
pokemon_hp('nidorina', 70).
pokemon_hp('nidoqueen', 90).
pokemon_hp('nidoranm', 46).
pokemon_hp('nidorino', 61).
pokemon_hp('nidoking', 81).
pokemon_hp('clefairy', 70).
pokemon_hp('clefable', 95).
pokemon_hp('clefablemega', 0).
pokemon_hp('vulpix', 38).
pokemon_hp('vulpixalola', 38).
pokemon_hp('ninetales', 73).
pokemon_hp('ninetalesalola', 73).
pokemon_hp('jigglypuff', 115).
pokemon_hp('wigglytuff', 140).
pokemon_hp('zubat', 40).
pokemon_hp('golbat', 75).
pokemon_hp('oddish', 45).
pokemon_hp('gloom', 60).
pokemon_hp('vileplume', 75).
pokemon_hp('paras', 35).
pokemon_hp('parasect', 60).
pokemon_hp('venonat', 60).
pokemon_hp('venomoth', 70).
pokemon_hp('diglett', 10).
pokemon_hp('diglettalola', 10).
pokemon_hp('dugtrio', 35).
pokemon_hp('dugtrioalola', 35).
pokemon_hp('meowth', 40).
pokemon_hp('meowthalola', 40).
pokemon_hp('meowthgalar', 50).
pokemon_hp('meowthgmax', 40).
pokemon_hp('persian', 65).
pokemon_hp('persianalola', 65).
pokemon_hp('psyduck', 50).
pokemon_hp('golduck', 80).
pokemon_hp('mankey', 40).
pokemon_hp('primeape', 65).
pokemon_hp('growlithe', 55).
pokemon_hp('growlithehisui', 60).
pokemon_hp('arcanine', 90).
pokemon_hp('arcaninehisui', 95).
pokemon_hp('poliwag', 40).
pokemon_hp('poliwhirl', 65).
pokemon_hp('poliwrath', 90).
pokemon_hp('abra', 25).
pokemon_hp('kadabra', 40).
pokemon_hp('alakazam', 55).
pokemon_hp('alakazammega', 55).
pokemon_hp('machop', 70).
pokemon_hp('machoke', 80).
pokemon_hp('machamp', 90).
pokemon_hp('machampgmax', 90).
pokemon_hp('bellsprout', 50).
pokemon_hp('weepinbell', 65).
pokemon_hp('victreebel', 80).
pokemon_hp('victreebelmega', 0).
pokemon_hp('tentacool', 40).
pokemon_hp('tentacruel', 80).
pokemon_hp('geodude', 40).
pokemon_hp('geodudealola', 40).
pokemon_hp('graveler', 55).
pokemon_hp('graveleralola', 55).
pokemon_hp('golem', 80).
pokemon_hp('golemalola', 80).
pokemon_hp('ponyta', 50).
pokemon_hp('ponytagalar', 50).
pokemon_hp('rapidash', 65).
pokemon_hp('rapidashgalar', 65).
pokemon_hp('slowpoke', 90).
pokemon_hp('slowpokegalar', 90).
pokemon_hp('slowbro', 95).
pokemon_hp('slowbromega', 95).
pokemon_hp('slowbrogalar', 95).
pokemon_hp('magnemite', 25).
pokemon_hp('magneton', 50).
pokemon_hp('farfetchd', 52).
pokemon_hp('farfetchdgalar', 52).
pokemon_hp('doduo', 35).
pokemon_hp('dodrio', 60).
pokemon_hp('seel', 65).
pokemon_hp('dewgong', 90).
pokemon_hp('grimer', 80).
pokemon_hp('grimeralola', 80).
pokemon_hp('muk', 105).
pokemon_hp('mukalola', 105).
pokemon_hp('shellder', 30).
pokemon_hp('cloyster', 50).
pokemon_hp('gastly', 30).
pokemon_hp('haunter', 45).
pokemon_hp('gengar', 60).
pokemon_hp('gengarmega', 60).
pokemon_hp('gengargmax', 60).
pokemon_hp('onix', 35).
pokemon_hp('drowzee', 60).
pokemon_hp('hypno', 85).
pokemon_hp('krabby', 30).
pokemon_hp('kingler', 55).
pokemon_hp('kinglergmax', 55).
pokemon_hp('voltorb', 40).
pokemon_hp('voltorbhisui', 40).
pokemon_hp('electrode', 60).
pokemon_hp('electrodehisui', 60).
pokemon_hp('exeggcute', 60).
pokemon_hp('exeggutor', 95).
pokemon_hp('exeggutoralola', 95).
pokemon_hp('cubone', 50).
pokemon_hp('marowak', 60).
pokemon_hp('marowakalola', 60).
pokemon_hp('marowakalolatotem', 60).
pokemon_hp('hitmonlee', 50).
pokemon_hp('hitmonchan', 50).
pokemon_hp('lickitung', 90).
pokemon_hp('koffing', 40).
pokemon_hp('weezing', 65).
pokemon_hp('weezinggalar', 65).
pokemon_hp('rhyhorn', 80).
pokemon_hp('rhydon', 105).
pokemon_hp('chansey', 250).
pokemon_hp('tangela', 65).
pokemon_hp('kangaskhan', 105).
pokemon_hp('kangaskhanmega', 105).
pokemon_hp('horsea', 30).
pokemon_hp('seadra', 55).
pokemon_hp('goldeen', 45).
pokemon_hp('seaking', 80).
pokemon_hp('staryu', 30).
pokemon_hp('starmie', 60).
pokemon_hp('starmiemega', 0).
pokemon_hp('mrmime', 40).
pokemon_hp('mrmimegalar', 50).
pokemon_hp('scyther', 70).
pokemon_hp('jynx', 65).
pokemon_hp('electabuzz', 65).
pokemon_hp('magmar', 65).
pokemon_hp('pinsir', 65).
pokemon_hp('pinsirmega', 65).
pokemon_hp('tauros', 75).
pokemon_hp('taurospaldeacombat', 75).
pokemon_hp('taurospaldeablaze', 75).
pokemon_hp('taurospaldeaaqua', 75).
pokemon_hp('magikarp', 20).
pokemon_hp('gyarados', 95).
pokemon_hp('gyaradosmega', 95).
pokemon_hp('lapras', 130).
pokemon_hp('laprasgmax', 130).
pokemon_hp('ditto', 48).
pokemon_hp('eevee', 55).
pokemon_hp('eeveestarter', 65).
pokemon_hp('eeveegmax', 55).
pokemon_hp('vaporeon', 130).
pokemon_hp('jolteon', 65).
pokemon_hp('flareon', 65).
pokemon_hp('porygon', 65).
pokemon_hp('omanyte', 35).
pokemon_hp('omastar', 70).
pokemon_hp('kabuto', 30).
pokemon_hp('kabutops', 60).
pokemon_hp('aerodactyl', 80).
pokemon_hp('aerodactylmega', 80).
pokemon_hp('snorlax', 160).
pokemon_hp('snorlaxgmax', 160).
pokemon_hp('articuno', 90).
pokemon_hp('articunogalar', 90).
pokemon_hp('zapdos', 90).
pokemon_hp('zapdosgalar', 90).
pokemon_hp('moltres', 90).
pokemon_hp('moltresgalar', 90).
pokemon_hp('dratini', 41).
pokemon_hp('dragonair', 61).
pokemon_hp('dragonite', 91).
pokemon_hp('dragonitemega', 0).
pokemon_hp('mewtwo', 106).
pokemon_hp('mewtwomegax', 106).
pokemon_hp('mewtwomegay', 106).
pokemon_hp('mew', 100).
pokemon_hp('chikorita', 45).
pokemon_hp('bayleef', 60).
pokemon_hp('meganium', 80).
pokemon_hp('meganiummega', 0).
pokemon_hp('cyndaquil', 39).
pokemon_hp('quilava', 58).
pokemon_hp('typhlosion', 78).
pokemon_hp('typhlosionhisui', 73).
pokemon_hp('totodile', 50).
pokemon_hp('croconaw', 65).
pokemon_hp('feraligatr', 85).
pokemon_hp('feraligatrmega', 0).
pokemon_hp('sentret', 35).
pokemon_hp('furret', 85).
pokemon_hp('hoothoot', 60).
pokemon_hp('noctowl', 100).
pokemon_hp('ledyba', 40).
pokemon_hp('ledian', 55).
pokemon_hp('spinarak', 40).
pokemon_hp('ariados', 70).
pokemon_hp('crobat', 85).
pokemon_hp('chinchou', 75).
pokemon_hp('lanturn', 125).
pokemon_hp('pichu', 20).
pokemon_hp('pichuspikyeared', 20).
pokemon_hp('cleffa', 50).
pokemon_hp('igglybuff', 90).
pokemon_hp('togepi', 35).
pokemon_hp('togetic', 55).
pokemon_hp('natu', 40).
pokemon_hp('xatu', 65).
pokemon_hp('mareep', 55).
pokemon_hp('flaaffy', 70).
pokemon_hp('ampharos', 90).
pokemon_hp('ampharosmega', 90).
pokemon_hp('bellossom', 75).
pokemon_hp('marill', 70).
pokemon_hp('azumarill', 100).
pokemon_hp('sudowoodo', 70).
pokemon_hp('politoed', 90).
pokemon_hp('hoppip', 35).
pokemon_hp('skiploom', 55).
pokemon_hp('jumpluff', 75).
pokemon_hp('aipom', 55).
pokemon_hp('sunkern', 30).
pokemon_hp('sunflora', 75).
pokemon_hp('yanma', 65).
pokemon_hp('wooper', 55).
pokemon_hp('wooperpaldea', 55).
pokemon_hp('quagsire', 95).
pokemon_hp('espeon', 65).
pokemon_hp('umbreon', 95).
pokemon_hp('murkrow', 60).
pokemon_hp('slowking', 95).
pokemon_hp('slowkinggalar', 95).
pokemon_hp('misdreavus', 60).
pokemon_hp('unown', 48).
pokemon_hp('wobbuffet', 190).
pokemon_hp('girafarig', 70).
pokemon_hp('pineco', 50).
pokemon_hp('forretress', 75).
pokemon_hp('dunsparce', 100).
pokemon_hp('gligar', 65).
pokemon_hp('steelix', 75).
pokemon_hp('steelixmega', 75).
pokemon_hp('snubbull', 60).
pokemon_hp('granbull', 90).
pokemon_hp('qwilfish', 65).
pokemon_hp('qwilfishhisui', 65).
pokemon_hp('scizor', 70).
pokemon_hp('scizormega', 70).
pokemon_hp('shuckle', 20).
pokemon_hp('heracross', 80).
pokemon_hp('heracrossmega', 80).
pokemon_hp('sneasel', 55).
pokemon_hp('sneaselhisui', 55).
pokemon_hp('teddiursa', 60).
pokemon_hp('ursaring', 90).
pokemon_hp('slugma', 40).
pokemon_hp('magcargo', 60).
pokemon_hp('swinub', 50).
pokemon_hp('piloswine', 100).
pokemon_hp('corsola', 65).
pokemon_hp('corsolagalar', 60).
pokemon_hp('remoraid', 35).
pokemon_hp('octillery', 75).
pokemon_hp('delibird', 45).
pokemon_hp('mantine', 85).
pokemon_hp('skarmory', 65).
pokemon_hp('skarmorymega', 0).
pokemon_hp('houndour', 45).
pokemon_hp('houndoom', 75).
pokemon_hp('houndoommega', 75).
pokemon_hp('kingdra', 75).
pokemon_hp('phanpy', 90).
pokemon_hp('donphan', 90).
pokemon_hp('porygon2', 85).
pokemon_hp('stantler', 73).
pokemon_hp('smeargle', 55).
pokemon_hp('tyrogue', 35).
pokemon_hp('hitmontop', 50).
pokemon_hp('smoochum', 45).
pokemon_hp('elekid', 45).
pokemon_hp('magby', 45).
pokemon_hp('miltank', 95).
pokemon_hp('blissey', 255).
pokemon_hp('raikou', 90).
pokemon_hp('entei', 115).
pokemon_hp('suicune', 100).
pokemon_hp('larvitar', 50).
pokemon_hp('pupitar', 70).
pokemon_hp('tyranitar', 100).
pokemon_hp('tyranitarmega', 100).
pokemon_hp('lugia', 106).
pokemon_hp('hooh', 106).
pokemon_hp('celebi', 100).
pokemon_hp('treecko', 40).
pokemon_hp('grovyle', 50).
pokemon_hp('sceptile', 70).
pokemon_hp('sceptilemega', 70).
pokemon_hp('torchic', 45).
pokemon_hp('combusken', 60).
pokemon_hp('blaziken', 80).
pokemon_hp('blazikenmega', 80).
pokemon_hp('mudkip', 50).
pokemon_hp('marshtomp', 70).
pokemon_hp('swampert', 100).
pokemon_hp('swampertmega', 100).
pokemon_hp('poochyena', 35).
pokemon_hp('mightyena', 70).
pokemon_hp('zigzagoon', 38).
pokemon_hp('zigzagoongalar', 38).
pokemon_hp('linoone', 78).
pokemon_hp('linoonegalar', 78).
pokemon_hp('wurmple', 45).
pokemon_hp('silcoon', 50).
pokemon_hp('beautifly', 60).
pokemon_hp('cascoon', 50).
pokemon_hp('dustox', 60).
pokemon_hp('lotad', 40).
pokemon_hp('lombre', 60).
pokemon_hp('ludicolo', 80).
pokemon_hp('seedot', 40).
pokemon_hp('nuzleaf', 70).
pokemon_hp('shiftry', 90).
pokemon_hp('taillow', 40).
pokemon_hp('swellow', 60).
pokemon_hp('wingull', 40).
pokemon_hp('pelipper', 60).
pokemon_hp('ralts', 28).
pokemon_hp('kirlia', 38).
pokemon_hp('gardevoir', 68).
pokemon_hp('gardevoirmega', 68).
pokemon_hp('surskit', 40).
pokemon_hp('masquerain', 70).
pokemon_hp('shroomish', 60).
pokemon_hp('breloom', 60).
pokemon_hp('slakoth', 60).
pokemon_hp('vigoroth', 80).
pokemon_hp('slaking', 150).
pokemon_hp('nincada', 31).
pokemon_hp('ninjask', 61).
pokemon_hp('shedinja', 1).
pokemon_hp('whismur', 64).
pokemon_hp('loudred', 84).
pokemon_hp('exploud', 104).
pokemon_hp('makuhita', 72).
pokemon_hp('hariyama', 144).
pokemon_hp('azurill', 50).
pokemon_hp('nosepass', 30).
pokemon_hp('skitty', 50).
pokemon_hp('delcatty', 70).
pokemon_hp('sableye', 50).
pokemon_hp('sableyemega', 50).
pokemon_hp('mawile', 50).
pokemon_hp('mawilemega', 50).
pokemon_hp('aron', 50).
pokemon_hp('lairon', 60).
pokemon_hp('aggron', 70).
pokemon_hp('aggronmega', 70).
pokemon_hp('meditite', 30).
pokemon_hp('medicham', 60).
pokemon_hp('medichammega', 60).
pokemon_hp('electrike', 40).
pokemon_hp('manectric', 70).
pokemon_hp('manectricmega', 70).
pokemon_hp('plusle', 60).
pokemon_hp('minun', 60).
pokemon_hp('volbeat', 65).
pokemon_hp('illumise', 65).
pokemon_hp('roselia', 50).
pokemon_hp('gulpin', 70).
pokemon_hp('swalot', 100).
pokemon_hp('carvanha', 45).
pokemon_hp('sharpedo', 70).
pokemon_hp('sharpedomega', 70).
pokemon_hp('wailmer', 130).
pokemon_hp('wailord', 170).
pokemon_hp('numel', 60).
pokemon_hp('camerupt', 70).
pokemon_hp('cameruptmega', 70).
pokemon_hp('torkoal', 70).
pokemon_hp('spoink', 60).
pokemon_hp('grumpig', 80).
pokemon_hp('spinda', 60).
pokemon_hp('trapinch', 45).
pokemon_hp('vibrava', 50).
pokemon_hp('flygon', 80).
pokemon_hp('cacnea', 50).
pokemon_hp('cacturne', 70).
pokemon_hp('swablu', 45).
pokemon_hp('altaria', 75).
pokemon_hp('altariamega', 75).
pokemon_hp('zangoose', 73).
pokemon_hp('seviper', 73).
pokemon_hp('lunatone', 90).
pokemon_hp('solrock', 90).
pokemon_hp('barboach', 50).
pokemon_hp('whiscash', 110).
pokemon_hp('corphish', 43).
pokemon_hp('crawdaunt', 63).
pokemon_hp('baltoy', 40).
pokemon_hp('claydol', 60).
pokemon_hp('lileep', 66).
pokemon_hp('cradily', 86).
pokemon_hp('anorith', 45).
pokemon_hp('armaldo', 75).
pokemon_hp('feebas', 20).
pokemon_hp('milotic', 95).
pokemon_hp('castform', 70).
pokemon_hp('castformsunny', 70).
pokemon_hp('castformrainy', 70).
pokemon_hp('castformsnowy', 70).
pokemon_hp('kecleon', 60).
pokemon_hp('shuppet', 44).
pokemon_hp('banette', 64).
pokemon_hp('banettemega', 64).
pokemon_hp('duskull', 20).
pokemon_hp('dusclops', 40).
pokemon_hp('tropius', 99).
pokemon_hp('chimecho', 75).
pokemon_hp('absol', 65).
pokemon_hp('absolmega', 65).
pokemon_hp('wynaut', 95).
pokemon_hp('snorunt', 50).
pokemon_hp('glalie', 80).
pokemon_hp('glaliemega', 80).
pokemon_hp('spheal', 70).
pokemon_hp('sealeo', 90).
pokemon_hp('walrein', 110).
pokemon_hp('clamperl', 35).
pokemon_hp('huntail', 55).
pokemon_hp('gorebyss', 55).
pokemon_hp('relicanth', 100).
pokemon_hp('luvdisc', 43).
pokemon_hp('bagon', 45).
pokemon_hp('shelgon', 65).
pokemon_hp('salamence', 95).
pokemon_hp('salamencemega', 95).
pokemon_hp('beldum', 40).
pokemon_hp('metang', 60).
pokemon_hp('metagross', 80).
pokemon_hp('metagrossmega', 80).
pokemon_hp('regirock', 80).
pokemon_hp('regice', 80).
pokemon_hp('registeel', 80).
pokemon_hp('latias', 80).
pokemon_hp('latiasmega', 80).
pokemon_hp('latios', 80).
pokemon_hp('latiosmega', 80).
pokemon_hp('kyogre', 100).
pokemon_hp('kyogreprimal', 100).
pokemon_hp('groudon', 100).
pokemon_hp('groudonprimal', 100).
pokemon_hp('rayquaza', 105).
pokemon_hp('rayquazamega', 105).
pokemon_hp('jirachi', 100).
pokemon_hp('deoxys', 50).
pokemon_hp('deoxysattack', 50).
pokemon_hp('deoxysdefense', 50).
pokemon_hp('deoxysspeed', 50).
pokemon_hp('turtwig', 55).
pokemon_hp('grotle', 75).
pokemon_hp('torterra', 95).
pokemon_hp('chimchar', 44).
pokemon_hp('monferno', 64).
pokemon_hp('infernape', 76).
pokemon_hp('piplup', 53).
pokemon_hp('prinplup', 64).
pokemon_hp('empoleon', 84).
pokemon_hp('starly', 40).
pokemon_hp('staravia', 55).
pokemon_hp('staraptor', 85).
pokemon_hp('bidoof', 59).
pokemon_hp('bibarel', 79).
pokemon_hp('kricketot', 37).
pokemon_hp('kricketune', 77).
pokemon_hp('shinx', 45).
pokemon_hp('luxio', 60).
pokemon_hp('luxray', 80).
pokemon_hp('budew', 40).
pokemon_hp('roserade', 60).
pokemon_hp('cranidos', 67).
pokemon_hp('rampardos', 97).
pokemon_hp('shieldon', 30).
pokemon_hp('bastiodon', 60).
pokemon_hp('burmy', 40).
pokemon_hp('burmysandy', 40).
pokemon_hp('burmytrash', 40).
pokemon_hp('wormadam', 60).
pokemon_hp('wormadamsandy', 60).
pokemon_hp('wormadamtrash', 60).
pokemon_hp('mothim', 70).
pokemon_hp('combee', 30).
pokemon_hp('vespiquen', 70).
pokemon_hp('pachirisu', 60).
pokemon_hp('buizel', 55).
pokemon_hp('floatzel', 85).
pokemon_hp('cherubi', 45).
pokemon_hp('cherrim', 70).
pokemon_hp('cherrimsunshine', 70).
pokemon_hp('shellos', 76).
pokemon_hp('shelloseast', 76).
pokemon_hp('gastrodon', 111).
pokemon_hp('gastrodoneast', 111).
pokemon_hp('ambipom', 75).
pokemon_hp('drifloon', 90).
pokemon_hp('drifblim', 150).
pokemon_hp('buneary', 55).
pokemon_hp('lopunny', 65).
pokemon_hp('lopunnymega', 65).
pokemon_hp('mismagius', 60).
pokemon_hp('honchkrow', 100).
pokemon_hp('glameow', 49).
pokemon_hp('purugly', 71).
pokemon_hp('chingling', 45).
pokemon_hp('stunky', 63).
pokemon_hp('skuntank', 103).
pokemon_hp('bronzor', 57).
pokemon_hp('bronzong', 67).
pokemon_hp('bonsly', 50).
pokemon_hp('mimejr', 20).
pokemon_hp('happiny', 100).
pokemon_hp('chatot', 76).
pokemon_hp('spiritomb', 50).
pokemon_hp('gible', 58).
pokemon_hp('gabite', 68).
pokemon_hp('garchomp', 108).
pokemon_hp('garchompmega', 108).
pokemon_hp('munchlax', 135).
pokemon_hp('riolu', 40).
pokemon_hp('lucario', 70).
pokemon_hp('lucariomega', 70).
pokemon_hp('hippopotas', 68).
pokemon_hp('hippowdon', 108).
pokemon_hp('skorupi', 40).
pokemon_hp('drapion', 70).
pokemon_hp('croagunk', 48).
pokemon_hp('toxicroak', 83).
pokemon_hp('carnivine', 74).
pokemon_hp('finneon', 49).
pokemon_hp('lumineon', 69).
pokemon_hp('mantyke', 45).
pokemon_hp('snover', 60).
pokemon_hp('abomasnow', 90).
pokemon_hp('abomasnowmega', 90).
pokemon_hp('weavile', 70).
pokemon_hp('magnezone', 70).
pokemon_hp('lickilicky', 110).
pokemon_hp('rhyperior', 115).
pokemon_hp('tangrowth', 100).
pokemon_hp('electivire', 75).
pokemon_hp('magmortar', 75).
pokemon_hp('togekiss', 85).
pokemon_hp('yanmega', 86).
pokemon_hp('leafeon', 65).
pokemon_hp('glaceon', 65).
pokemon_hp('gliscor', 75).
pokemon_hp('mamoswine', 110).
pokemon_hp('porygonz', 85).
pokemon_hp('gallade', 68).
pokemon_hp('gallademega', 68).
pokemon_hp('probopass', 60).
pokemon_hp('dusknoir', 45).
pokemon_hp('froslass', 70).
pokemon_hp('froslassmega', 0).
pokemon_hp('rotom', 50).
pokemon_hp('rotomheat', 50).
pokemon_hp('rotomwash', 50).
pokemon_hp('rotomfrost', 50).
pokemon_hp('rotomfan', 50).
pokemon_hp('rotommow', 50).
pokemon_hp('uxie', 75).
pokemon_hp('mesprit', 80).
pokemon_hp('azelf', 75).
pokemon_hp('dialga', 100).
pokemon_hp('dialgaorigin', 100).
pokemon_hp('palkia', 90).
pokemon_hp('palkiaorigin', 90).
pokemon_hp('heatran', 91).
pokemon_hp('regigigas', 110).
pokemon_hp('giratina', 150).
pokemon_hp('giratinaorigin', 150).
pokemon_hp('cresselia', 120).
pokemon_hp('phione', 80).
pokemon_hp('manaphy', 100).
pokemon_hp('darkrai', 70).
pokemon_hp('shaymin', 100).
pokemon_hp('shayminsky', 100).
pokemon_hp('arceus', 120).
pokemon_hp('arceusbug', 120).
pokemon_hp('arceusdark', 120).
pokemon_hp('arceusdragon', 120).
pokemon_hp('arceuselectric', 120).
pokemon_hp('arceusfairy', 120).
pokemon_hp('arceusfighting', 120).
pokemon_hp('arceusfire', 120).
pokemon_hp('arceusflying', 120).
pokemon_hp('arceusghost', 120).
pokemon_hp('arceusgrass', 120).
pokemon_hp('arceusground', 120).
pokemon_hp('arceusice', 120).
pokemon_hp('arceuspoison', 120).
pokemon_hp('arceuspsychic', 120).
pokemon_hp('arceusrock', 120).
pokemon_hp('arceussteel', 120).
pokemon_hp('arceuswater', 120).
pokemon_hp('victini', 100).
pokemon_hp('snivy', 45).
pokemon_hp('servine', 60).
pokemon_hp('serperior', 75).
pokemon_hp('tepig', 65).
pokemon_hp('pignite', 90).
pokemon_hp('emboar', 110).
pokemon_hp('emboarmega', 0).
pokemon_hp('oshawott', 55).
pokemon_hp('dewott', 75).
pokemon_hp('samurott', 95).
pokemon_hp('samurotthisui', 90).
pokemon_hp('patrat', 45).
pokemon_hp('watchog', 60).
pokemon_hp('lillipup', 45).
pokemon_hp('herdier', 65).
pokemon_hp('stoutland', 85).
pokemon_hp('purrloin', 41).
pokemon_hp('liepard', 64).
pokemon_hp('pansage', 50).
pokemon_hp('simisage', 75).
pokemon_hp('pansear', 50).
pokemon_hp('simisear', 75).
pokemon_hp('panpour', 50).
pokemon_hp('simipour', 75).
pokemon_hp('munna', 76).
pokemon_hp('musharna', 116).
pokemon_hp('pidove', 50).
pokemon_hp('tranquill', 62).
pokemon_hp('unfezant', 80).
pokemon_hp('blitzle', 45).
pokemon_hp('zebstrika', 75).
pokemon_hp('roggenrola', 55).
pokemon_hp('boldore', 70).
pokemon_hp('gigalith', 85).
pokemon_hp('woobat', 65).
pokemon_hp('swoobat', 67).
pokemon_hp('drilbur', 60).
pokemon_hp('excadrill', 110).
pokemon_hp('excadrillmega', 0).
pokemon_hp('audino', 103).
pokemon_hp('audinomega', 103).
pokemon_hp('timburr', 75).
pokemon_hp('gurdurr', 85).
pokemon_hp('conkeldurr', 105).
pokemon_hp('tympole', 50).
pokemon_hp('palpitoad', 75).
pokemon_hp('seismitoad', 105).
pokemon_hp('throh', 120).
pokemon_hp('sawk', 75).
pokemon_hp('sewaddle', 45).
pokemon_hp('swadloon', 55).
pokemon_hp('leavanny', 75).
pokemon_hp('venipede', 30).
pokemon_hp('whirlipede', 40).
pokemon_hp('scolipede', 60).
pokemon_hp('scolipedemega', 0).
pokemon_hp('cottonee', 40).
pokemon_hp('whimsicott', 60).
pokemon_hp('petilil', 45).
pokemon_hp('lilligant', 70).
pokemon_hp('lilliganthisui', 70).
pokemon_hp('basculin', 70).
pokemon_hp('basculinbluestriped', 70).
pokemon_hp('basculinwhitestriped', 70).
pokemon_hp('sandile', 50).
pokemon_hp('krokorok', 60).
pokemon_hp('krookodile', 95).
pokemon_hp('darumaka', 70).
pokemon_hp('darumakagalar', 70).
pokemon_hp('darmanitan', 105).
pokemon_hp('darmanitanzen', 105).
pokemon_hp('darmanitangalar', 105).
pokemon_hp('darmanitangalarzen', 105).
pokemon_hp('maractus', 75).
pokemon_hp('dwebble', 50).
pokemon_hp('crustle', 70).
pokemon_hp('scraggy', 50).
pokemon_hp('scrafty', 65).
pokemon_hp('scraftymega', 0).
pokemon_hp('sigilyph', 72).
pokemon_hp('yamask', 38).
pokemon_hp('yamaskgalar', 38).
pokemon_hp('cofagrigus', 58).
pokemon_hp('tirtouga', 54).
pokemon_hp('carracosta', 74).
pokemon_hp('archen', 55).
pokemon_hp('archeops', 75).
pokemon_hp('trubbish', 50).
pokemon_hp('garbodor', 80).
pokemon_hp('garbodorgmax', 80).
pokemon_hp('zorua', 40).
pokemon_hp('zoruahisui', 35).
pokemon_hp('zoroark', 60).
pokemon_hp('zoroarkhisui', 55).
pokemon_hp('minccino', 55).
pokemon_hp('cinccino', 75).
pokemon_hp('gothita', 45).
pokemon_hp('gothorita', 60).
pokemon_hp('gothitelle', 70).
pokemon_hp('solosis', 45).
pokemon_hp('duosion', 65).
pokemon_hp('reuniclus', 110).
pokemon_hp('ducklett', 62).
pokemon_hp('swanna', 75).
pokemon_hp('vanillite', 36).
pokemon_hp('vanillish', 51).
pokemon_hp('vanilluxe', 71).
pokemon_hp('deerling', 60).
pokemon_hp('deerlingsummer', 60).
pokemon_hp('deerlingautumn', 60).
pokemon_hp('deerlingwinter', 60).
pokemon_hp('sawsbuck', 80).
pokemon_hp('emolga', 55).
pokemon_hp('karrablast', 50).
pokemon_hp('escavalier', 70).
pokemon_hp('foongus', 69).
pokemon_hp('amoonguss', 114).
pokemon_hp('frillish', 55).
pokemon_hp('jellicent', 100).
pokemon_hp('alomomola', 165).
pokemon_hp('joltik', 50).
pokemon_hp('galvantula', 70).
pokemon_hp('ferroseed', 44).
pokemon_hp('ferrothorn', 74).
pokemon_hp('klink', 40).
pokemon_hp('klang', 60).
pokemon_hp('klinklang', 60).
pokemon_hp('tynamo', 35).
pokemon_hp('eelektrik', 65).
pokemon_hp('eelektross', 85).
pokemon_hp('eelektrossmega', 0).
pokemon_hp('elgyem', 55).
pokemon_hp('beheeyem', 75).
pokemon_hp('litwick', 50).
pokemon_hp('lampent', 60).
pokemon_hp('chandelure', 60).
pokemon_hp('chandeluremega', 0).
pokemon_hp('axew', 46).
pokemon_hp('fraxure', 66).
pokemon_hp('haxorus', 76).
pokemon_hp('cubchoo', 55).
pokemon_hp('beartic', 95).
pokemon_hp('cryogonal', 80).
pokemon_hp('shelmet', 50).
pokemon_hp('accelgor', 80).
pokemon_hp('stunfisk', 109).
pokemon_hp('stunfiskgalar', 109).
pokemon_hp('mienfoo', 45).
pokemon_hp('mienshao', 65).
pokemon_hp('druddigon', 77).
pokemon_hp('golett', 59).
pokemon_hp('golurk', 89).
pokemon_hp('pawniard', 45).
pokemon_hp('bisharp', 65).
pokemon_hp('bouffalant', 95).
pokemon_hp('rufflet', 70).
pokemon_hp('braviary', 100).
pokemon_hp('braviaryhisui', 110).
pokemon_hp('vullaby', 70).
pokemon_hp('mandibuzz', 110).
pokemon_hp('heatmor', 85).
pokemon_hp('durant', 58).
pokemon_hp('deino', 52).
pokemon_hp('zweilous', 72).
pokemon_hp('hydreigon', 92).
pokemon_hp('larvesta', 55).
pokemon_hp('volcarona', 85).
pokemon_hp('cobalion', 91).
pokemon_hp('terrakion', 91).
pokemon_hp('virizion', 91).
pokemon_hp('tornadus', 79).
pokemon_hp('tornadustherian', 79).
pokemon_hp('thundurus', 79).
pokemon_hp('thundurustherian', 79).
pokemon_hp('reshiram', 100).
pokemon_hp('zekrom', 100).
pokemon_hp('landorus', 89).
pokemon_hp('landorustherian', 89).
pokemon_hp('kyurem', 125).
pokemon_hp('kyuremblack', 125).
pokemon_hp('kyuremwhite', 125).
pokemon_hp('keldeo', 91).
pokemon_hp('keldeoresolute', 91).
pokemon_hp('meloetta', 100).
pokemon_hp('meloettapirouette', 100).
pokemon_hp('genesect', 71).
pokemon_hp('genesectdouse', 71).
pokemon_hp('genesectshock', 71).
pokemon_hp('genesectburn', 71).
pokemon_hp('genesectchill', 71).
pokemon_hp('chespin', 56).
pokemon_hp('quilladin', 61).
pokemon_hp('chesnaught', 88).
pokemon_hp('chesnaughtmega', 0).
pokemon_hp('fennekin', 40).
pokemon_hp('braixen', 59).
pokemon_hp('delphox', 75).
pokemon_hp('delphoxmega', 0).
pokemon_hp('froakie', 41).
pokemon_hp('frogadier', 54).
pokemon_hp('greninja', 72).
pokemon_hp('greninjabond', 72).
pokemon_hp('greninjaash', 72).
pokemon_hp('greninjamega', 0).
pokemon_hp('bunnelby', 38).
pokemon_hp('diggersby', 85).
pokemon_hp('fletchling', 45).
pokemon_hp('fletchinder', 62).
pokemon_hp('talonflame', 78).
pokemon_hp('scatterbug', 38).
pokemon_hp('spewpa', 45).
pokemon_hp('vivillon', 80).
pokemon_hp('vivillonicysnow', 80).
pokemon_hp('vivillonpolar', 80).
pokemon_hp('vivillontundra', 80).
pokemon_hp('vivilloncontinental', 80).
pokemon_hp('vivillongarden', 80).
pokemon_hp('vivillonelegant', 80).
pokemon_hp('vivillonmodern', 80).
pokemon_hp('vivillonmarine', 80).
pokemon_hp('vivillonarchipelago', 80).
pokemon_hp('vivillonhighplains', 80).
pokemon_hp('vivillonsandstorm', 80).
pokemon_hp('vivillonriver', 80).
pokemon_hp('vivillonmonsoon', 80).
pokemon_hp('vivillonsavanna', 80).
pokemon_hp('vivillonsun', 80).
pokemon_hp('vivillonocean', 80).
pokemon_hp('vivillonjungle', 80).
pokemon_hp('vivillonfancy', 80).
pokemon_hp('vivillonpokeball', 80).
pokemon_hp('litleo', 62).
pokemon_hp('pyroar', 86).
pokemon_hp('pyroarmega', 0).
pokemon_hp('flabebe', 44).
pokemon_hp('floette', 54).
pokemon_hp('floetteeternal', 74).
pokemon_hp('floettemega', 0).
pokemon_hp('florges', 78).
pokemon_hp('skiddo', 66).
pokemon_hp('gogoat', 123).
pokemon_hp('pancham', 67).
pokemon_hp('pangoro', 95).
pokemon_hp('furfrou', 75).
pokemon_hp('espurr', 62).
pokemon_hp('meowstic', 74).
pokemon_hp('meowsticf', 74).
pokemon_hp('honedge', 45).
pokemon_hp('doublade', 59).
pokemon_hp('aegislash', 60).
pokemon_hp('aegislashblade', 60).
pokemon_hp('spritzee', 78).
pokemon_hp('aromatisse', 101).
pokemon_hp('swirlix', 62).
pokemon_hp('slurpuff', 82).
pokemon_hp('inkay', 53).
pokemon_hp('malamar', 86).
pokemon_hp('malamarmega', 0).
pokemon_hp('binacle', 42).
pokemon_hp('barbaracle', 72).
pokemon_hp('barbaraclemega', 0).
pokemon_hp('skrelp', 50).
pokemon_hp('dragalge', 65).
pokemon_hp('dragalgemega', 0).
pokemon_hp('clauncher', 50).
pokemon_hp('clawitzer', 71).
pokemon_hp('helioptile', 44).
pokemon_hp('heliolisk', 62).
pokemon_hp('tyrunt', 58).
pokemon_hp('tyrantrum', 82).
pokemon_hp('amaura', 77).
pokemon_hp('aurorus', 123).
pokemon_hp('sylveon', 95).
pokemon_hp('hawlucha', 78).
pokemon_hp('hawluchamega', 0).
pokemon_hp('dedenne', 67).
pokemon_hp('carbink', 50).
pokemon_hp('goomy', 45).
pokemon_hp('sliggoo', 68).
pokemon_hp('sliggoohisui', 58).
pokemon_hp('goodra', 90).
pokemon_hp('goodrahisui', 80).
pokemon_hp('klefki', 57).
pokemon_hp('phantump', 43).
pokemon_hp('trevenant', 85).
pokemon_hp('pumpkaboo', 49).
pokemon_hp('pumpkaboosmall', 44).
pokemon_hp('pumpkaboolarge', 54).
pokemon_hp('pumpkaboosuper', 59).
pokemon_hp('gourgeist', 65).
pokemon_hp('gourgeistsmall', 55).
pokemon_hp('gourgeistlarge', 75).
pokemon_hp('gourgeistsuper', 85).
pokemon_hp('bergmite', 55).
pokemon_hp('avalugg', 95).
pokemon_hp('avalugghisui', 95).
pokemon_hp('noibat', 40).
pokemon_hp('noivern', 85).
pokemon_hp('xerneas', 126).
pokemon_hp('xerneasneutral', 126).
pokemon_hp('yveltal', 126).
pokemon_hp('zygarde', 108).
pokemon_hp('zygarde10', 54).
pokemon_hp('zygardecomplete', 216).
pokemon_hp('zygardemega', 0).
pokemon_hp('diancie', 50).
pokemon_hp('dianciemega', 50).
pokemon_hp('hoopa', 80).
pokemon_hp('hoopaunbound', 80).
pokemon_hp('volcanion', 80).
pokemon_hp('rowlet', 68).
pokemon_hp('dartrix', 78).
pokemon_hp('decidueye', 78).
pokemon_hp('decidueyehisui', 88).
pokemon_hp('litten', 45).
pokemon_hp('torracat', 65).
pokemon_hp('incineroar', 95).
pokemon_hp('popplio', 50).
pokemon_hp('brionne', 60).
pokemon_hp('primarina', 80).
pokemon_hp('pikipek', 35).
pokemon_hp('trumbeak', 55).
pokemon_hp('toucannon', 80).
pokemon_hp('yungoos', 48).
pokemon_hp('gumshoos', 88).
pokemon_hp('gumshoostotem', 88).
pokemon_hp('grubbin', 47).
pokemon_hp('charjabug', 57).
pokemon_hp('vikavolt', 77).
pokemon_hp('vikavolttotem', 77).
pokemon_hp('crabrawler', 47).
pokemon_hp('crabominable', 97).
pokemon_hp('oricorio', 75).
pokemon_hp('oricoriopompom', 75).
pokemon_hp('oricoriopau', 75).
pokemon_hp('oricoriosensu', 75).
pokemon_hp('cutiefly', 40).
pokemon_hp('ribombee', 60).
pokemon_hp('ribombeetotem', 60).
pokemon_hp('rockruff', 45).
pokemon_hp('rockruffdusk', 45).
pokemon_hp('lycanroc', 75).
pokemon_hp('lycanrocmidnight', 85).
pokemon_hp('lycanrocdusk', 75).
pokemon_hp('wishiwashi', 45).
pokemon_hp('wishiwashischool', 45).
pokemon_hp('mareanie', 50).
pokemon_hp('toxapex', 50).
pokemon_hp('mudbray', 70).
pokemon_hp('mudsdale', 100).
pokemon_hp('dewpider', 38).
pokemon_hp('araquanid', 68).
pokemon_hp('araquanidtotem', 68).
pokemon_hp('fomantis', 40).
pokemon_hp('lurantis', 70).
pokemon_hp('lurantistotem', 70).
pokemon_hp('morelull', 40).
pokemon_hp('shiinotic', 60).
pokemon_hp('salandit', 48).
pokemon_hp('salazzle', 68).
pokemon_hp('salazzletotem', 68).
pokemon_hp('stufful', 70).
pokemon_hp('bewear', 120).
pokemon_hp('bounsweet', 42).
pokemon_hp('steenee', 52).
pokemon_hp('tsareena', 72).
pokemon_hp('comfey', 51).
pokemon_hp('oranguru', 90).
pokemon_hp('passimian', 100).
pokemon_hp('wimpod', 25).
pokemon_hp('golisopod', 75).
pokemon_hp('sandygast', 55).
pokemon_hp('palossand', 85).
pokemon_hp('pyukumuku', 55).
pokemon_hp('typenull', 95).
pokemon_hp('silvally', 95).
pokemon_hp('silvallybug', 95).
pokemon_hp('silvallydark', 95).
pokemon_hp('silvallydragon', 95).
pokemon_hp('silvallyelectric', 95).
pokemon_hp('silvallyfairy', 95).
pokemon_hp('silvallyfighting', 95).
pokemon_hp('silvallyfire', 95).
pokemon_hp('silvallyflying', 95).
pokemon_hp('silvallyghost', 95).
pokemon_hp('silvallygrass', 95).
pokemon_hp('silvallyground', 95).
pokemon_hp('silvallyice', 95).
pokemon_hp('silvallypoison', 95).
pokemon_hp('silvallypsychic', 95).
pokemon_hp('silvallyrock', 95).
pokemon_hp('silvallysteel', 95).
pokemon_hp('silvallywater', 95).
pokemon_hp('minior', 60).
pokemon_hp('miniororange', 60).
pokemon_hp('minioryellow', 60).
pokemon_hp('miniorgreen', 60).
pokemon_hp('miniorblue', 60).
pokemon_hp('miniorindigo', 60).
pokemon_hp('miniorviolet', 60).
pokemon_hp('miniormeteor', 60).
pokemon_hp('komala', 65).
pokemon_hp('turtonator', 60).
pokemon_hp('togedemaru', 65).
pokemon_hp('togedemarutotem', 65).
pokemon_hp('mimikyu', 55).
pokemon_hp('mimikyubusted', 55).
pokemon_hp('mimikyutotem', 55).
pokemon_hp('mimikyubustedtotem', 55).
pokemon_hp('bruxish', 68).
pokemon_hp('drampa', 78).
pokemon_hp('drampamega', 0).
pokemon_hp('dhelmise', 70).
pokemon_hp('jangmoo', 45).
pokemon_hp('hakamoo', 55).
pokemon_hp('kommoo', 75).
pokemon_hp('kommoototem', 75).
pokemon_hp('tapukoko', 70).
pokemon_hp('tapulele', 70).
pokemon_hp('tapubulu', 70).
pokemon_hp('tapufini', 70).
pokemon_hp('cosmog', 43).
pokemon_hp('cosmoem', 43).
pokemon_hp('solgaleo', 137).
pokemon_hp('lunala', 137).
pokemon_hp('nihilego', 109).
pokemon_hp('buzzwole', 107).
pokemon_hp('pheromosa', 71).
pokemon_hp('xurkitree', 83).
pokemon_hp('celesteela', 97).
pokemon_hp('kartana', 59).
pokemon_hp('guzzlord', 223).
pokemon_hp('necrozma', 97).
pokemon_hp('necrozmaduskmane', 97).
pokemon_hp('necrozmadawnwings', 97).
pokemon_hp('necrozmaultra', 97).
pokemon_hp('magearna', 80).
pokemon_hp('magearnaoriginal', 80).
pokemon_hp('marshadow', 90).
pokemon_hp('poipole', 67).
pokemon_hp('naganadel', 73).
pokemon_hp('stakataka', 61).
pokemon_hp('blacephalon', 53).
pokemon_hp('zeraora', 88).
pokemon_hp('meltan', 46).
pokemon_hp('melmetal', 135).
pokemon_hp('melmetalgmax', 135).
pokemon_hp('grookey', 50).
pokemon_hp('thwackey', 70).
pokemon_hp('rillaboom', 100).
pokemon_hp('rillaboomgmax', 100).
pokemon_hp('scorbunny', 50).
pokemon_hp('raboot', 65).
pokemon_hp('cinderace', 80).
pokemon_hp('cinderacegmax', 80).
pokemon_hp('sobble', 50).
pokemon_hp('drizzile', 65).
pokemon_hp('inteleon', 70).
pokemon_hp('inteleongmax', 70).
pokemon_hp('skwovet', 70).
pokemon_hp('greedent', 120).
pokemon_hp('rookidee', 38).
pokemon_hp('corvisquire', 68).
pokemon_hp('corviknight', 98).
pokemon_hp('corviknightgmax', 98).
pokemon_hp('blipbug', 25).
pokemon_hp('dottler', 50).
pokemon_hp('orbeetle', 60).
pokemon_hp('orbeetlegmax', 60).
pokemon_hp('nickit', 40).
pokemon_hp('thievul', 70).
pokemon_hp('gossifleur', 40).
pokemon_hp('eldegoss', 60).
pokemon_hp('wooloo', 42).
pokemon_hp('dubwool', 72).
pokemon_hp('chewtle', 50).
pokemon_hp('drednaw', 90).
pokemon_hp('drednawgmax', 90).
pokemon_hp('yamper', 59).
pokemon_hp('boltund', 69).
pokemon_hp('rolycoly', 30).
pokemon_hp('carkol', 80).
pokemon_hp('coalossal', 110).
pokemon_hp('coalossalgmax', 110).
pokemon_hp('applin', 40).
pokemon_hp('flapple', 70).
pokemon_hp('flapplegmax', 70).
pokemon_hp('appletun', 110).
pokemon_hp('appletungmax', 110).
pokemon_hp('silicobra', 52).
pokemon_hp('sandaconda', 72).
pokemon_hp('sandacondagmax', 72).
pokemon_hp('cramorant', 70).
pokemon_hp('cramorantgulping', 70).
pokemon_hp('cramorantgorging', 70).
pokemon_hp('arrokuda', 41).
pokemon_hp('barraskewda', 61).
pokemon_hp('toxel', 40).
pokemon_hp('toxtricity', 75).
pokemon_hp('toxtricitylowkey', 75).
pokemon_hp('toxtricitygmax', 75).
pokemon_hp('toxtricitylowkeygmax', 75).
pokemon_hp('sizzlipede', 50).
pokemon_hp('centiskorch', 100).
pokemon_hp('centiskorchgmax', 100).
pokemon_hp('clobbopus', 50).
pokemon_hp('grapploct', 80).
pokemon_hp('sinistea', 40).
pokemon_hp('sinisteaantique', 40).
pokemon_hp('polteageist', 60).
pokemon_hp('polteageistantique', 60).
pokemon_hp('hatenna', 42).
pokemon_hp('hattrem', 57).
pokemon_hp('hatterene', 57).
pokemon_hp('hatterenegmax', 57).
pokemon_hp('impidimp', 45).
pokemon_hp('morgrem', 65).
pokemon_hp('grimmsnarl', 95).
pokemon_hp('grimmsnarlgmax', 95).
pokemon_hp('obstagoon', 93).
pokemon_hp('perrserker', 70).
pokemon_hp('cursola', 60).
pokemon_hp('sirfetchd', 62).
pokemon_hp('mrrime', 80).
pokemon_hp('runerigus', 58).
pokemon_hp('milcery', 45).
pokemon_hp('alcremie', 65).
pokemon_hp('alcremierubycream', 65).
pokemon_hp('alcremiematchacream', 65).
pokemon_hp('alcremiemintcream', 65).
pokemon_hp('alcremielemoncream', 65).
pokemon_hp('alcremierubyswirl', 65).
pokemon_hp('alcremiecaramelswirl', 65).
pokemon_hp('alcremierainbowswirl', 65).
pokemon_hp('alcremiegmax', 65).
pokemon_hp('falinks', 65).
pokemon_hp('falinksmega', 0).
pokemon_hp('pincurchin', 48).
pokemon_hp('snom', 30).
pokemon_hp('frosmoth', 70).
pokemon_hp('stonjourner', 100).
pokemon_hp('eiscue', 75).
pokemon_hp('eiscuenoice', 75).
pokemon_hp('indeedee', 60).
pokemon_hp('indeedeef', 70).
pokemon_hp('morpeko', 58).
pokemon_hp('morpekohangry', 58).
pokemon_hp('cufant', 72).
pokemon_hp('copperajah', 122).
pokemon_hp('copperajahgmax', 122).
pokemon_hp('dracozolt', 90).
pokemon_hp('arctozolt', 90).
pokemon_hp('dracovish', 90).
pokemon_hp('arctovish', 90).
pokemon_hp('duraludon', 70).
pokemon_hp('duraludongmax', 70).
pokemon_hp('dreepy', 28).
pokemon_hp('drakloak', 68).
pokemon_hp('dragapult', 88).
pokemon_hp('zacian', 92).
pokemon_hp('zaciancrowned', 92).
pokemon_hp('zamazenta', 92).
pokemon_hp('zamazentacrowned', 92).
pokemon_hp('eternatus', 140).
pokemon_hp('eternatuseternamax', 255).
pokemon_hp('kubfu', 60).
pokemon_hp('urshifu', 100).
pokemon_hp('urshifurapidstrike', 100).
pokemon_hp('urshifugmax', 100).
pokemon_hp('urshifurapidstrikegmax', 100).
pokemon_hp('zarude', 105).
pokemon_hp('zarudedada', 105).
pokemon_hp('regieleki', 80).
pokemon_hp('regidrago', 200).
pokemon_hp('glastrier', 100).
pokemon_hp('spectrier', 100).
pokemon_hp('calyrex', 100).
pokemon_hp('calyrexice', 100).
pokemon_hp('calyrexshadow', 100).
pokemon_hp('wyrdeer', 103).
pokemon_hp('kleavor', 70).
pokemon_hp('ursaluna', 130).
pokemon_hp('ursalunabloodmoon', 113).
pokemon_hp('basculegion', 120).
pokemon_hp('basculegionf', 120).
pokemon_hp('sneasler', 80).
pokemon_hp('overqwil', 85).
pokemon_hp('enamorus', 74).
pokemon_hp('enamorustherian', 74).
pokemon_hp('sprigatito', 40).
pokemon_hp('floragato', 61).
pokemon_hp('meowscarada', 76).
pokemon_hp('fuecoco', 67).
pokemon_hp('crocalor', 81).
pokemon_hp('skeledirge', 104).
pokemon_hp('quaxly', 55).
pokemon_hp('quaxwell', 70).
pokemon_hp('quaquaval', 85).
pokemon_hp('lechonk', 54).
pokemon_hp('oinkologne', 110).
pokemon_hp('oinkolognef', 115).
pokemon_hp('tarountula', 35).
pokemon_hp('spidops', 60).
pokemon_hp('nymble', 33).
pokemon_hp('lokix', 71).
pokemon_hp('pawmi', 45).
pokemon_hp('pawmo', 60).
pokemon_hp('pawmot', 70).
pokemon_hp('tandemaus', 50).
pokemon_hp('maushold', 74).
pokemon_hp('mausholdfour', 74).
pokemon_hp('fidough', 37).
pokemon_hp('dachsbun', 57).
pokemon_hp('smoliv', 41).
pokemon_hp('dolliv', 52).
pokemon_hp('arboliva', 78).
pokemon_hp('squawkabilly', 82).
pokemon_hp('squawkabillyblue', 82).
pokemon_hp('squawkabillyyellow', 82).
pokemon_hp('squawkabillywhite', 82).
pokemon_hp('nacli', 55).
pokemon_hp('naclstack', 60).
pokemon_hp('garganacl', 100).
pokemon_hp('charcadet', 40).
pokemon_hp('armarouge', 85).
pokemon_hp('ceruledge', 75).
pokemon_hp('tadbulb', 61).
pokemon_hp('bellibolt', 109).
pokemon_hp('wattrel', 40).
pokemon_hp('kilowattrel', 70).
pokemon_hp('maschiff', 60).
pokemon_hp('mabosstiff', 80).
pokemon_hp('shroodle', 40).
pokemon_hp('grafaiai', 63).
pokemon_hp('bramblin', 40).
pokemon_hp('brambleghast', 55).
pokemon_hp('toedscool', 40).
pokemon_hp('toedscruel', 80).
pokemon_hp('klawf', 70).
pokemon_hp('capsakid', 50).
pokemon_hp('scovillain', 65).
pokemon_hp('rellor', 41).
pokemon_hp('rabsca', 75).
pokemon_hp('flittle', 30).
pokemon_hp('espathra', 95).
pokemon_hp('tinkatink', 50).
pokemon_hp('tinkatuff', 65).
pokemon_hp('tinkaton', 85).
pokemon_hp('wiglett', 10).
pokemon_hp('wugtrio', 35).
pokemon_hp('bombirdier', 70).
pokemon_hp('finizen', 70).
pokemon_hp('palafin', 100).
pokemon_hp('palafinhero', 100).
pokemon_hp('varoom', 45).
pokemon_hp('revavroom', 80).
pokemon_hp('cyclizar', 70).
pokemon_hp('orthworm', 70).
pokemon_hp('glimmet', 48).
pokemon_hp('glimmora', 83).
pokemon_hp('greavard', 50).
pokemon_hp('houndstone', 72).
pokemon_hp('flamigo', 82).
pokemon_hp('cetoddle', 108).
pokemon_hp('cetitan', 170).
pokemon_hp('veluza', 90).
pokemon_hp('dondozo', 150).
pokemon_hp('tatsugiri', 68).
pokemon_hp('tatsugiridroopy', 68).
pokemon_hp('tatsugiristretchy', 68).
pokemon_hp('annihilape', 110).
pokemon_hp('clodsire', 130).
pokemon_hp('farigiraf', 120).
pokemon_hp('dudunsparce', 125).
pokemon_hp('dudunsparcethreesegment', 125).
pokemon_hp('kingambit', 100).
pokemon_hp('greattusk', 115).
pokemon_hp('screamtail', 115).
pokemon_hp('brutebonnet', 111).
pokemon_hp('fluttermane', 55).
pokemon_hp('slitherwing', 85).
pokemon_hp('sandyshocks', 85).
pokemon_hp('irontreads', 90).
pokemon_hp('ironbundle', 56).
pokemon_hp('ironhands', 154).
pokemon_hp('ironjugulis', 94).
pokemon_hp('ironmoth', 80).
pokemon_hp('ironthorns', 100).
pokemon_hp('frigibax', 65).
pokemon_hp('arctibax', 90).
pokemon_hp('baxcalibur', 115).
pokemon_hp('gimmighoul', 45).
pokemon_hp('gimmighoulroaming', 45).
pokemon_hp('gholdengo', 87).
pokemon_hp('wochien', 85).
pokemon_hp('chienpao', 80).
pokemon_hp('tinglu', 155).
pokemon_hp('chiyu', 55).
pokemon_hp('roaringmoon', 105).
pokemon_hp('ironvaliant', 74).
pokemon_hp('koraidon', 100).
pokemon_hp('miraidon', 100).
pokemon_hp('walkingwake', 99).
pokemon_hp('ironleaves', 90).
pokemon_hp('dipplin', 80).
pokemon_hp('poltchageist', 40).
pokemon_hp('poltchageistartisan', 40).
pokemon_hp('sinistcha', 71).
pokemon_hp('sinistchamasterpiece', 71).
pokemon_hp('okidogi', 88).
pokemon_hp('munkidori', 88).
pokemon_hp('fezandipiti', 88).
pokemon_hp('ogerpon', 80).
pokemon_hp('ogerponwellspring', 80).
pokemon_hp('ogerponhearthflame', 80).
pokemon_hp('ogerponcornerstone', 80).
pokemon_hp('ogerpontealtera', 80).
pokemon_hp('ogerponwellspringtera', 80).
pokemon_hp('ogerponhearthflametera', 80).
pokemon_hp('ogerponcornerstonetera', 80).
pokemon_hp('archaludon', 90).
pokemon_hp('hydrapple', 106).
pokemon_hp('gougingfire', 105).
pokemon_hp('ragingbolt', 125).
pokemon_hp('ironboulder', 90).
pokemon_hp('ironcrown', 90).
pokemon_hp('terapagos', 90).
pokemon_hp('terapagosterastal', 95).
pokemon_hp('terapagosstellar', 160).
pokemon_hp('pecharunt', 88).
pokemon_hp('missingno', 33).
pokemon_hp('ramnarok', 0).
pokemon_hp('ramnarokradiant', 0).
pokemon_hp('pokestarsmeargle', 55).
pokemon_hp('pokestarufo', 100).
pokemon_hp('pokestarufo2', 100).
pokemon_hp('pokestarbrycenman', 100).
pokemon_hp('pokestarmt', 100).
pokemon_hp('pokestarmt2', 100).
pokemon_hp('pokestartransport', 100).
pokemon_hp('pokestargiant', 100).
pokemon_hp('pokestarhumanoid', 100).
pokemon_hp('pokestarmonster', 100).
pokemon_hp('pokestarf00', 100).
pokemon_hp('pokestarf002', 100).
pokemon_hp('pokestarspirit', 100).
pokemon_hp('pokestarblackdoor', 100).
pokemon_hp('pokestarwhitedoor', 100).
pokemon_hp('pokestarblackbelt', 100).
pokemon_hp('pokestarufopropu2', 100).
pokemon_atk('bulbasaur', 49).
pokemon_atk('ivysaur', 62).
pokemon_atk('venusaur', 82).
pokemon_atk('venusaurmega', 100).
pokemon_atk('venusaurgmax', 82).
pokemon_atk('charmander', 52).
pokemon_atk('charmeleon', 64).
pokemon_atk('charizard', 84).
pokemon_atk('charizardmegax', 130).
pokemon_atk('charizardmegay', 104).
pokemon_atk('charizardgmax', 84).
pokemon_atk('squirtle', 48).
pokemon_atk('wartortle', 63).
pokemon_atk('blastoise', 83).
pokemon_atk('blastoisemega', 103).
pokemon_atk('blastoisegmax', 83).
pokemon_atk('caterpie', 30).
pokemon_atk('metapod', 20).
pokemon_atk('butterfree', 45).
pokemon_atk('butterfreegmax', 45).
pokemon_atk('weedle', 35).
pokemon_atk('kakuna', 25).
pokemon_atk('beedrill', 90).
pokemon_atk('beedrillmega', 150).
pokemon_atk('pidgey', 45).
pokemon_atk('pidgeotto', 60).
pokemon_atk('pidgeot', 80).
pokemon_atk('pidgeotmega', 80).
pokemon_atk('rattata', 56).
pokemon_atk('rattataalola', 56).
pokemon_atk('raticate', 81).
pokemon_atk('raticatealola', 71).
pokemon_atk('raticatealolatotem', 71).
pokemon_atk('spearow', 60).
pokemon_atk('fearow', 90).
pokemon_atk('ekans', 60).
pokemon_atk('arbok', 95).
pokemon_atk('pikachu', 55).
pokemon_atk('pikachucosplay', 55).
pokemon_atk('pikachurockstar', 55).
pokemon_atk('pikachubelle', 55).
pokemon_atk('pikachupopstar', 55).
pokemon_atk('pikachuphd', 55).
pokemon_atk('pikachulibre', 55).
pokemon_atk('pikachuoriginal', 55).
pokemon_atk('pikachuhoenn', 55).
pokemon_atk('pikachusinnoh', 55).
pokemon_atk('pikachuunova', 55).
pokemon_atk('pikachukalos', 55).
pokemon_atk('pikachualola', 55).
pokemon_atk('pikachupartner', 55).
pokemon_atk('pikachustarter', 80).
pokemon_atk('pikachugmax', 55).
pokemon_atk('pikachuworld', 55).
pokemon_atk('raichu', 90).
pokemon_atk('raichualola', 85).
pokemon_atk('sandshrew', 75).
pokemon_atk('sandshrewalola', 75).
pokemon_atk('sandslash', 100).
pokemon_atk('sandslashalola', 100).
pokemon_atk('nidoranf', 47).
pokemon_atk('nidorina', 62).
pokemon_atk('nidoqueen', 92).
pokemon_atk('nidoranm', 57).
pokemon_atk('nidorino', 72).
pokemon_atk('nidoking', 102).
pokemon_atk('clefairy', 45).
pokemon_atk('clefable', 70).
pokemon_atk('clefablemega', 0).
pokemon_atk('vulpix', 41).
pokemon_atk('vulpixalola', 41).
pokemon_atk('ninetales', 76).
pokemon_atk('ninetalesalola', 67).
pokemon_atk('jigglypuff', 45).
pokemon_atk('wigglytuff', 70).
pokemon_atk('zubat', 45).
pokemon_atk('golbat', 80).
pokemon_atk('oddish', 50).
pokemon_atk('gloom', 65).
pokemon_atk('vileplume', 80).
pokemon_atk('paras', 70).
pokemon_atk('parasect', 95).
pokemon_atk('venonat', 55).
pokemon_atk('venomoth', 65).
pokemon_atk('diglett', 55).
pokemon_atk('diglettalola', 55).
pokemon_atk('dugtrio', 100).
pokemon_atk('dugtrioalola', 100).
pokemon_atk('meowth', 45).
pokemon_atk('meowthalola', 35).
pokemon_atk('meowthgalar', 65).
pokemon_atk('meowthgmax', 45).
pokemon_atk('persian', 70).
pokemon_atk('persianalola', 60).
pokemon_atk('psyduck', 52).
pokemon_atk('golduck', 82).
pokemon_atk('mankey', 80).
pokemon_atk('primeape', 105).
pokemon_atk('growlithe', 70).
pokemon_atk('growlithehisui', 75).
pokemon_atk('arcanine', 110).
pokemon_atk('arcaninehisui', 115).
pokemon_atk('poliwag', 50).
pokemon_atk('poliwhirl', 65).
pokemon_atk('poliwrath', 95).
pokemon_atk('abra', 20).
pokemon_atk('kadabra', 35).
pokemon_atk('alakazam', 50).
pokemon_atk('alakazammega', 50).
pokemon_atk('machop', 80).
pokemon_atk('machoke', 100).
pokemon_atk('machamp', 130).
pokemon_atk('machampgmax', 130).
pokemon_atk('bellsprout', 75).
pokemon_atk('weepinbell', 90).
pokemon_atk('victreebel', 105).
pokemon_atk('victreebelmega', 0).
pokemon_atk('tentacool', 40).
pokemon_atk('tentacruel', 70).
pokemon_atk('geodude', 80).
pokemon_atk('geodudealola', 80).
pokemon_atk('graveler', 95).
pokemon_atk('graveleralola', 95).
pokemon_atk('golem', 120).
pokemon_atk('golemalola', 120).
pokemon_atk('ponyta', 85).
pokemon_atk('ponytagalar', 85).
pokemon_atk('rapidash', 100).
pokemon_atk('rapidashgalar', 100).
pokemon_atk('slowpoke', 65).
pokemon_atk('slowpokegalar', 65).
pokemon_atk('slowbro', 75).
pokemon_atk('slowbromega', 75).
pokemon_atk('slowbrogalar', 100).
pokemon_atk('magnemite', 35).
pokemon_atk('magneton', 60).
pokemon_atk('farfetchd', 90).
pokemon_atk('farfetchdgalar', 95).
pokemon_atk('doduo', 85).
pokemon_atk('dodrio', 110).
pokemon_atk('seel', 45).
pokemon_atk('dewgong', 70).
pokemon_atk('grimer', 80).
pokemon_atk('grimeralola', 80).
pokemon_atk('muk', 105).
pokemon_atk('mukalola', 105).
pokemon_atk('shellder', 65).
pokemon_atk('cloyster', 95).
pokemon_atk('gastly', 35).
pokemon_atk('haunter', 50).
pokemon_atk('gengar', 65).
pokemon_atk('gengarmega', 65).
pokemon_atk('gengargmax', 65).
pokemon_atk('onix', 45).
pokemon_atk('drowzee', 48).
pokemon_atk('hypno', 73).
pokemon_atk('krabby', 105).
pokemon_atk('kingler', 130).
pokemon_atk('kinglergmax', 130).
pokemon_atk('voltorb', 30).
pokemon_atk('voltorbhisui', 30).
pokemon_atk('electrode', 50).
pokemon_atk('electrodehisui', 50).
pokemon_atk('exeggcute', 40).
pokemon_atk('exeggutor', 95).
pokemon_atk('exeggutoralola', 105).
pokemon_atk('cubone', 50).
pokemon_atk('marowak', 80).
pokemon_atk('marowakalola', 80).
pokemon_atk('marowakalolatotem', 80).
pokemon_atk('hitmonlee', 120).
pokemon_atk('hitmonchan', 105).
pokemon_atk('lickitung', 55).
pokemon_atk('koffing', 65).
pokemon_atk('weezing', 90).
pokemon_atk('weezinggalar', 90).
pokemon_atk('rhyhorn', 85).
pokemon_atk('rhydon', 130).
pokemon_atk('chansey', 5).
pokemon_atk('tangela', 55).
pokemon_atk('kangaskhan', 95).
pokemon_atk('kangaskhanmega', 125).
pokemon_atk('horsea', 40).
pokemon_atk('seadra', 65).
pokemon_atk('goldeen', 67).
pokemon_atk('seaking', 92).
pokemon_atk('staryu', 45).
pokemon_atk('starmie', 75).
pokemon_atk('starmiemega', 0).
pokemon_atk('mrmime', 45).
pokemon_atk('mrmimegalar', 65).
pokemon_atk('scyther', 110).
pokemon_atk('jynx', 50).
pokemon_atk('electabuzz', 83).
pokemon_atk('magmar', 95).
pokemon_atk('pinsir', 125).
pokemon_atk('pinsirmega', 155).
pokemon_atk('tauros', 100).
pokemon_atk('taurospaldeacombat', 110).
pokemon_atk('taurospaldeablaze', 110).
pokemon_atk('taurospaldeaaqua', 110).
pokemon_atk('magikarp', 10).
pokemon_atk('gyarados', 125).
pokemon_atk('gyaradosmega', 155).
pokemon_atk('lapras', 85).
pokemon_atk('laprasgmax', 85).
pokemon_atk('ditto', 48).
pokemon_atk('eevee', 55).
pokemon_atk('eeveestarter', 75).
pokemon_atk('eeveegmax', 55).
pokemon_atk('vaporeon', 65).
pokemon_atk('jolteon', 65).
pokemon_atk('flareon', 130).
pokemon_atk('porygon', 60).
pokemon_atk('omanyte', 40).
pokemon_atk('omastar', 60).
pokemon_atk('kabuto', 80).
pokemon_atk('kabutops', 115).
pokemon_atk('aerodactyl', 105).
pokemon_atk('aerodactylmega', 135).
pokemon_atk('snorlax', 110).
pokemon_atk('snorlaxgmax', 110).
pokemon_atk('articuno', 85).
pokemon_atk('articunogalar', 85).
pokemon_atk('zapdos', 90).
pokemon_atk('zapdosgalar', 125).
pokemon_atk('moltres', 100).
pokemon_atk('moltresgalar', 85).
pokemon_atk('dratini', 64).
pokemon_atk('dragonair', 84).
pokemon_atk('dragonite', 134).
pokemon_atk('dragonitemega', 0).
pokemon_atk('mewtwo', 110).
pokemon_atk('mewtwomegax', 190).
pokemon_atk('mewtwomegay', 150).
pokemon_atk('mew', 100).
pokemon_atk('chikorita', 49).
pokemon_atk('bayleef', 62).
pokemon_atk('meganium', 82).
pokemon_atk('meganiummega', 0).
pokemon_atk('cyndaquil', 52).
pokemon_atk('quilava', 64).
pokemon_atk('typhlosion', 84).
pokemon_atk('typhlosionhisui', 84).
pokemon_atk('totodile', 65).
pokemon_atk('croconaw', 80).
pokemon_atk('feraligatr', 105).
pokemon_atk('feraligatrmega', 0).
pokemon_atk('sentret', 46).
pokemon_atk('furret', 76).
pokemon_atk('hoothoot', 30).
pokemon_atk('noctowl', 50).
pokemon_atk('ledyba', 20).
pokemon_atk('ledian', 35).
pokemon_atk('spinarak', 60).
pokemon_atk('ariados', 90).
pokemon_atk('crobat', 90).
pokemon_atk('chinchou', 38).
pokemon_atk('lanturn', 58).
pokemon_atk('pichu', 40).
pokemon_atk('pichuspikyeared', 40).
pokemon_atk('cleffa', 25).
pokemon_atk('igglybuff', 30).
pokemon_atk('togepi', 20).
pokemon_atk('togetic', 40).
pokemon_atk('natu', 50).
pokemon_atk('xatu', 75).
pokemon_atk('mareep', 40).
pokemon_atk('flaaffy', 55).
pokemon_atk('ampharos', 75).
pokemon_atk('ampharosmega', 95).
pokemon_atk('bellossom', 80).
pokemon_atk('marill', 20).
pokemon_atk('azumarill', 50).
pokemon_atk('sudowoodo', 100).
pokemon_atk('politoed', 75).
pokemon_atk('hoppip', 35).
pokemon_atk('skiploom', 45).
pokemon_atk('jumpluff', 55).
pokemon_atk('aipom', 70).
pokemon_atk('sunkern', 30).
pokemon_atk('sunflora', 75).
pokemon_atk('yanma', 65).
pokemon_atk('wooper', 45).
pokemon_atk('wooperpaldea', 45).
pokemon_atk('quagsire', 85).
pokemon_atk('espeon', 65).
pokemon_atk('umbreon', 65).
pokemon_atk('murkrow', 85).
pokemon_atk('slowking', 75).
pokemon_atk('slowkinggalar', 65).
pokemon_atk('misdreavus', 60).
pokemon_atk('unown', 72).
pokemon_atk('wobbuffet', 33).
pokemon_atk('girafarig', 80).
pokemon_atk('pineco', 65).
pokemon_atk('forretress', 90).
pokemon_atk('dunsparce', 70).
pokemon_atk('gligar', 75).
pokemon_atk('steelix', 85).
pokemon_atk('steelixmega', 125).
pokemon_atk('snubbull', 80).
pokemon_atk('granbull', 120).
pokemon_atk('qwilfish', 95).
pokemon_atk('qwilfishhisui', 95).
pokemon_atk('scizor', 130).
pokemon_atk('scizormega', 150).
pokemon_atk('shuckle', 10).
pokemon_atk('heracross', 125).
pokemon_atk('heracrossmega', 185).
pokemon_atk('sneasel', 95).
pokemon_atk('sneaselhisui', 95).
pokemon_atk('teddiursa', 80).
pokemon_atk('ursaring', 130).
pokemon_atk('slugma', 40).
pokemon_atk('magcargo', 50).
pokemon_atk('swinub', 50).
pokemon_atk('piloswine', 100).
pokemon_atk('corsola', 55).
pokemon_atk('corsolagalar', 55).
pokemon_atk('remoraid', 65).
pokemon_atk('octillery', 105).
pokemon_atk('delibird', 55).
pokemon_atk('mantine', 40).
pokemon_atk('skarmory', 80).
pokemon_atk('skarmorymega', 0).
pokemon_atk('houndour', 60).
pokemon_atk('houndoom', 90).
pokemon_atk('houndoommega', 90).
pokemon_atk('kingdra', 95).
pokemon_atk('phanpy', 60).
pokemon_atk('donphan', 120).
pokemon_atk('porygon2', 80).
pokemon_atk('stantler', 95).
pokemon_atk('smeargle', 20).
pokemon_atk('tyrogue', 35).
pokemon_atk('hitmontop', 95).
pokemon_atk('smoochum', 30).
pokemon_atk('elekid', 63).
pokemon_atk('magby', 75).
pokemon_atk('miltank', 80).
pokemon_atk('blissey', 10).
pokemon_atk('raikou', 85).
pokemon_atk('entei', 115).
pokemon_atk('suicune', 75).
pokemon_atk('larvitar', 64).
pokemon_atk('pupitar', 84).
pokemon_atk('tyranitar', 134).
pokemon_atk('tyranitarmega', 164).
pokemon_atk('lugia', 90).
pokemon_atk('hooh', 130).
pokemon_atk('celebi', 100).
pokemon_atk('treecko', 45).
pokemon_atk('grovyle', 65).
pokemon_atk('sceptile', 85).
pokemon_atk('sceptilemega', 110).
pokemon_atk('torchic', 60).
pokemon_atk('combusken', 85).
pokemon_atk('blaziken', 120).
pokemon_atk('blazikenmega', 160).
pokemon_atk('mudkip', 70).
pokemon_atk('marshtomp', 85).
pokemon_atk('swampert', 110).
pokemon_atk('swampertmega', 150).
pokemon_atk('poochyena', 55).
pokemon_atk('mightyena', 90).
pokemon_atk('zigzagoon', 30).
pokemon_atk('zigzagoongalar', 30).
pokemon_atk('linoone', 70).
pokemon_atk('linoonegalar', 70).
pokemon_atk('wurmple', 45).
pokemon_atk('silcoon', 35).
pokemon_atk('beautifly', 70).
pokemon_atk('cascoon', 35).
pokemon_atk('dustox', 50).
pokemon_atk('lotad', 30).
pokemon_atk('lombre', 50).
pokemon_atk('ludicolo', 70).
pokemon_atk('seedot', 40).
pokemon_atk('nuzleaf', 70).
pokemon_atk('shiftry', 100).
pokemon_atk('taillow', 55).
pokemon_atk('swellow', 85).
pokemon_atk('wingull', 30).
pokemon_atk('pelipper', 50).
pokemon_atk('ralts', 25).
pokemon_atk('kirlia', 35).
pokemon_atk('gardevoir', 65).
pokemon_atk('gardevoirmega', 85).
pokemon_atk('surskit', 30).
pokemon_atk('masquerain', 60).
pokemon_atk('shroomish', 40).
pokemon_atk('breloom', 130).
pokemon_atk('slakoth', 60).
pokemon_atk('vigoroth', 80).
pokemon_atk('slaking', 160).
pokemon_atk('nincada', 45).
pokemon_atk('ninjask', 90).
pokemon_atk('shedinja', 90).
pokemon_atk('whismur', 51).
pokemon_atk('loudred', 71).
pokemon_atk('exploud', 91).
pokemon_atk('makuhita', 60).
pokemon_atk('hariyama', 120).
pokemon_atk('azurill', 20).
pokemon_atk('nosepass', 45).
pokemon_atk('skitty', 45).
pokemon_atk('delcatty', 65).
pokemon_atk('sableye', 75).
pokemon_atk('sableyemega', 85).
pokemon_atk('mawile', 85).
pokemon_atk('mawilemega', 105).
pokemon_atk('aron', 70).
pokemon_atk('lairon', 90).
pokemon_atk('aggron', 110).
pokemon_atk('aggronmega', 140).
pokemon_atk('meditite', 40).
pokemon_atk('medicham', 60).
pokemon_atk('medichammega', 100).
pokemon_atk('electrike', 45).
pokemon_atk('manectric', 75).
pokemon_atk('manectricmega', 75).
pokemon_atk('plusle', 50).
pokemon_atk('minun', 40).
pokemon_atk('volbeat', 73).
pokemon_atk('illumise', 47).
pokemon_atk('roselia', 60).
pokemon_atk('gulpin', 43).
pokemon_atk('swalot', 73).
pokemon_atk('carvanha', 90).
pokemon_atk('sharpedo', 120).
pokemon_atk('sharpedomega', 140).
pokemon_atk('wailmer', 70).
pokemon_atk('wailord', 90).
pokemon_atk('numel', 60).
pokemon_atk('camerupt', 100).
pokemon_atk('cameruptmega', 120).
pokemon_atk('torkoal', 85).
pokemon_atk('spoink', 25).
pokemon_atk('grumpig', 45).
pokemon_atk('spinda', 60).
pokemon_atk('trapinch', 100).
pokemon_atk('vibrava', 70).
pokemon_atk('flygon', 100).
pokemon_atk('cacnea', 85).
pokemon_atk('cacturne', 115).
pokemon_atk('swablu', 40).
pokemon_atk('altaria', 70).
pokemon_atk('altariamega', 110).
pokemon_atk('zangoose', 115).
pokemon_atk('seviper', 100).
pokemon_atk('lunatone', 55).
pokemon_atk('solrock', 95).
pokemon_atk('barboach', 48).
pokemon_atk('whiscash', 78).
pokemon_atk('corphish', 80).
pokemon_atk('crawdaunt', 120).
pokemon_atk('baltoy', 40).
pokemon_atk('claydol', 70).
pokemon_atk('lileep', 41).
pokemon_atk('cradily', 81).
pokemon_atk('anorith', 95).
pokemon_atk('armaldo', 125).
pokemon_atk('feebas', 15).
pokemon_atk('milotic', 60).
pokemon_atk('castform', 70).
pokemon_atk('castformsunny', 70).
pokemon_atk('castformrainy', 70).
pokemon_atk('castformsnowy', 70).
pokemon_atk('kecleon', 90).
pokemon_atk('shuppet', 75).
pokemon_atk('banette', 115).
pokemon_atk('banettemega', 165).
pokemon_atk('duskull', 40).
pokemon_atk('dusclops', 70).
pokemon_atk('tropius', 68).
pokemon_atk('chimecho', 50).
pokemon_atk('absol', 130).
pokemon_atk('absolmega', 150).
pokemon_atk('wynaut', 23).
pokemon_atk('snorunt', 50).
pokemon_atk('glalie', 80).
pokemon_atk('glaliemega', 120).
pokemon_atk('spheal', 40).
pokemon_atk('sealeo', 60).
pokemon_atk('walrein', 80).
pokemon_atk('clamperl', 64).
pokemon_atk('huntail', 104).
pokemon_atk('gorebyss', 84).
pokemon_atk('relicanth', 90).
pokemon_atk('luvdisc', 30).
pokemon_atk('bagon', 75).
pokemon_atk('shelgon', 95).
pokemon_atk('salamence', 135).
pokemon_atk('salamencemega', 145).
pokemon_atk('beldum', 55).
pokemon_atk('metang', 75).
pokemon_atk('metagross', 135).
pokemon_atk('metagrossmega', 145).
pokemon_atk('regirock', 100).
pokemon_atk('regice', 50).
pokemon_atk('registeel', 75).
pokemon_atk('latias', 80).
pokemon_atk('latiasmega', 100).
pokemon_atk('latios', 90).
pokemon_atk('latiosmega', 130).
pokemon_atk('kyogre', 100).
pokemon_atk('kyogreprimal', 150).
pokemon_atk('groudon', 150).
pokemon_atk('groudonprimal', 180).
pokemon_atk('rayquaza', 150).
pokemon_atk('rayquazamega', 180).
pokemon_atk('jirachi', 100).
pokemon_atk('deoxys', 150).
pokemon_atk('deoxysattack', 180).
pokemon_atk('deoxysdefense', 70).
pokemon_atk('deoxysspeed', 95).
pokemon_atk('turtwig', 68).
pokemon_atk('grotle', 89).
pokemon_atk('torterra', 109).
pokemon_atk('chimchar', 58).
pokemon_atk('monferno', 78).
pokemon_atk('infernape', 104).
pokemon_atk('piplup', 51).
pokemon_atk('prinplup', 66).
pokemon_atk('empoleon', 86).
pokemon_atk('starly', 55).
pokemon_atk('staravia', 75).
pokemon_atk('staraptor', 120).
pokemon_atk('bidoof', 45).
pokemon_atk('bibarel', 85).
pokemon_atk('kricketot', 25).
pokemon_atk('kricketune', 85).
pokemon_atk('shinx', 65).
pokemon_atk('luxio', 85).
pokemon_atk('luxray', 120).
pokemon_atk('budew', 30).
pokemon_atk('roserade', 70).
pokemon_atk('cranidos', 125).
pokemon_atk('rampardos', 165).
pokemon_atk('shieldon', 42).
pokemon_atk('bastiodon', 52).
pokemon_atk('burmy', 29).
pokemon_atk('burmysandy', 29).
pokemon_atk('burmytrash', 29).
pokemon_atk('wormadam', 59).
pokemon_atk('wormadamsandy', 79).
pokemon_atk('wormadamtrash', 69).
pokemon_atk('mothim', 94).
pokemon_atk('combee', 30).
pokemon_atk('vespiquen', 80).
pokemon_atk('pachirisu', 45).
pokemon_atk('buizel', 65).
pokemon_atk('floatzel', 105).
pokemon_atk('cherubi', 35).
pokemon_atk('cherrim', 60).
pokemon_atk('cherrimsunshine', 60).
pokemon_atk('shellos', 48).
pokemon_atk('shelloseast', 48).
pokemon_atk('gastrodon', 83).
pokemon_atk('gastrodoneast', 83).
pokemon_atk('ambipom', 100).
pokemon_atk('drifloon', 50).
pokemon_atk('drifblim', 80).
pokemon_atk('buneary', 66).
pokemon_atk('lopunny', 76).
pokemon_atk('lopunnymega', 136).
pokemon_atk('mismagius', 60).
pokemon_atk('honchkrow', 125).
pokemon_atk('glameow', 55).
pokemon_atk('purugly', 82).
pokemon_atk('chingling', 30).
pokemon_atk('stunky', 63).
pokemon_atk('skuntank', 93).
pokemon_atk('bronzor', 24).
pokemon_atk('bronzong', 89).
pokemon_atk('bonsly', 80).
pokemon_atk('mimejr', 25).
pokemon_atk('happiny', 5).
pokemon_atk('chatot', 65).
pokemon_atk('spiritomb', 92).
pokemon_atk('gible', 70).
pokemon_atk('gabite', 90).
pokemon_atk('garchomp', 130).
pokemon_atk('garchompmega', 170).
pokemon_atk('munchlax', 85).
pokemon_atk('riolu', 70).
pokemon_atk('lucario', 110).
pokemon_atk('lucariomega', 145).
pokemon_atk('hippopotas', 72).
pokemon_atk('hippowdon', 112).
pokemon_atk('skorupi', 50).
pokemon_atk('drapion', 90).
pokemon_atk('croagunk', 61).
pokemon_atk('toxicroak', 106).
pokemon_atk('carnivine', 100).
pokemon_atk('finneon', 49).
pokemon_atk('lumineon', 69).
pokemon_atk('mantyke', 20).
pokemon_atk('snover', 62).
pokemon_atk('abomasnow', 92).
pokemon_atk('abomasnowmega', 132).
pokemon_atk('weavile', 120).
pokemon_atk('magnezone', 70).
pokemon_atk('lickilicky', 85).
pokemon_atk('rhyperior', 140).
pokemon_atk('tangrowth', 100).
pokemon_atk('electivire', 123).
pokemon_atk('magmortar', 95).
pokemon_atk('togekiss', 50).
pokemon_atk('yanmega', 76).
pokemon_atk('leafeon', 110).
pokemon_atk('glaceon', 60).
pokemon_atk('gliscor', 95).
pokemon_atk('mamoswine', 130).
pokemon_atk('porygonz', 80).
pokemon_atk('gallade', 125).
pokemon_atk('gallademega', 165).
pokemon_atk('probopass', 55).
pokemon_atk('dusknoir', 100).
pokemon_atk('froslass', 80).
pokemon_atk('froslassmega', 0).
pokemon_atk('rotom', 50).
pokemon_atk('rotomheat', 65).
pokemon_atk('rotomwash', 65).
pokemon_atk('rotomfrost', 65).
pokemon_atk('rotomfan', 65).
pokemon_atk('rotommow', 65).
pokemon_atk('uxie', 75).
pokemon_atk('mesprit', 105).
pokemon_atk('azelf', 125).
pokemon_atk('dialga', 120).
pokemon_atk('dialgaorigin', 100).
pokemon_atk('palkia', 120).
pokemon_atk('palkiaorigin', 100).
pokemon_atk('heatran', 90).
pokemon_atk('regigigas', 160).
pokemon_atk('giratina', 100).
pokemon_atk('giratinaorigin', 120).
pokemon_atk('cresselia', 70).
pokemon_atk('phione', 80).
pokemon_atk('manaphy', 100).
pokemon_atk('darkrai', 90).
pokemon_atk('shaymin', 100).
pokemon_atk('shayminsky', 103).
pokemon_atk('arceus', 120).
pokemon_atk('arceusbug', 120).
pokemon_atk('arceusdark', 120).
pokemon_atk('arceusdragon', 120).
pokemon_atk('arceuselectric', 120).
pokemon_atk('arceusfairy', 120).
pokemon_atk('arceusfighting', 120).
pokemon_atk('arceusfire', 120).
pokemon_atk('arceusflying', 120).
pokemon_atk('arceusghost', 120).
pokemon_atk('arceusgrass', 120).
pokemon_atk('arceusground', 120).
pokemon_atk('arceusice', 120).
pokemon_atk('arceuspoison', 120).
pokemon_atk('arceuspsychic', 120).
pokemon_atk('arceusrock', 120).
pokemon_atk('arceussteel', 120).
pokemon_atk('arceuswater', 120).
pokemon_atk('victini', 100).
pokemon_atk('snivy', 45).
pokemon_atk('servine', 60).
pokemon_atk('serperior', 75).
pokemon_atk('tepig', 63).
pokemon_atk('pignite', 93).
pokemon_atk('emboar', 123).
pokemon_atk('emboarmega', 0).
pokemon_atk('oshawott', 55).
pokemon_atk('dewott', 75).
pokemon_atk('samurott', 100).
pokemon_atk('samurotthisui', 108).
pokemon_atk('patrat', 55).
pokemon_atk('watchog', 85).
pokemon_atk('lillipup', 60).
pokemon_atk('herdier', 80).
pokemon_atk('stoutland', 110).
pokemon_atk('purrloin', 50).
pokemon_atk('liepard', 88).
pokemon_atk('pansage', 53).
pokemon_atk('simisage', 98).
pokemon_atk('pansear', 53).
pokemon_atk('simisear', 98).
pokemon_atk('panpour', 53).
pokemon_atk('simipour', 98).
pokemon_atk('munna', 25).
pokemon_atk('musharna', 55).
pokemon_atk('pidove', 55).
pokemon_atk('tranquill', 77).
pokemon_atk('unfezant', 115).
pokemon_atk('blitzle', 60).
pokemon_atk('zebstrika', 100).
pokemon_atk('roggenrola', 75).
pokemon_atk('boldore', 105).
pokemon_atk('gigalith', 135).
pokemon_atk('woobat', 45).
pokemon_atk('swoobat', 57).
pokemon_atk('drilbur', 85).
pokemon_atk('excadrill', 135).
pokemon_atk('excadrillmega', 0).
pokemon_atk('audino', 60).
pokemon_atk('audinomega', 60).
pokemon_atk('timburr', 80).
pokemon_atk('gurdurr', 105).
pokemon_atk('conkeldurr', 140).
pokemon_atk('tympole', 50).
pokemon_atk('palpitoad', 65).
pokemon_atk('seismitoad', 95).
pokemon_atk('throh', 100).
pokemon_atk('sawk', 125).
pokemon_atk('sewaddle', 53).
pokemon_atk('swadloon', 63).
pokemon_atk('leavanny', 103).
pokemon_atk('venipede', 45).
pokemon_atk('whirlipede', 55).
pokemon_atk('scolipede', 100).
pokemon_atk('scolipedemega', 0).
pokemon_atk('cottonee', 27).
pokemon_atk('whimsicott', 67).
pokemon_atk('petilil', 35).
pokemon_atk('lilligant', 60).
pokemon_atk('lilliganthisui', 105).
pokemon_atk('basculin', 92).
pokemon_atk('basculinbluestriped', 92).
pokemon_atk('basculinwhitestriped', 92).
pokemon_atk('sandile', 72).
pokemon_atk('krokorok', 82).
pokemon_atk('krookodile', 117).
pokemon_atk('darumaka', 90).
pokemon_atk('darumakagalar', 90).
pokemon_atk('darmanitan', 140).
pokemon_atk('darmanitanzen', 30).
pokemon_atk('darmanitangalar', 140).
pokemon_atk('darmanitangalarzen', 160).
pokemon_atk('maractus', 86).
pokemon_atk('dwebble', 65).
pokemon_atk('crustle', 105).
pokemon_atk('scraggy', 75).
pokemon_atk('scrafty', 90).
pokemon_atk('scraftymega', 0).
pokemon_atk('sigilyph', 58).
pokemon_atk('yamask', 30).
pokemon_atk('yamaskgalar', 55).
pokemon_atk('cofagrigus', 50).
pokemon_atk('tirtouga', 78).
pokemon_atk('carracosta', 108).
pokemon_atk('archen', 112).
pokemon_atk('archeops', 140).
pokemon_atk('trubbish', 50).
pokemon_atk('garbodor', 95).
pokemon_atk('garbodorgmax', 95).
pokemon_atk('zorua', 65).
pokemon_atk('zoruahisui', 60).
pokemon_atk('zoroark', 105).
pokemon_atk('zoroarkhisui', 100).
pokemon_atk('minccino', 50).
pokemon_atk('cinccino', 95).
pokemon_atk('gothita', 30).
pokemon_atk('gothorita', 45).
pokemon_atk('gothitelle', 55).
pokemon_atk('solosis', 30).
pokemon_atk('duosion', 40).
pokemon_atk('reuniclus', 65).
pokemon_atk('ducklett', 44).
pokemon_atk('swanna', 87).
pokemon_atk('vanillite', 50).
pokemon_atk('vanillish', 65).
pokemon_atk('vanilluxe', 95).
pokemon_atk('deerling', 60).
pokemon_atk('deerlingsummer', 60).
pokemon_atk('deerlingautumn', 60).
pokemon_atk('deerlingwinter', 60).
pokemon_atk('sawsbuck', 100).
pokemon_atk('emolga', 75).
pokemon_atk('karrablast', 75).
pokemon_atk('escavalier', 135).
pokemon_atk('foongus', 55).
pokemon_atk('amoonguss', 85).
pokemon_atk('frillish', 40).
pokemon_atk('jellicent', 60).
pokemon_atk('alomomola', 75).
pokemon_atk('joltik', 47).
pokemon_atk('galvantula', 77).
pokemon_atk('ferroseed', 50).
pokemon_atk('ferrothorn', 94).
pokemon_atk('klink', 55).
pokemon_atk('klang', 80).
pokemon_atk('klinklang', 100).
pokemon_atk('tynamo', 55).
pokemon_atk('eelektrik', 85).
pokemon_atk('eelektross', 115).
pokemon_atk('eelektrossmega', 0).
pokemon_atk('elgyem', 55).
pokemon_atk('beheeyem', 75).
pokemon_atk('litwick', 30).
pokemon_atk('lampent', 40).
pokemon_atk('chandelure', 55).
pokemon_atk('chandeluremega', 0).
pokemon_atk('axew', 87).
pokemon_atk('fraxure', 117).
pokemon_atk('haxorus', 147).
pokemon_atk('cubchoo', 70).
pokemon_atk('beartic', 130).
pokemon_atk('cryogonal', 50).
pokemon_atk('shelmet', 40).
pokemon_atk('accelgor', 70).
pokemon_atk('stunfisk', 66).
pokemon_atk('stunfiskgalar', 81).
pokemon_atk('mienfoo', 85).
pokemon_atk('mienshao', 125).
pokemon_atk('druddigon', 120).
pokemon_atk('golett', 74).
pokemon_atk('golurk', 124).
pokemon_atk('pawniard', 85).
pokemon_atk('bisharp', 125).
pokemon_atk('bouffalant', 110).
pokemon_atk('rufflet', 83).
pokemon_atk('braviary', 123).
pokemon_atk('braviaryhisui', 83).
pokemon_atk('vullaby', 55).
pokemon_atk('mandibuzz', 65).
pokemon_atk('heatmor', 97).
pokemon_atk('durant', 109).
pokemon_atk('deino', 65).
pokemon_atk('zweilous', 85).
pokemon_atk('hydreigon', 105).
pokemon_atk('larvesta', 85).
pokemon_atk('volcarona', 60).
pokemon_atk('cobalion', 90).
pokemon_atk('terrakion', 129).
pokemon_atk('virizion', 90).
pokemon_atk('tornadus', 115).
pokemon_atk('tornadustherian', 100).
pokemon_atk('thundurus', 115).
pokemon_atk('thundurustherian', 105).
pokemon_atk('reshiram', 120).
pokemon_atk('zekrom', 150).
pokemon_atk('landorus', 125).
pokemon_atk('landorustherian', 145).
pokemon_atk('kyurem', 130).
pokemon_atk('kyuremblack', 170).
pokemon_atk('kyuremwhite', 120).
pokemon_atk('keldeo', 72).
pokemon_atk('keldeoresolute', 72).
pokemon_atk('meloetta', 77).
pokemon_atk('meloettapirouette', 128).
pokemon_atk('genesect', 120).
pokemon_atk('genesectdouse', 120).
pokemon_atk('genesectshock', 120).
pokemon_atk('genesectburn', 120).
pokemon_atk('genesectchill', 120).
pokemon_atk('chespin', 61).
pokemon_atk('quilladin', 78).
pokemon_atk('chesnaught', 107).
pokemon_atk('chesnaughtmega', 0).
pokemon_atk('fennekin', 45).
pokemon_atk('braixen', 59).
pokemon_atk('delphox', 69).
pokemon_atk('delphoxmega', 0).
pokemon_atk('froakie', 56).
pokemon_atk('frogadier', 63).
pokemon_atk('greninja', 95).
pokemon_atk('greninjabond', 95).
pokemon_atk('greninjaash', 145).
pokemon_atk('greninjamega', 0).
pokemon_atk('bunnelby', 36).
pokemon_atk('diggersby', 56).
pokemon_atk('fletchling', 50).
pokemon_atk('fletchinder', 73).
pokemon_atk('talonflame', 81).
pokemon_atk('scatterbug', 35).
pokemon_atk('spewpa', 22).
pokemon_atk('vivillon', 52).
pokemon_atk('vivillonicysnow', 52).
pokemon_atk('vivillonpolar', 52).
pokemon_atk('vivillontundra', 52).
pokemon_atk('vivilloncontinental', 52).
pokemon_atk('vivillongarden', 52).
pokemon_atk('vivillonelegant', 52).
pokemon_atk('vivillonmodern', 52).
pokemon_atk('vivillonmarine', 52).
pokemon_atk('vivillonarchipelago', 52).
pokemon_atk('vivillonhighplains', 52).
pokemon_atk('vivillonsandstorm', 52).
pokemon_atk('vivillonriver', 52).
pokemon_atk('vivillonmonsoon', 52).
pokemon_atk('vivillonsavanna', 52).
pokemon_atk('vivillonsun', 52).
pokemon_atk('vivillonocean', 52).
pokemon_atk('vivillonjungle', 52).
pokemon_atk('vivillonfancy', 52).
pokemon_atk('vivillonpokeball', 52).
pokemon_atk('litleo', 50).
pokemon_atk('pyroar', 68).
pokemon_atk('pyroarmega', 0).
pokemon_atk('flabebe', 38).
pokemon_atk('floette', 45).
pokemon_atk('floetteeternal', 65).
pokemon_atk('floettemega', 0).
pokemon_atk('florges', 65).
pokemon_atk('skiddo', 65).
pokemon_atk('gogoat', 100).
pokemon_atk('pancham', 82).
pokemon_atk('pangoro', 124).
pokemon_atk('furfrou', 80).
pokemon_atk('espurr', 48).
pokemon_atk('meowstic', 48).
pokemon_atk('meowsticf', 48).
pokemon_atk('honedge', 80).
pokemon_atk('doublade', 110).
pokemon_atk('aegislash', 50).
pokemon_atk('aegislashblade', 140).
pokemon_atk('spritzee', 52).
pokemon_atk('aromatisse', 72).
pokemon_atk('swirlix', 48).
pokemon_atk('slurpuff', 80).
pokemon_atk('inkay', 54).
pokemon_atk('malamar', 92).
pokemon_atk('malamarmega', 0).
pokemon_atk('binacle', 52).
pokemon_atk('barbaracle', 105).
pokemon_atk('barbaraclemega', 0).
pokemon_atk('skrelp', 60).
pokemon_atk('dragalge', 75).
pokemon_atk('dragalgemega', 0).
pokemon_atk('clauncher', 53).
pokemon_atk('clawitzer', 73).
pokemon_atk('helioptile', 38).
pokemon_atk('heliolisk', 55).
pokemon_atk('tyrunt', 89).
pokemon_atk('tyrantrum', 121).
pokemon_atk('amaura', 59).
pokemon_atk('aurorus', 77).
pokemon_atk('sylveon', 65).
pokemon_atk('hawlucha', 92).
pokemon_atk('hawluchamega', 0).
pokemon_atk('dedenne', 58).
pokemon_atk('carbink', 50).
pokemon_atk('goomy', 50).
pokemon_atk('sliggoo', 75).
pokemon_atk('sliggoohisui', 75).
pokemon_atk('goodra', 100).
pokemon_atk('goodrahisui', 100).
pokemon_atk('klefki', 80).
pokemon_atk('phantump', 70).
pokemon_atk('trevenant', 110).
pokemon_atk('pumpkaboo', 66).
pokemon_atk('pumpkaboosmall', 66).
pokemon_atk('pumpkaboolarge', 66).
pokemon_atk('pumpkaboosuper', 66).
pokemon_atk('gourgeist', 90).
pokemon_atk('gourgeistsmall', 85).
pokemon_atk('gourgeistlarge', 95).
pokemon_atk('gourgeistsuper', 100).
pokemon_atk('bergmite', 69).
pokemon_atk('avalugg', 117).
pokemon_atk('avalugghisui', 127).
pokemon_atk('noibat', 30).
pokemon_atk('noivern', 70).
pokemon_atk('xerneas', 131).
pokemon_atk('xerneasneutral', 131).
pokemon_atk('yveltal', 131).
pokemon_atk('zygarde', 100).
pokemon_atk('zygarde10', 100).
pokemon_atk('zygardecomplete', 100).
pokemon_atk('zygardemega', 0).
pokemon_atk('diancie', 100).
pokemon_atk('dianciemega', 160).
pokemon_atk('hoopa', 110).
pokemon_atk('hoopaunbound', 160).
pokemon_atk('volcanion', 110).
pokemon_atk('rowlet', 55).
pokemon_atk('dartrix', 75).
pokemon_atk('decidueye', 107).
pokemon_atk('decidueyehisui', 112).
pokemon_atk('litten', 65).
pokemon_atk('torracat', 85).
pokemon_atk('incineroar', 115).
pokemon_atk('popplio', 54).
pokemon_atk('brionne', 69).
pokemon_atk('primarina', 74).
pokemon_atk('pikipek', 75).
pokemon_atk('trumbeak', 85).
pokemon_atk('toucannon', 120).
pokemon_atk('yungoos', 70).
pokemon_atk('gumshoos', 110).
pokemon_atk('gumshoostotem', 110).
pokemon_atk('grubbin', 62).
pokemon_atk('charjabug', 82).
pokemon_atk('vikavolt', 70).
pokemon_atk('vikavolttotem', 70).
pokemon_atk('crabrawler', 82).
pokemon_atk('crabominable', 132).
pokemon_atk('oricorio', 70).
pokemon_atk('oricoriopompom', 70).
pokemon_atk('oricoriopau', 70).
pokemon_atk('oricoriosensu', 70).
pokemon_atk('cutiefly', 45).
pokemon_atk('ribombee', 55).
pokemon_atk('ribombeetotem', 55).
pokemon_atk('rockruff', 65).
pokemon_atk('rockruffdusk', 65).
pokemon_atk('lycanroc', 115).
pokemon_atk('lycanrocmidnight', 115).
pokemon_atk('lycanrocdusk', 117).
pokemon_atk('wishiwashi', 20).
pokemon_atk('wishiwashischool', 140).
pokemon_atk('mareanie', 53).
pokemon_atk('toxapex', 63).
pokemon_atk('mudbray', 100).
pokemon_atk('mudsdale', 125).
pokemon_atk('dewpider', 40).
pokemon_atk('araquanid', 70).
pokemon_atk('araquanidtotem', 70).
pokemon_atk('fomantis', 55).
pokemon_atk('lurantis', 105).
pokemon_atk('lurantistotem', 105).
pokemon_atk('morelull', 35).
pokemon_atk('shiinotic', 45).
pokemon_atk('salandit', 44).
pokemon_atk('salazzle', 64).
pokemon_atk('salazzletotem', 64).
pokemon_atk('stufful', 75).
pokemon_atk('bewear', 125).
pokemon_atk('bounsweet', 30).
pokemon_atk('steenee', 40).
pokemon_atk('tsareena', 120).
pokemon_atk('comfey', 52).
pokemon_atk('oranguru', 60).
pokemon_atk('passimian', 120).
pokemon_atk('wimpod', 35).
pokemon_atk('golisopod', 125).
pokemon_atk('sandygast', 55).
pokemon_atk('palossand', 75).
pokemon_atk('pyukumuku', 60).
pokemon_atk('typenull', 95).
pokemon_atk('silvally', 95).
pokemon_atk('silvallybug', 95).
pokemon_atk('silvallydark', 95).
pokemon_atk('silvallydragon', 95).
pokemon_atk('silvallyelectric', 95).
pokemon_atk('silvallyfairy', 95).
pokemon_atk('silvallyfighting', 95).
pokemon_atk('silvallyfire', 95).
pokemon_atk('silvallyflying', 95).
pokemon_atk('silvallyghost', 95).
pokemon_atk('silvallygrass', 95).
pokemon_atk('silvallyground', 95).
pokemon_atk('silvallyice', 95).
pokemon_atk('silvallypoison', 95).
pokemon_atk('silvallypsychic', 95).
pokemon_atk('silvallyrock', 95).
pokemon_atk('silvallysteel', 95).
pokemon_atk('silvallywater', 95).
pokemon_atk('minior', 100).
pokemon_atk('miniororange', 100).
pokemon_atk('minioryellow', 100).
pokemon_atk('miniorgreen', 100).
pokemon_atk('miniorblue', 100).
pokemon_atk('miniorindigo', 100).
pokemon_atk('miniorviolet', 100).
pokemon_atk('miniormeteor', 60).
pokemon_atk('komala', 115).
pokemon_atk('turtonator', 78).
pokemon_atk('togedemaru', 98).
pokemon_atk('togedemarutotem', 98).
pokemon_atk('mimikyu', 90).
pokemon_atk('mimikyubusted', 90).
pokemon_atk('mimikyutotem', 90).
pokemon_atk('mimikyubustedtotem', 90).
pokemon_atk('bruxish', 105).
pokemon_atk('drampa', 60).
pokemon_atk('drampamega', 0).
pokemon_atk('dhelmise', 131).
pokemon_atk('jangmoo', 55).
pokemon_atk('hakamoo', 75).
pokemon_atk('kommoo', 110).
pokemon_atk('kommoototem', 110).
pokemon_atk('tapukoko', 115).
pokemon_atk('tapulele', 85).
pokemon_atk('tapubulu', 130).
pokemon_atk('tapufini', 75).
pokemon_atk('cosmog', 29).
pokemon_atk('cosmoem', 29).
pokemon_atk('solgaleo', 137).
pokemon_atk('lunala', 113).
pokemon_atk('nihilego', 53).
pokemon_atk('buzzwole', 139).
pokemon_atk('pheromosa', 137).
pokemon_atk('xurkitree', 89).
pokemon_atk('celesteela', 101).
pokemon_atk('kartana', 181).
pokemon_atk('guzzlord', 101).
pokemon_atk('necrozma', 107).
pokemon_atk('necrozmaduskmane', 157).
pokemon_atk('necrozmadawnwings', 113).
pokemon_atk('necrozmaultra', 167).
pokemon_atk('magearna', 95).
pokemon_atk('magearnaoriginal', 95).
pokemon_atk('marshadow', 125).
pokemon_atk('poipole', 73).
pokemon_atk('naganadel', 73).
pokemon_atk('stakataka', 131).
pokemon_atk('blacephalon', 127).
pokemon_atk('zeraora', 112).
pokemon_atk('meltan', 65).
pokemon_atk('melmetal', 143).
pokemon_atk('melmetalgmax', 143).
pokemon_atk('grookey', 65).
pokemon_atk('thwackey', 85).
pokemon_atk('rillaboom', 125).
pokemon_atk('rillaboomgmax', 125).
pokemon_atk('scorbunny', 71).
pokemon_atk('raboot', 86).
pokemon_atk('cinderace', 116).
pokemon_atk('cinderacegmax', 116).
pokemon_atk('sobble', 40).
pokemon_atk('drizzile', 60).
pokemon_atk('inteleon', 85).
pokemon_atk('inteleongmax', 85).
pokemon_atk('skwovet', 55).
pokemon_atk('greedent', 95).
pokemon_atk('rookidee', 47).
pokemon_atk('corvisquire', 67).
pokemon_atk('corviknight', 87).
pokemon_atk('corviknightgmax', 87).
pokemon_atk('blipbug', 20).
pokemon_atk('dottler', 35).
pokemon_atk('orbeetle', 45).
pokemon_atk('orbeetlegmax', 45).
pokemon_atk('nickit', 28).
pokemon_atk('thievul', 58).
pokemon_atk('gossifleur', 40).
pokemon_atk('eldegoss', 50).
pokemon_atk('wooloo', 40).
pokemon_atk('dubwool', 80).
pokemon_atk('chewtle', 64).
pokemon_atk('drednaw', 115).
pokemon_atk('drednawgmax', 115).
pokemon_atk('yamper', 45).
pokemon_atk('boltund', 90).
pokemon_atk('rolycoly', 40).
pokemon_atk('carkol', 60).
pokemon_atk('coalossal', 80).
pokemon_atk('coalossalgmax', 80).
pokemon_atk('applin', 40).
pokemon_atk('flapple', 110).
pokemon_atk('flapplegmax', 110).
pokemon_atk('appletun', 85).
pokemon_atk('appletungmax', 85).
pokemon_atk('silicobra', 57).
pokemon_atk('sandaconda', 107).
pokemon_atk('sandacondagmax', 107).
pokemon_atk('cramorant', 85).
pokemon_atk('cramorantgulping', 85).
pokemon_atk('cramorantgorging', 85).
pokemon_atk('arrokuda', 63).
pokemon_atk('barraskewda', 123).
pokemon_atk('toxel', 38).
pokemon_atk('toxtricity', 98).
pokemon_atk('toxtricitylowkey', 98).
pokemon_atk('toxtricitygmax', 98).
pokemon_atk('toxtricitylowkeygmax', 98).
pokemon_atk('sizzlipede', 65).
pokemon_atk('centiskorch', 115).
pokemon_atk('centiskorchgmax', 115).
pokemon_atk('clobbopus', 68).
pokemon_atk('grapploct', 118).
pokemon_atk('sinistea', 45).
pokemon_atk('sinisteaantique', 45).
pokemon_atk('polteageist', 65).
pokemon_atk('polteageistantique', 65).
pokemon_atk('hatenna', 30).
pokemon_atk('hattrem', 40).
pokemon_atk('hatterene', 90).
pokemon_atk('hatterenegmax', 90).
pokemon_atk('impidimp', 45).
pokemon_atk('morgrem', 60).
pokemon_atk('grimmsnarl', 120).
pokemon_atk('grimmsnarlgmax', 120).
pokemon_atk('obstagoon', 90).
pokemon_atk('perrserker', 110).
pokemon_atk('cursola', 95).
pokemon_atk('sirfetchd', 135).
pokemon_atk('mrrime', 85).
pokemon_atk('runerigus', 95).
pokemon_atk('milcery', 40).
pokemon_atk('alcremie', 60).
pokemon_atk('alcremierubycream', 60).
pokemon_atk('alcremiematchacream', 60).
pokemon_atk('alcremiemintcream', 60).
pokemon_atk('alcremielemoncream', 60).
pokemon_atk('alcremierubyswirl', 60).
pokemon_atk('alcremiecaramelswirl', 60).
pokemon_atk('alcremierainbowswirl', 60).
pokemon_atk('alcremiegmax', 60).
pokemon_atk('falinks', 100).
pokemon_atk('falinksmega', 0).
pokemon_atk('pincurchin', 101).
pokemon_atk('snom', 25).
pokemon_atk('frosmoth', 65).
pokemon_atk('stonjourner', 125).
pokemon_atk('eiscue', 80).
pokemon_atk('eiscuenoice', 80).
pokemon_atk('indeedee', 65).
pokemon_atk('indeedeef', 55).
pokemon_atk('morpeko', 95).
pokemon_atk('morpekohangry', 95).
pokemon_atk('cufant', 80).
pokemon_atk('copperajah', 130).
pokemon_atk('copperajahgmax', 130).
pokemon_atk('dracozolt', 100).
pokemon_atk('arctozolt', 100).
pokemon_atk('dracovish', 90).
pokemon_atk('arctovish', 90).
pokemon_atk('duraludon', 95).
pokemon_atk('duraludongmax', 95).
pokemon_atk('dreepy', 60).
pokemon_atk('drakloak', 80).
pokemon_atk('dragapult', 120).
pokemon_atk('zacian', 120).
pokemon_atk('zaciancrowned', 150).
pokemon_atk('zamazenta', 120).
pokemon_atk('zamazentacrowned', 120).
pokemon_atk('eternatus', 85).
pokemon_atk('eternatuseternamax', 115).
pokemon_atk('kubfu', 90).
pokemon_atk('urshifu', 130).
pokemon_atk('urshifurapidstrike', 130).
pokemon_atk('urshifugmax', 130).
pokemon_atk('urshifurapidstrikegmax', 130).
pokemon_atk('zarude', 120).
pokemon_atk('zarudedada', 120).
pokemon_atk('regieleki', 100).
pokemon_atk('regidrago', 100).
pokemon_atk('glastrier', 145).
pokemon_atk('spectrier', 65).
pokemon_atk('calyrex', 80).
pokemon_atk('calyrexice', 165).
pokemon_atk('calyrexshadow', 85).
pokemon_atk('wyrdeer', 105).
pokemon_atk('kleavor', 135).
pokemon_atk('ursaluna', 140).
pokemon_atk('ursalunabloodmoon', 70).
pokemon_atk('basculegion', 112).
pokemon_atk('basculegionf', 92).
pokemon_atk('sneasler', 130).
pokemon_atk('overqwil', 115).
pokemon_atk('enamorus', 115).
pokemon_atk('enamorustherian', 115).
pokemon_atk('sprigatito', 61).
pokemon_atk('floragato', 80).
pokemon_atk('meowscarada', 110).
pokemon_atk('fuecoco', 45).
pokemon_atk('crocalor', 55).
pokemon_atk('skeledirge', 75).
pokemon_atk('quaxly', 65).
pokemon_atk('quaxwell', 85).
pokemon_atk('quaquaval', 120).
pokemon_atk('lechonk', 45).
pokemon_atk('oinkologne', 100).
pokemon_atk('oinkolognef', 90).
pokemon_atk('tarountula', 41).
pokemon_atk('spidops', 79).
pokemon_atk('nymble', 46).
pokemon_atk('lokix', 102).
pokemon_atk('pawmi', 50).
pokemon_atk('pawmo', 75).
pokemon_atk('pawmot', 115).
pokemon_atk('tandemaus', 50).
pokemon_atk('maushold', 75).
pokemon_atk('mausholdfour', 75).
pokemon_atk('fidough', 55).
pokemon_atk('dachsbun', 80).
pokemon_atk('smoliv', 35).
pokemon_atk('dolliv', 53).
pokemon_atk('arboliva', 69).
pokemon_atk('squawkabilly', 96).
pokemon_atk('squawkabillyblue', 96).
pokemon_atk('squawkabillyyellow', 96).
pokemon_atk('squawkabillywhite', 96).
pokemon_atk('nacli', 55).
pokemon_atk('naclstack', 60).
pokemon_atk('garganacl', 100).
pokemon_atk('charcadet', 50).
pokemon_atk('armarouge', 60).
pokemon_atk('ceruledge', 125).
pokemon_atk('tadbulb', 31).
pokemon_atk('bellibolt', 64).
pokemon_atk('wattrel', 40).
pokemon_atk('kilowattrel', 70).
pokemon_atk('maschiff', 78).
pokemon_atk('mabosstiff', 120).
pokemon_atk('shroodle', 65).
pokemon_atk('grafaiai', 95).
pokemon_atk('bramblin', 65).
pokemon_atk('brambleghast', 115).
pokemon_atk('toedscool', 40).
pokemon_atk('toedscruel', 70).
pokemon_atk('klawf', 100).
pokemon_atk('capsakid', 62).
pokemon_atk('scovillain', 108).
pokemon_atk('rellor', 50).
pokemon_atk('rabsca', 50).
pokemon_atk('flittle', 35).
pokemon_atk('espathra', 60).
pokemon_atk('tinkatink', 45).
pokemon_atk('tinkatuff', 55).
pokemon_atk('tinkaton', 75).
pokemon_atk('wiglett', 55).
pokemon_atk('wugtrio', 100).
pokemon_atk('bombirdier', 103).
pokemon_atk('finizen', 45).
pokemon_atk('palafin', 70).
pokemon_atk('palafinhero', 160).
pokemon_atk('varoom', 70).
pokemon_atk('revavroom', 119).
pokemon_atk('cyclizar', 95).
pokemon_atk('orthworm', 85).
pokemon_atk('glimmet', 35).
pokemon_atk('glimmora', 55).
pokemon_atk('greavard', 61).
pokemon_atk('houndstone', 101).
pokemon_atk('flamigo', 115).
pokemon_atk('cetoddle', 68).
pokemon_atk('cetitan', 113).
pokemon_atk('veluza', 102).
pokemon_atk('dondozo', 100).
pokemon_atk('tatsugiri', 50).
pokemon_atk('tatsugiridroopy', 50).
pokemon_atk('tatsugiristretchy', 50).
pokemon_atk('annihilape', 115).
pokemon_atk('clodsire', 75).
pokemon_atk('farigiraf', 90).
pokemon_atk('dudunsparce', 100).
pokemon_atk('dudunsparcethreesegment', 100).
pokemon_atk('kingambit', 135).
pokemon_atk('greattusk', 131).
pokemon_atk('screamtail', 65).
pokemon_atk('brutebonnet', 127).
pokemon_atk('fluttermane', 55).
pokemon_atk('slitherwing', 135).
pokemon_atk('sandyshocks', 81).
pokemon_atk('irontreads', 112).
pokemon_atk('ironbundle', 80).
pokemon_atk('ironhands', 140).
pokemon_atk('ironjugulis', 80).
pokemon_atk('ironmoth', 70).
pokemon_atk('ironthorns', 134).
pokemon_atk('frigibax', 75).
pokemon_atk('arctibax', 95).
pokemon_atk('baxcalibur', 145).
pokemon_atk('gimmighoul', 30).
pokemon_atk('gimmighoulroaming', 30).
pokemon_atk('gholdengo', 60).
pokemon_atk('wochien', 85).
pokemon_atk('chienpao', 120).
pokemon_atk('tinglu', 110).
pokemon_atk('chiyu', 80).
pokemon_atk('roaringmoon', 139).
pokemon_atk('ironvaliant', 130).
pokemon_atk('koraidon', 135).
pokemon_atk('miraidon', 85).
pokemon_atk('walkingwake', 83).
pokemon_atk('ironleaves', 130).
pokemon_atk('dipplin', 80).
pokemon_atk('poltchageist', 45).
pokemon_atk('poltchageistartisan', 45).
pokemon_atk('sinistcha', 60).
pokemon_atk('sinistchamasterpiece', 60).
pokemon_atk('okidogi', 128).
pokemon_atk('munkidori', 75).
pokemon_atk('fezandipiti', 91).
pokemon_atk('ogerpon', 120).
pokemon_atk('ogerponwellspring', 120).
pokemon_atk('ogerponhearthflame', 120).
pokemon_atk('ogerponcornerstone', 120).
pokemon_atk('ogerpontealtera', 120).
pokemon_atk('ogerponwellspringtera', 120).
pokemon_atk('ogerponhearthflametera', 120).
pokemon_atk('ogerponcornerstonetera', 120).
pokemon_atk('archaludon', 105).
pokemon_atk('hydrapple', 80).
pokemon_atk('gougingfire', 115).
pokemon_atk('ragingbolt', 73).
pokemon_atk('ironboulder', 120).
pokemon_atk('ironcrown', 72).
pokemon_atk('terapagos', 65).
pokemon_atk('terapagosterastal', 95).
pokemon_atk('terapagosstellar', 105).
pokemon_atk('pecharunt', 88).
pokemon_atk('missingno', 136).
pokemon_atk('ramnarok', 0).
pokemon_atk('ramnarokradiant', 0).
pokemon_atk('pokestarsmeargle', 20).
pokemon_atk('pokestarufo', 100).
pokemon_atk('pokestarufo2', 100).
pokemon_atk('pokestarbrycenman', 100).
pokemon_atk('pokestarmt', 100).
pokemon_atk('pokestarmt2', 100).
pokemon_atk('pokestartransport', 100).
pokemon_atk('pokestargiant', 100).
pokemon_atk('pokestarhumanoid', 100).
pokemon_atk('pokestarmonster', 100).
pokemon_atk('pokestarf00', 100).
pokemon_atk('pokestarf002', 100).
pokemon_atk('pokestarspirit', 100).
pokemon_atk('pokestarblackdoor', 100).
pokemon_atk('pokestarwhitedoor', 100).
pokemon_atk('pokestarblackbelt', 100).
pokemon_atk('pokestarufopropu2', 100).
pokemon_def('bulbasaur', 49).
pokemon_def('ivysaur', 63).
pokemon_def('venusaur', 83).
pokemon_def('venusaurmega', 123).
pokemon_def('venusaurgmax', 83).
pokemon_def('charmander', 43).
pokemon_def('charmeleon', 58).
pokemon_def('charizard', 78).
pokemon_def('charizardmegax', 111).
pokemon_def('charizardmegay', 78).
pokemon_def('charizardgmax', 78).
pokemon_def('squirtle', 65).
pokemon_def('wartortle', 80).
pokemon_def('blastoise', 100).
pokemon_def('blastoisemega', 120).
pokemon_def('blastoisegmax', 100).
pokemon_def('caterpie', 35).
pokemon_def('metapod', 55).
pokemon_def('butterfree', 50).
pokemon_def('butterfreegmax', 50).
pokemon_def('weedle', 30).
pokemon_def('kakuna', 50).
pokemon_def('beedrill', 40).
pokemon_def('beedrillmega', 40).
pokemon_def('pidgey', 40).
pokemon_def('pidgeotto', 55).
pokemon_def('pidgeot', 75).
pokemon_def('pidgeotmega', 80).
pokemon_def('rattata', 35).
pokemon_def('rattataalola', 35).
pokemon_def('raticate', 60).
pokemon_def('raticatealola', 70).
pokemon_def('raticatealolatotem', 70).
pokemon_def('spearow', 30).
pokemon_def('fearow', 65).
pokemon_def('ekans', 44).
pokemon_def('arbok', 69).
pokemon_def('pikachu', 40).
pokemon_def('pikachucosplay', 40).
pokemon_def('pikachurockstar', 40).
pokemon_def('pikachubelle', 40).
pokemon_def('pikachupopstar', 40).
pokemon_def('pikachuphd', 40).
pokemon_def('pikachulibre', 40).
pokemon_def('pikachuoriginal', 40).
pokemon_def('pikachuhoenn', 40).
pokemon_def('pikachusinnoh', 40).
pokemon_def('pikachuunova', 40).
pokemon_def('pikachukalos', 40).
pokemon_def('pikachualola', 40).
pokemon_def('pikachupartner', 40).
pokemon_def('pikachustarter', 50).
pokemon_def('pikachugmax', 40).
pokemon_def('pikachuworld', 40).
pokemon_def('raichu', 55).
pokemon_def('raichualola', 50).
pokemon_def('sandshrew', 85).
pokemon_def('sandshrewalola', 90).
pokemon_def('sandslash', 110).
pokemon_def('sandslashalola', 120).
pokemon_def('nidoranf', 52).
pokemon_def('nidorina', 67).
pokemon_def('nidoqueen', 87).
pokemon_def('nidoranm', 40).
pokemon_def('nidorino', 57).
pokemon_def('nidoking', 77).
pokemon_def('clefairy', 48).
pokemon_def('clefable', 73).
pokemon_def('clefablemega', 0).
pokemon_def('vulpix', 40).
pokemon_def('vulpixalola', 40).
pokemon_def('ninetales', 75).
pokemon_def('ninetalesalola', 75).
pokemon_def('jigglypuff', 20).
pokemon_def('wigglytuff', 45).
pokemon_def('zubat', 35).
pokemon_def('golbat', 70).
pokemon_def('oddish', 55).
pokemon_def('gloom', 70).
pokemon_def('vileplume', 85).
pokemon_def('paras', 55).
pokemon_def('parasect', 80).
pokemon_def('venonat', 50).
pokemon_def('venomoth', 60).
pokemon_def('diglett', 25).
pokemon_def('diglettalola', 30).
pokemon_def('dugtrio', 50).
pokemon_def('dugtrioalola', 60).
pokemon_def('meowth', 35).
pokemon_def('meowthalola', 35).
pokemon_def('meowthgalar', 55).
pokemon_def('meowthgmax', 35).
pokemon_def('persian', 60).
pokemon_def('persianalola', 60).
pokemon_def('psyduck', 48).
pokemon_def('golduck', 78).
pokemon_def('mankey', 35).
pokemon_def('primeape', 60).
pokemon_def('growlithe', 45).
pokemon_def('growlithehisui', 45).
pokemon_def('arcanine', 80).
pokemon_def('arcaninehisui', 80).
pokemon_def('poliwag', 40).
pokemon_def('poliwhirl', 65).
pokemon_def('poliwrath', 95).
pokemon_def('abra', 15).
pokemon_def('kadabra', 30).
pokemon_def('alakazam', 45).
pokemon_def('alakazammega', 65).
pokemon_def('machop', 50).
pokemon_def('machoke', 70).
pokemon_def('machamp', 80).
pokemon_def('machampgmax', 80).
pokemon_def('bellsprout', 35).
pokemon_def('weepinbell', 50).
pokemon_def('victreebel', 65).
pokemon_def('victreebelmega', 0).
pokemon_def('tentacool', 35).
pokemon_def('tentacruel', 65).
pokemon_def('geodude', 100).
pokemon_def('geodudealola', 100).
pokemon_def('graveler', 115).
pokemon_def('graveleralola', 115).
pokemon_def('golem', 130).
pokemon_def('golemalola', 130).
pokemon_def('ponyta', 55).
pokemon_def('ponytagalar', 55).
pokemon_def('rapidash', 70).
pokemon_def('rapidashgalar', 70).
pokemon_def('slowpoke', 65).
pokemon_def('slowpokegalar', 65).
pokemon_def('slowbro', 110).
pokemon_def('slowbromega', 180).
pokemon_def('slowbrogalar', 95).
pokemon_def('magnemite', 70).
pokemon_def('magneton', 95).
pokemon_def('farfetchd', 55).
pokemon_def('farfetchdgalar', 55).
pokemon_def('doduo', 45).
pokemon_def('dodrio', 70).
pokemon_def('seel', 55).
pokemon_def('dewgong', 80).
pokemon_def('grimer', 50).
pokemon_def('grimeralola', 50).
pokemon_def('muk', 75).
pokemon_def('mukalola', 75).
pokemon_def('shellder', 100).
pokemon_def('cloyster', 180).
pokemon_def('gastly', 30).
pokemon_def('haunter', 45).
pokemon_def('gengar', 60).
pokemon_def('gengarmega', 80).
pokemon_def('gengargmax', 60).
pokemon_def('onix', 160).
pokemon_def('drowzee', 45).
pokemon_def('hypno', 70).
pokemon_def('krabby', 90).
pokemon_def('kingler', 115).
pokemon_def('kinglergmax', 115).
pokemon_def('voltorb', 50).
pokemon_def('voltorbhisui', 50).
pokemon_def('electrode', 70).
pokemon_def('electrodehisui', 70).
pokemon_def('exeggcute', 80).
pokemon_def('exeggutor', 85).
pokemon_def('exeggutoralola', 85).
pokemon_def('cubone', 95).
pokemon_def('marowak', 110).
pokemon_def('marowakalola', 110).
pokemon_def('marowakalolatotem', 110).
pokemon_def('hitmonlee', 53).
pokemon_def('hitmonchan', 79).
pokemon_def('lickitung', 75).
pokemon_def('koffing', 95).
pokemon_def('weezing', 120).
pokemon_def('weezinggalar', 120).
pokemon_def('rhyhorn', 95).
pokemon_def('rhydon', 120).
pokemon_def('chansey', 5).
pokemon_def('tangela', 115).
pokemon_def('kangaskhan', 80).
pokemon_def('kangaskhanmega', 100).
pokemon_def('horsea', 70).
pokemon_def('seadra', 95).
pokemon_def('goldeen', 60).
pokemon_def('seaking', 65).
pokemon_def('staryu', 55).
pokemon_def('starmie', 85).
pokemon_def('starmiemega', 0).
pokemon_def('mrmime', 65).
pokemon_def('mrmimegalar', 65).
pokemon_def('scyther', 80).
pokemon_def('jynx', 35).
pokemon_def('electabuzz', 57).
pokemon_def('magmar', 57).
pokemon_def('pinsir', 100).
pokemon_def('pinsirmega', 120).
pokemon_def('tauros', 95).
pokemon_def('taurospaldeacombat', 105).
pokemon_def('taurospaldeablaze', 105).
pokemon_def('taurospaldeaaqua', 105).
pokemon_def('magikarp', 55).
pokemon_def('gyarados', 79).
pokemon_def('gyaradosmega', 109).
pokemon_def('lapras', 80).
pokemon_def('laprasgmax', 80).
pokemon_def('ditto', 48).
pokemon_def('eevee', 50).
pokemon_def('eeveestarter', 70).
pokemon_def('eeveegmax', 50).
pokemon_def('vaporeon', 60).
pokemon_def('jolteon', 60).
pokemon_def('flareon', 60).
pokemon_def('porygon', 70).
pokemon_def('omanyte', 100).
pokemon_def('omastar', 125).
pokemon_def('kabuto', 90).
pokemon_def('kabutops', 105).
pokemon_def('aerodactyl', 65).
pokemon_def('aerodactylmega', 85).
pokemon_def('snorlax', 65).
pokemon_def('snorlaxgmax', 65).
pokemon_def('articuno', 100).
pokemon_def('articunogalar', 85).
pokemon_def('zapdos', 85).
pokemon_def('zapdosgalar', 90).
pokemon_def('moltres', 90).
pokemon_def('moltresgalar', 90).
pokemon_def('dratini', 45).
pokemon_def('dragonair', 65).
pokemon_def('dragonite', 95).
pokemon_def('dragonitemega', 0).
pokemon_def('mewtwo', 90).
pokemon_def('mewtwomegax', 100).
pokemon_def('mewtwomegay', 70).
pokemon_def('mew', 100).
pokemon_def('chikorita', 65).
pokemon_def('bayleef', 80).
pokemon_def('meganium', 100).
pokemon_def('meganiummega', 0).
pokemon_def('cyndaquil', 43).
pokemon_def('quilava', 58).
pokemon_def('typhlosion', 78).
pokemon_def('typhlosionhisui', 78).
pokemon_def('totodile', 64).
pokemon_def('croconaw', 80).
pokemon_def('feraligatr', 100).
pokemon_def('feraligatrmega', 0).
pokemon_def('sentret', 34).
pokemon_def('furret', 64).
pokemon_def('hoothoot', 30).
pokemon_def('noctowl', 50).
pokemon_def('ledyba', 30).
pokemon_def('ledian', 50).
pokemon_def('spinarak', 40).
pokemon_def('ariados', 70).
pokemon_def('crobat', 80).
pokemon_def('chinchou', 38).
pokemon_def('lanturn', 58).
pokemon_def('pichu', 15).
pokemon_def('pichuspikyeared', 15).
pokemon_def('cleffa', 28).
pokemon_def('igglybuff', 15).
pokemon_def('togepi', 65).
pokemon_def('togetic', 85).
pokemon_def('natu', 45).
pokemon_def('xatu', 70).
pokemon_def('mareep', 40).
pokemon_def('flaaffy', 55).
pokemon_def('ampharos', 85).
pokemon_def('ampharosmega', 105).
pokemon_def('bellossom', 95).
pokemon_def('marill', 50).
pokemon_def('azumarill', 80).
pokemon_def('sudowoodo', 115).
pokemon_def('politoed', 75).
pokemon_def('hoppip', 40).
pokemon_def('skiploom', 50).
pokemon_def('jumpluff', 70).
pokemon_def('aipom', 55).
pokemon_def('sunkern', 30).
pokemon_def('sunflora', 55).
pokemon_def('yanma', 45).
pokemon_def('wooper', 45).
pokemon_def('wooperpaldea', 45).
pokemon_def('quagsire', 85).
pokemon_def('espeon', 60).
pokemon_def('umbreon', 110).
pokemon_def('murkrow', 42).
pokemon_def('slowking', 80).
pokemon_def('slowkinggalar', 80).
pokemon_def('misdreavus', 60).
pokemon_def('unown', 48).
pokemon_def('wobbuffet', 58).
pokemon_def('girafarig', 65).
pokemon_def('pineco', 90).
pokemon_def('forretress', 140).
pokemon_def('dunsparce', 70).
pokemon_def('gligar', 105).
pokemon_def('steelix', 200).
pokemon_def('steelixmega', 230).
pokemon_def('snubbull', 50).
pokemon_def('granbull', 75).
pokemon_def('qwilfish', 85).
pokemon_def('qwilfishhisui', 85).
pokemon_def('scizor', 100).
pokemon_def('scizormega', 140).
pokemon_def('shuckle', 230).
pokemon_def('heracross', 75).
pokemon_def('heracrossmega', 115).
pokemon_def('sneasel', 55).
pokemon_def('sneaselhisui', 55).
pokemon_def('teddiursa', 50).
pokemon_def('ursaring', 75).
pokemon_def('slugma', 40).
pokemon_def('magcargo', 120).
pokemon_def('swinub', 40).
pokemon_def('piloswine', 80).
pokemon_def('corsola', 95).
pokemon_def('corsolagalar', 100).
pokemon_def('remoraid', 35).
pokemon_def('octillery', 75).
pokemon_def('delibird', 45).
pokemon_def('mantine', 70).
pokemon_def('skarmory', 140).
pokemon_def('skarmorymega', 0).
pokemon_def('houndour', 30).
pokemon_def('houndoom', 50).
pokemon_def('houndoommega', 90).
pokemon_def('kingdra', 95).
pokemon_def('phanpy', 60).
pokemon_def('donphan', 120).
pokemon_def('porygon2', 90).
pokemon_def('stantler', 62).
pokemon_def('smeargle', 35).
pokemon_def('tyrogue', 35).
pokemon_def('hitmontop', 95).
pokemon_def('smoochum', 15).
pokemon_def('elekid', 37).
pokemon_def('magby', 37).
pokemon_def('miltank', 105).
pokemon_def('blissey', 10).
pokemon_def('raikou', 75).
pokemon_def('entei', 85).
pokemon_def('suicune', 115).
pokemon_def('larvitar', 50).
pokemon_def('pupitar', 70).
pokemon_def('tyranitar', 110).
pokemon_def('tyranitarmega', 150).
pokemon_def('lugia', 130).
pokemon_def('hooh', 90).
pokemon_def('celebi', 100).
pokemon_def('treecko', 35).
pokemon_def('grovyle', 45).
pokemon_def('sceptile', 65).
pokemon_def('sceptilemega', 75).
pokemon_def('torchic', 40).
pokemon_def('combusken', 60).
pokemon_def('blaziken', 70).
pokemon_def('blazikenmega', 80).
pokemon_def('mudkip', 50).
pokemon_def('marshtomp', 70).
pokemon_def('swampert', 90).
pokemon_def('swampertmega', 110).
pokemon_def('poochyena', 35).
pokemon_def('mightyena', 70).
pokemon_def('zigzagoon', 41).
pokemon_def('zigzagoongalar', 41).
pokemon_def('linoone', 61).
pokemon_def('linoonegalar', 61).
pokemon_def('wurmple', 35).
pokemon_def('silcoon', 55).
pokemon_def('beautifly', 50).
pokemon_def('cascoon', 55).
pokemon_def('dustox', 70).
pokemon_def('lotad', 30).
pokemon_def('lombre', 50).
pokemon_def('ludicolo', 70).
pokemon_def('seedot', 50).
pokemon_def('nuzleaf', 40).
pokemon_def('shiftry', 60).
pokemon_def('taillow', 30).
pokemon_def('swellow', 60).
pokemon_def('wingull', 30).
pokemon_def('pelipper', 100).
pokemon_def('ralts', 25).
pokemon_def('kirlia', 35).
pokemon_def('gardevoir', 65).
pokemon_def('gardevoirmega', 65).
pokemon_def('surskit', 32).
pokemon_def('masquerain', 62).
pokemon_def('shroomish', 60).
pokemon_def('breloom', 80).
pokemon_def('slakoth', 60).
pokemon_def('vigoroth', 80).
pokemon_def('slaking', 100).
pokemon_def('nincada', 90).
pokemon_def('ninjask', 45).
pokemon_def('shedinja', 45).
pokemon_def('whismur', 23).
pokemon_def('loudred', 43).
pokemon_def('exploud', 63).
pokemon_def('makuhita', 30).
pokemon_def('hariyama', 60).
pokemon_def('azurill', 40).
pokemon_def('nosepass', 135).
pokemon_def('skitty', 45).
pokemon_def('delcatty', 65).
pokemon_def('sableye', 75).
pokemon_def('sableyemega', 125).
pokemon_def('mawile', 85).
pokemon_def('mawilemega', 125).
pokemon_def('aron', 100).
pokemon_def('lairon', 140).
pokemon_def('aggron', 180).
pokemon_def('aggronmega', 230).
pokemon_def('meditite', 55).
pokemon_def('medicham', 75).
pokemon_def('medichammega', 85).
pokemon_def('electrike', 40).
pokemon_def('manectric', 60).
pokemon_def('manectricmega', 80).
pokemon_def('plusle', 40).
pokemon_def('minun', 50).
pokemon_def('volbeat', 75).
pokemon_def('illumise', 75).
pokemon_def('roselia', 45).
pokemon_def('gulpin', 53).
pokemon_def('swalot', 83).
pokemon_def('carvanha', 20).
pokemon_def('sharpedo', 40).
pokemon_def('sharpedomega', 70).
pokemon_def('wailmer', 35).
pokemon_def('wailord', 45).
pokemon_def('numel', 40).
pokemon_def('camerupt', 70).
pokemon_def('cameruptmega', 100).
pokemon_def('torkoal', 140).
pokemon_def('spoink', 35).
pokemon_def('grumpig', 65).
pokemon_def('spinda', 60).
pokemon_def('trapinch', 45).
pokemon_def('vibrava', 50).
pokemon_def('flygon', 80).
pokemon_def('cacnea', 40).
pokemon_def('cacturne', 60).
pokemon_def('swablu', 60).
pokemon_def('altaria', 90).
pokemon_def('altariamega', 110).
pokemon_def('zangoose', 60).
pokemon_def('seviper', 60).
pokemon_def('lunatone', 65).
pokemon_def('solrock', 85).
pokemon_def('barboach', 43).
pokemon_def('whiscash', 73).
pokemon_def('corphish', 65).
pokemon_def('crawdaunt', 85).
pokemon_def('baltoy', 55).
pokemon_def('claydol', 105).
pokemon_def('lileep', 77).
pokemon_def('cradily', 97).
pokemon_def('anorith', 50).
pokemon_def('armaldo', 100).
pokemon_def('feebas', 20).
pokemon_def('milotic', 79).
pokemon_def('castform', 70).
pokemon_def('castformsunny', 70).
pokemon_def('castformrainy', 70).
pokemon_def('castformsnowy', 70).
pokemon_def('kecleon', 70).
pokemon_def('shuppet', 35).
pokemon_def('banette', 65).
pokemon_def('banettemega', 75).
pokemon_def('duskull', 90).
pokemon_def('dusclops', 130).
pokemon_def('tropius', 83).
pokemon_def('chimecho', 80).
pokemon_def('absol', 60).
pokemon_def('absolmega', 60).
pokemon_def('wynaut', 48).
pokemon_def('snorunt', 50).
pokemon_def('glalie', 80).
pokemon_def('glaliemega', 80).
pokemon_def('spheal', 50).
pokemon_def('sealeo', 70).
pokemon_def('walrein', 90).
pokemon_def('clamperl', 85).
pokemon_def('huntail', 105).
pokemon_def('gorebyss', 105).
pokemon_def('relicanth', 130).
pokemon_def('luvdisc', 55).
pokemon_def('bagon', 60).
pokemon_def('shelgon', 100).
pokemon_def('salamence', 80).
pokemon_def('salamencemega', 130).
pokemon_def('beldum', 80).
pokemon_def('metang', 100).
pokemon_def('metagross', 130).
pokemon_def('metagrossmega', 150).
pokemon_def('regirock', 200).
pokemon_def('regice', 100).
pokemon_def('registeel', 150).
pokemon_def('latias', 90).
pokemon_def('latiasmega', 120).
pokemon_def('latios', 80).
pokemon_def('latiosmega', 100).
pokemon_def('kyogre', 90).
pokemon_def('kyogreprimal', 90).
pokemon_def('groudon', 140).
pokemon_def('groudonprimal', 160).
pokemon_def('rayquaza', 90).
pokemon_def('rayquazamega', 100).
pokemon_def('jirachi', 100).
pokemon_def('deoxys', 50).
pokemon_def('deoxysattack', 20).
pokemon_def('deoxysdefense', 160).
pokemon_def('deoxysspeed', 90).
pokemon_def('turtwig', 64).
pokemon_def('grotle', 85).
pokemon_def('torterra', 105).
pokemon_def('chimchar', 44).
pokemon_def('monferno', 52).
pokemon_def('infernape', 71).
pokemon_def('piplup', 53).
pokemon_def('prinplup', 68).
pokemon_def('empoleon', 88).
pokemon_def('starly', 30).
pokemon_def('staravia', 50).
pokemon_def('staraptor', 70).
pokemon_def('bidoof', 40).
pokemon_def('bibarel', 60).
pokemon_def('kricketot', 41).
pokemon_def('kricketune', 51).
pokemon_def('shinx', 34).
pokemon_def('luxio', 49).
pokemon_def('luxray', 79).
pokemon_def('budew', 35).
pokemon_def('roserade', 65).
pokemon_def('cranidos', 40).
pokemon_def('rampardos', 60).
pokemon_def('shieldon', 118).
pokemon_def('bastiodon', 168).
pokemon_def('burmy', 45).
pokemon_def('burmysandy', 45).
pokemon_def('burmytrash', 45).
pokemon_def('wormadam', 85).
pokemon_def('wormadamsandy', 105).
pokemon_def('wormadamtrash', 95).
pokemon_def('mothim', 50).
pokemon_def('combee', 42).
pokemon_def('vespiquen', 102).
pokemon_def('pachirisu', 70).
pokemon_def('buizel', 35).
pokemon_def('floatzel', 55).
pokemon_def('cherubi', 45).
pokemon_def('cherrim', 70).
pokemon_def('cherrimsunshine', 70).
pokemon_def('shellos', 48).
pokemon_def('shelloseast', 48).
pokemon_def('gastrodon', 68).
pokemon_def('gastrodoneast', 68).
pokemon_def('ambipom', 66).
pokemon_def('drifloon', 34).
pokemon_def('drifblim', 44).
pokemon_def('buneary', 44).
pokemon_def('lopunny', 84).
pokemon_def('lopunnymega', 94).
pokemon_def('mismagius', 60).
pokemon_def('honchkrow', 52).
pokemon_def('glameow', 42).
pokemon_def('purugly', 64).
pokemon_def('chingling', 50).
pokemon_def('stunky', 47).
pokemon_def('skuntank', 67).
pokemon_def('bronzor', 86).
pokemon_def('bronzong', 116).
pokemon_def('bonsly', 95).
pokemon_def('mimejr', 45).
pokemon_def('happiny', 5).
pokemon_def('chatot', 45).
pokemon_def('spiritomb', 108).
pokemon_def('gible', 45).
pokemon_def('gabite', 65).
pokemon_def('garchomp', 95).
pokemon_def('garchompmega', 115).
pokemon_def('munchlax', 40).
pokemon_def('riolu', 40).
pokemon_def('lucario', 70).
pokemon_def('lucariomega', 88).
pokemon_def('hippopotas', 78).
pokemon_def('hippowdon', 118).
pokemon_def('skorupi', 90).
pokemon_def('drapion', 110).
pokemon_def('croagunk', 40).
pokemon_def('toxicroak', 65).
pokemon_def('carnivine', 72).
pokemon_def('finneon', 56).
pokemon_def('lumineon', 76).
pokemon_def('mantyke', 50).
pokemon_def('snover', 50).
pokemon_def('abomasnow', 75).
pokemon_def('abomasnowmega', 105).
pokemon_def('weavile', 65).
pokemon_def('magnezone', 115).
pokemon_def('lickilicky', 95).
pokemon_def('rhyperior', 130).
pokemon_def('tangrowth', 125).
pokemon_def('electivire', 67).
pokemon_def('magmortar', 67).
pokemon_def('togekiss', 95).
pokemon_def('yanmega', 86).
pokemon_def('leafeon', 130).
pokemon_def('glaceon', 110).
pokemon_def('gliscor', 125).
pokemon_def('mamoswine', 80).
pokemon_def('porygonz', 70).
pokemon_def('gallade', 65).
pokemon_def('gallademega', 95).
pokemon_def('probopass', 145).
pokemon_def('dusknoir', 135).
pokemon_def('froslass', 70).
pokemon_def('froslassmega', 0).
pokemon_def('rotom', 77).
pokemon_def('rotomheat', 107).
pokemon_def('rotomwash', 107).
pokemon_def('rotomfrost', 107).
pokemon_def('rotomfan', 107).
pokemon_def('rotommow', 107).
pokemon_def('uxie', 130).
pokemon_def('mesprit', 105).
pokemon_def('azelf', 70).
pokemon_def('dialga', 120).
pokemon_def('dialgaorigin', 120).
pokemon_def('palkia', 100).
pokemon_def('palkiaorigin', 100).
pokemon_def('heatran', 106).
pokemon_def('regigigas', 110).
pokemon_def('giratina', 120).
pokemon_def('giratinaorigin', 100).
pokemon_def('cresselia', 110).
pokemon_def('phione', 80).
pokemon_def('manaphy', 100).
pokemon_def('darkrai', 90).
pokemon_def('shaymin', 100).
pokemon_def('shayminsky', 75).
pokemon_def('arceus', 120).
pokemon_def('arceusbug', 120).
pokemon_def('arceusdark', 120).
pokemon_def('arceusdragon', 120).
pokemon_def('arceuselectric', 120).
pokemon_def('arceusfairy', 120).
pokemon_def('arceusfighting', 120).
pokemon_def('arceusfire', 120).
pokemon_def('arceusflying', 120).
pokemon_def('arceusghost', 120).
pokemon_def('arceusgrass', 120).
pokemon_def('arceusground', 120).
pokemon_def('arceusice', 120).
pokemon_def('arceuspoison', 120).
pokemon_def('arceuspsychic', 120).
pokemon_def('arceusrock', 120).
pokemon_def('arceussteel', 120).
pokemon_def('arceuswater', 120).
pokemon_def('victini', 100).
pokemon_def('snivy', 55).
pokemon_def('servine', 75).
pokemon_def('serperior', 95).
pokemon_def('tepig', 45).
pokemon_def('pignite', 55).
pokemon_def('emboar', 65).
pokemon_def('emboarmega', 0).
pokemon_def('oshawott', 45).
pokemon_def('dewott', 60).
pokemon_def('samurott', 85).
pokemon_def('samurotthisui', 80).
pokemon_def('patrat', 39).
pokemon_def('watchog', 69).
pokemon_def('lillipup', 45).
pokemon_def('herdier', 65).
pokemon_def('stoutland', 90).
pokemon_def('purrloin', 37).
pokemon_def('liepard', 50).
pokemon_def('pansage', 48).
pokemon_def('simisage', 63).
pokemon_def('pansear', 48).
pokemon_def('simisear', 63).
pokemon_def('panpour', 48).
pokemon_def('simipour', 63).
pokemon_def('munna', 45).
pokemon_def('musharna', 85).
pokemon_def('pidove', 50).
pokemon_def('tranquill', 62).
pokemon_def('unfezant', 80).
pokemon_def('blitzle', 32).
pokemon_def('zebstrika', 63).
pokemon_def('roggenrola', 85).
pokemon_def('boldore', 105).
pokemon_def('gigalith', 130).
pokemon_def('woobat', 43).
pokemon_def('swoobat', 55).
pokemon_def('drilbur', 40).
pokemon_def('excadrill', 60).
pokemon_def('excadrillmega', 0).
pokemon_def('audino', 86).
pokemon_def('audinomega', 126).
pokemon_def('timburr', 55).
pokemon_def('gurdurr', 85).
pokemon_def('conkeldurr', 95).
pokemon_def('tympole', 40).
pokemon_def('palpitoad', 55).
pokemon_def('seismitoad', 75).
pokemon_def('throh', 85).
pokemon_def('sawk', 75).
pokemon_def('sewaddle', 70).
pokemon_def('swadloon', 90).
pokemon_def('leavanny', 80).
pokemon_def('venipede', 59).
pokemon_def('whirlipede', 99).
pokemon_def('scolipede', 89).
pokemon_def('scolipedemega', 0).
pokemon_def('cottonee', 60).
pokemon_def('whimsicott', 85).
pokemon_def('petilil', 50).
pokemon_def('lilligant', 75).
pokemon_def('lilliganthisui', 75).
pokemon_def('basculin', 65).
pokemon_def('basculinbluestriped', 65).
pokemon_def('basculinwhitestriped', 65).
pokemon_def('sandile', 35).
pokemon_def('krokorok', 45).
pokemon_def('krookodile', 80).
pokemon_def('darumaka', 45).
pokemon_def('darumakagalar', 45).
pokemon_def('darmanitan', 55).
pokemon_def('darmanitanzen', 105).
pokemon_def('darmanitangalar', 55).
pokemon_def('darmanitangalarzen', 55).
pokemon_def('maractus', 67).
pokemon_def('dwebble', 85).
pokemon_def('crustle', 125).
pokemon_def('scraggy', 70).
pokemon_def('scrafty', 115).
pokemon_def('scraftymega', 0).
pokemon_def('sigilyph', 80).
pokemon_def('yamask', 85).
pokemon_def('yamaskgalar', 85).
pokemon_def('cofagrigus', 145).
pokemon_def('tirtouga', 103).
pokemon_def('carracosta', 133).
pokemon_def('archen', 45).
pokemon_def('archeops', 65).
pokemon_def('trubbish', 62).
pokemon_def('garbodor', 82).
pokemon_def('garbodorgmax', 82).
pokemon_def('zorua', 40).
pokemon_def('zoruahisui', 40).
pokemon_def('zoroark', 60).
pokemon_def('zoroarkhisui', 60).
pokemon_def('minccino', 40).
pokemon_def('cinccino', 60).
pokemon_def('gothita', 50).
pokemon_def('gothorita', 70).
pokemon_def('gothitelle', 95).
pokemon_def('solosis', 40).
pokemon_def('duosion', 50).
pokemon_def('reuniclus', 75).
pokemon_def('ducklett', 50).
pokemon_def('swanna', 63).
pokemon_def('vanillite', 50).
pokemon_def('vanillish', 65).
pokemon_def('vanilluxe', 85).
pokemon_def('deerling', 50).
pokemon_def('deerlingsummer', 50).
pokemon_def('deerlingautumn', 50).
pokemon_def('deerlingwinter', 50).
pokemon_def('sawsbuck', 70).
pokemon_def('emolga', 60).
pokemon_def('karrablast', 45).
pokemon_def('escavalier', 105).
pokemon_def('foongus', 45).
pokemon_def('amoonguss', 70).
pokemon_def('frillish', 50).
pokemon_def('jellicent', 70).
pokemon_def('alomomola', 80).
pokemon_def('joltik', 50).
pokemon_def('galvantula', 60).
pokemon_def('ferroseed', 91).
pokemon_def('ferrothorn', 131).
pokemon_def('klink', 70).
pokemon_def('klang', 95).
pokemon_def('klinklang', 115).
pokemon_def('tynamo', 40).
pokemon_def('eelektrik', 70).
pokemon_def('eelektross', 80).
pokemon_def('eelektrossmega', 0).
pokemon_def('elgyem', 55).
pokemon_def('beheeyem', 75).
pokemon_def('litwick', 55).
pokemon_def('lampent', 60).
pokemon_def('chandelure', 90).
pokemon_def('chandeluremega', 0).
pokemon_def('axew', 60).
pokemon_def('fraxure', 70).
pokemon_def('haxorus', 90).
pokemon_def('cubchoo', 40).
pokemon_def('beartic', 80).
pokemon_def('cryogonal', 50).
pokemon_def('shelmet', 85).
pokemon_def('accelgor', 40).
pokemon_def('stunfisk', 84).
pokemon_def('stunfiskgalar', 99).
pokemon_def('mienfoo', 50).
pokemon_def('mienshao', 60).
pokemon_def('druddigon', 90).
pokemon_def('golett', 50).
pokemon_def('golurk', 80).
pokemon_def('pawniard', 70).
pokemon_def('bisharp', 100).
pokemon_def('bouffalant', 95).
pokemon_def('rufflet', 50).
pokemon_def('braviary', 75).
pokemon_def('braviaryhisui', 70).
pokemon_def('vullaby', 75).
pokemon_def('mandibuzz', 105).
pokemon_def('heatmor', 66).
pokemon_def('durant', 112).
pokemon_def('deino', 50).
pokemon_def('zweilous', 70).
pokemon_def('hydreigon', 90).
pokemon_def('larvesta', 55).
pokemon_def('volcarona', 65).
pokemon_def('cobalion', 129).
pokemon_def('terrakion', 90).
pokemon_def('virizion', 72).
pokemon_def('tornadus', 70).
pokemon_def('tornadustherian', 80).
pokemon_def('thundurus', 70).
pokemon_def('thundurustherian', 70).
pokemon_def('reshiram', 100).
pokemon_def('zekrom', 120).
pokemon_def('landorus', 90).
pokemon_def('landorustherian', 90).
pokemon_def('kyurem', 90).
pokemon_def('kyuremblack', 100).
pokemon_def('kyuremwhite', 90).
pokemon_def('keldeo', 90).
pokemon_def('keldeoresolute', 90).
pokemon_def('meloetta', 77).
pokemon_def('meloettapirouette', 90).
pokemon_def('genesect', 95).
pokemon_def('genesectdouse', 95).
pokemon_def('genesectshock', 95).
pokemon_def('genesectburn', 95).
pokemon_def('genesectchill', 95).
pokemon_def('chespin', 65).
pokemon_def('quilladin', 95).
pokemon_def('chesnaught', 122).
pokemon_def('chesnaughtmega', 0).
pokemon_def('fennekin', 40).
pokemon_def('braixen', 58).
pokemon_def('delphox', 72).
pokemon_def('delphoxmega', 0).
pokemon_def('froakie', 40).
pokemon_def('frogadier', 52).
pokemon_def('greninja', 67).
pokemon_def('greninjabond', 67).
pokemon_def('greninjaash', 67).
pokemon_def('greninjamega', 0).
pokemon_def('bunnelby', 38).
pokemon_def('diggersby', 77).
pokemon_def('fletchling', 43).
pokemon_def('fletchinder', 55).
pokemon_def('talonflame', 71).
pokemon_def('scatterbug', 40).
pokemon_def('spewpa', 60).
pokemon_def('vivillon', 50).
pokemon_def('vivillonicysnow', 50).
pokemon_def('vivillonpolar', 50).
pokemon_def('vivillontundra', 50).
pokemon_def('vivilloncontinental', 50).
pokemon_def('vivillongarden', 50).
pokemon_def('vivillonelegant', 50).
pokemon_def('vivillonmodern', 50).
pokemon_def('vivillonmarine', 50).
pokemon_def('vivillonarchipelago', 50).
pokemon_def('vivillonhighplains', 50).
pokemon_def('vivillonsandstorm', 50).
pokemon_def('vivillonriver', 50).
pokemon_def('vivillonmonsoon', 50).
pokemon_def('vivillonsavanna', 50).
pokemon_def('vivillonsun', 50).
pokemon_def('vivillonocean', 50).
pokemon_def('vivillonjungle', 50).
pokemon_def('vivillonfancy', 50).
pokemon_def('vivillonpokeball', 50).
pokemon_def('litleo', 58).
pokemon_def('pyroar', 72).
pokemon_def('pyroarmega', 0).
pokemon_def('flabebe', 39).
pokemon_def('floette', 47).
pokemon_def('floetteeternal', 67).
pokemon_def('floettemega', 0).
pokemon_def('florges', 68).
pokemon_def('skiddo', 48).
pokemon_def('gogoat', 62).
pokemon_def('pancham', 62).
pokemon_def('pangoro', 78).
pokemon_def('furfrou', 60).
pokemon_def('espurr', 54).
pokemon_def('meowstic', 76).
pokemon_def('meowsticf', 76).
pokemon_def('honedge', 100).
pokemon_def('doublade', 150).
pokemon_def('aegislash', 140).
pokemon_def('aegislashblade', 50).
pokemon_def('spritzee', 60).
pokemon_def('aromatisse', 72).
pokemon_def('swirlix', 66).
pokemon_def('slurpuff', 86).
pokemon_def('inkay', 53).
pokemon_def('malamar', 88).
pokemon_def('malamarmega', 0).
pokemon_def('binacle', 67).
pokemon_def('barbaracle', 115).
pokemon_def('barbaraclemega', 0).
pokemon_def('skrelp', 60).
pokemon_def('dragalge', 90).
pokemon_def('dragalgemega', 0).
pokemon_def('clauncher', 62).
pokemon_def('clawitzer', 88).
pokemon_def('helioptile', 33).
pokemon_def('heliolisk', 52).
pokemon_def('tyrunt', 77).
pokemon_def('tyrantrum', 119).
pokemon_def('amaura', 50).
pokemon_def('aurorus', 72).
pokemon_def('sylveon', 65).
pokemon_def('hawlucha', 75).
pokemon_def('hawluchamega', 0).
pokemon_def('dedenne', 57).
pokemon_def('carbink', 150).
pokemon_def('goomy', 35).
pokemon_def('sliggoo', 53).
pokemon_def('sliggoohisui', 83).
pokemon_def('goodra', 70).
pokemon_def('goodrahisui', 100).
pokemon_def('klefki', 91).
pokemon_def('phantump', 48).
pokemon_def('trevenant', 76).
pokemon_def('pumpkaboo', 70).
pokemon_def('pumpkaboosmall', 70).
pokemon_def('pumpkaboolarge', 70).
pokemon_def('pumpkaboosuper', 70).
pokemon_def('gourgeist', 122).
pokemon_def('gourgeistsmall', 122).
pokemon_def('gourgeistlarge', 122).
pokemon_def('gourgeistsuper', 122).
pokemon_def('bergmite', 85).
pokemon_def('avalugg', 184).
pokemon_def('avalugghisui', 184).
pokemon_def('noibat', 35).
pokemon_def('noivern', 80).
pokemon_def('xerneas', 95).
pokemon_def('xerneasneutral', 95).
pokemon_def('yveltal', 95).
pokemon_def('zygarde', 121).
pokemon_def('zygarde10', 71).
pokemon_def('zygardecomplete', 121).
pokemon_def('zygardemega', 0).
pokemon_def('diancie', 150).
pokemon_def('dianciemega', 110).
pokemon_def('hoopa', 60).
pokemon_def('hoopaunbound', 60).
pokemon_def('volcanion', 120).
pokemon_def('rowlet', 55).
pokemon_def('dartrix', 75).
pokemon_def('decidueye', 75).
pokemon_def('decidueyehisui', 80).
pokemon_def('litten', 40).
pokemon_def('torracat', 50).
pokemon_def('incineroar', 90).
pokemon_def('popplio', 54).
pokemon_def('brionne', 69).
pokemon_def('primarina', 74).
pokemon_def('pikipek', 30).
pokemon_def('trumbeak', 50).
pokemon_def('toucannon', 75).
pokemon_def('yungoos', 30).
pokemon_def('gumshoos', 60).
pokemon_def('gumshoostotem', 60).
pokemon_def('grubbin', 45).
pokemon_def('charjabug', 95).
pokemon_def('vikavolt', 90).
pokemon_def('vikavolttotem', 90).
pokemon_def('crabrawler', 57).
pokemon_def('crabominable', 77).
pokemon_def('oricorio', 70).
pokemon_def('oricoriopompom', 70).
pokemon_def('oricoriopau', 70).
pokemon_def('oricoriosensu', 70).
pokemon_def('cutiefly', 40).
pokemon_def('ribombee', 60).
pokemon_def('ribombeetotem', 60).
pokemon_def('rockruff', 40).
pokemon_def('rockruffdusk', 40).
pokemon_def('lycanroc', 65).
pokemon_def('lycanrocmidnight', 75).
pokemon_def('lycanrocdusk', 65).
pokemon_def('wishiwashi', 20).
pokemon_def('wishiwashischool', 130).
pokemon_def('mareanie', 62).
pokemon_def('toxapex', 152).
pokemon_def('mudbray', 70).
pokemon_def('mudsdale', 100).
pokemon_def('dewpider', 52).
pokemon_def('araquanid', 92).
pokemon_def('araquanidtotem', 92).
pokemon_def('fomantis', 35).
pokemon_def('lurantis', 90).
pokemon_def('lurantistotem', 90).
pokemon_def('morelull', 55).
pokemon_def('shiinotic', 80).
pokemon_def('salandit', 40).
pokemon_def('salazzle', 60).
pokemon_def('salazzletotem', 60).
pokemon_def('stufful', 50).
pokemon_def('bewear', 80).
pokemon_def('bounsweet', 38).
pokemon_def('steenee', 48).
pokemon_def('tsareena', 98).
pokemon_def('comfey', 90).
pokemon_def('oranguru', 80).
pokemon_def('passimian', 90).
pokemon_def('wimpod', 40).
pokemon_def('golisopod', 140).
pokemon_def('sandygast', 80).
pokemon_def('palossand', 110).
pokemon_def('pyukumuku', 130).
pokemon_def('typenull', 95).
pokemon_def('silvally', 95).
pokemon_def('silvallybug', 95).
pokemon_def('silvallydark', 95).
pokemon_def('silvallydragon', 95).
pokemon_def('silvallyelectric', 95).
pokemon_def('silvallyfairy', 95).
pokemon_def('silvallyfighting', 95).
pokemon_def('silvallyfire', 95).
pokemon_def('silvallyflying', 95).
pokemon_def('silvallyghost', 95).
pokemon_def('silvallygrass', 95).
pokemon_def('silvallyground', 95).
pokemon_def('silvallyice', 95).
pokemon_def('silvallypoison', 95).
pokemon_def('silvallypsychic', 95).
pokemon_def('silvallyrock', 95).
pokemon_def('silvallysteel', 95).
pokemon_def('silvallywater', 95).
pokemon_def('minior', 60).
pokemon_def('miniororange', 60).
pokemon_def('minioryellow', 60).
pokemon_def('miniorgreen', 60).
pokemon_def('miniorblue', 60).
pokemon_def('miniorindigo', 60).
pokemon_def('miniorviolet', 60).
pokemon_def('miniormeteor', 100).
pokemon_def('komala', 65).
pokemon_def('turtonator', 135).
pokemon_def('togedemaru', 63).
pokemon_def('togedemarutotem', 63).
pokemon_def('mimikyu', 80).
pokemon_def('mimikyubusted', 80).
pokemon_def('mimikyutotem', 80).
pokemon_def('mimikyubustedtotem', 80).
pokemon_def('bruxish', 70).
pokemon_def('drampa', 85).
pokemon_def('drampamega', 0).
pokemon_def('dhelmise', 100).
pokemon_def('jangmoo', 65).
pokemon_def('hakamoo', 90).
pokemon_def('kommoo', 125).
pokemon_def('kommoototem', 125).
pokemon_def('tapukoko', 85).
pokemon_def('tapulele', 75).
pokemon_def('tapubulu', 115).
pokemon_def('tapufini', 115).
pokemon_def('cosmog', 31).
pokemon_def('cosmoem', 131).
pokemon_def('solgaleo', 107).
pokemon_def('lunala', 89).
pokemon_def('nihilego', 47).
pokemon_def('buzzwole', 139).
pokemon_def('pheromosa', 37).
pokemon_def('xurkitree', 71).
pokemon_def('celesteela', 103).
pokemon_def('kartana', 131).
pokemon_def('guzzlord', 53).
pokemon_def('necrozma', 101).
pokemon_def('necrozmaduskmane', 127).
pokemon_def('necrozmadawnwings', 109).
pokemon_def('necrozmaultra', 97).
pokemon_def('magearna', 115).
pokemon_def('magearnaoriginal', 115).
pokemon_def('marshadow', 80).
pokemon_def('poipole', 67).
pokemon_def('naganadel', 73).
pokemon_def('stakataka', 211).
pokemon_def('blacephalon', 53).
pokemon_def('zeraora', 75).
pokemon_def('meltan', 65).
pokemon_def('melmetal', 143).
pokemon_def('melmetalgmax', 143).
pokemon_def('grookey', 50).
pokemon_def('thwackey', 70).
pokemon_def('rillaboom', 90).
pokemon_def('rillaboomgmax', 90).
pokemon_def('scorbunny', 40).
pokemon_def('raboot', 60).
pokemon_def('cinderace', 75).
pokemon_def('cinderacegmax', 75).
pokemon_def('sobble', 40).
pokemon_def('drizzile', 55).
pokemon_def('inteleon', 65).
pokemon_def('inteleongmax', 65).
pokemon_def('skwovet', 55).
pokemon_def('greedent', 95).
pokemon_def('rookidee', 35).
pokemon_def('corvisquire', 55).
pokemon_def('corviknight', 105).
pokemon_def('corviknightgmax', 105).
pokemon_def('blipbug', 20).
pokemon_def('dottler', 80).
pokemon_def('orbeetle', 110).
pokemon_def('orbeetlegmax', 110).
pokemon_def('nickit', 28).
pokemon_def('thievul', 58).
pokemon_def('gossifleur', 60).
pokemon_def('eldegoss', 90).
pokemon_def('wooloo', 55).
pokemon_def('dubwool', 100).
pokemon_def('chewtle', 50).
pokemon_def('drednaw', 90).
pokemon_def('drednawgmax', 90).
pokemon_def('yamper', 50).
pokemon_def('boltund', 60).
pokemon_def('rolycoly', 50).
pokemon_def('carkol', 90).
pokemon_def('coalossal', 120).
pokemon_def('coalossalgmax', 120).
pokemon_def('applin', 80).
pokemon_def('flapple', 80).
pokemon_def('flapplegmax', 80).
pokemon_def('appletun', 80).
pokemon_def('appletungmax', 80).
pokemon_def('silicobra', 75).
pokemon_def('sandaconda', 125).
pokemon_def('sandacondagmax', 125).
pokemon_def('cramorant', 55).
pokemon_def('cramorantgulping', 55).
pokemon_def('cramorantgorging', 55).
pokemon_def('arrokuda', 40).
pokemon_def('barraskewda', 60).
pokemon_def('toxel', 35).
pokemon_def('toxtricity', 70).
pokemon_def('toxtricitylowkey', 70).
pokemon_def('toxtricitygmax', 70).
pokemon_def('toxtricitylowkeygmax', 70).
pokemon_def('sizzlipede', 45).
pokemon_def('centiskorch', 65).
pokemon_def('centiskorchgmax', 65).
pokemon_def('clobbopus', 60).
pokemon_def('grapploct', 90).
pokemon_def('sinistea', 45).
pokemon_def('sinisteaantique', 45).
pokemon_def('polteageist', 65).
pokemon_def('polteageistantique', 65).
pokemon_def('hatenna', 45).
pokemon_def('hattrem', 65).
pokemon_def('hatterene', 95).
pokemon_def('hatterenegmax', 95).
pokemon_def('impidimp', 30).
pokemon_def('morgrem', 45).
pokemon_def('grimmsnarl', 65).
pokemon_def('grimmsnarlgmax', 65).
pokemon_def('obstagoon', 101).
pokemon_def('perrserker', 100).
pokemon_def('cursola', 50).
pokemon_def('sirfetchd', 95).
pokemon_def('mrrime', 75).
pokemon_def('runerigus', 145).
pokemon_def('milcery', 40).
pokemon_def('alcremie', 75).
pokemon_def('alcremierubycream', 75).
pokemon_def('alcremiematchacream', 75).
pokemon_def('alcremiemintcream', 75).
pokemon_def('alcremielemoncream', 75).
pokemon_def('alcremierubyswirl', 75).
pokemon_def('alcremiecaramelswirl', 75).
pokemon_def('alcremierainbowswirl', 75).
pokemon_def('alcremiegmax', 75).
pokemon_def('falinks', 100).
pokemon_def('falinksmega', 0).
pokemon_def('pincurchin', 95).
pokemon_def('snom', 35).
pokemon_def('frosmoth', 60).
pokemon_def('stonjourner', 135).
pokemon_def('eiscue', 110).
pokemon_def('eiscuenoice', 70).
pokemon_def('indeedee', 55).
pokemon_def('indeedeef', 65).
pokemon_def('morpeko', 58).
pokemon_def('morpekohangry', 58).
pokemon_def('cufant', 49).
pokemon_def('copperajah', 69).
pokemon_def('copperajahgmax', 69).
pokemon_def('dracozolt', 90).
pokemon_def('arctozolt', 90).
pokemon_def('dracovish', 100).
pokemon_def('arctovish', 100).
pokemon_def('duraludon', 115).
pokemon_def('duraludongmax', 115).
pokemon_def('dreepy', 30).
pokemon_def('drakloak', 50).
pokemon_def('dragapult', 75).
pokemon_def('zacian', 115).
pokemon_def('zaciancrowned', 115).
pokemon_def('zamazenta', 115).
pokemon_def('zamazentacrowned', 140).
pokemon_def('eternatus', 95).
pokemon_def('eternatuseternamax', 250).
pokemon_def('kubfu', 60).
pokemon_def('urshifu', 100).
pokemon_def('urshifurapidstrike', 100).
pokemon_def('urshifugmax', 100).
pokemon_def('urshifurapidstrikegmax', 100).
pokemon_def('zarude', 105).
pokemon_def('zarudedada', 105).
pokemon_def('regieleki', 50).
pokemon_def('regidrago', 50).
pokemon_def('glastrier', 130).
pokemon_def('spectrier', 60).
pokemon_def('calyrex', 80).
pokemon_def('calyrexice', 150).
pokemon_def('calyrexshadow', 80).
pokemon_def('wyrdeer', 72).
pokemon_def('kleavor', 95).
pokemon_def('ursaluna', 105).
pokemon_def('ursalunabloodmoon', 120).
pokemon_def('basculegion', 65).
pokemon_def('basculegionf', 65).
pokemon_def('sneasler', 60).
pokemon_def('overqwil', 95).
pokemon_def('enamorus', 70).
pokemon_def('enamorustherian', 110).
pokemon_def('sprigatito', 54).
pokemon_def('floragato', 63).
pokemon_def('meowscarada', 70).
pokemon_def('fuecoco', 59).
pokemon_def('crocalor', 78).
pokemon_def('skeledirge', 100).
pokemon_def('quaxly', 45).
pokemon_def('quaxwell', 65).
pokemon_def('quaquaval', 80).
pokemon_def('lechonk', 40).
pokemon_def('oinkologne', 75).
pokemon_def('oinkolognef', 70).
pokemon_def('tarountula', 45).
pokemon_def('spidops', 92).
pokemon_def('nymble', 40).
pokemon_def('lokix', 78).
pokemon_def('pawmi', 20).
pokemon_def('pawmo', 40).
pokemon_def('pawmot', 70).
pokemon_def('tandemaus', 45).
pokemon_def('maushold', 70).
pokemon_def('mausholdfour', 70).
pokemon_def('fidough', 70).
pokemon_def('dachsbun', 115).
pokemon_def('smoliv', 45).
pokemon_def('dolliv', 60).
pokemon_def('arboliva', 90).
pokemon_def('squawkabilly', 51).
pokemon_def('squawkabillyblue', 51).
pokemon_def('squawkabillyyellow', 51).
pokemon_def('squawkabillywhite', 51).
pokemon_def('nacli', 75).
pokemon_def('naclstack', 100).
pokemon_def('garganacl', 130).
pokemon_def('charcadet', 40).
pokemon_def('armarouge', 100).
pokemon_def('ceruledge', 80).
pokemon_def('tadbulb', 41).
pokemon_def('bellibolt', 91).
pokemon_def('wattrel', 35).
pokemon_def('kilowattrel', 60).
pokemon_def('maschiff', 60).
pokemon_def('mabosstiff', 90).
pokemon_def('shroodle', 35).
pokemon_def('grafaiai', 65).
pokemon_def('bramblin', 30).
pokemon_def('brambleghast', 70).
pokemon_def('toedscool', 35).
pokemon_def('toedscruel', 65).
pokemon_def('klawf', 115).
pokemon_def('capsakid', 40).
pokemon_def('scovillain', 65).
pokemon_def('rellor', 60).
pokemon_def('rabsca', 85).
pokemon_def('flittle', 30).
pokemon_def('espathra', 60).
pokemon_def('tinkatink', 45).
pokemon_def('tinkatuff', 55).
pokemon_def('tinkaton', 77).
pokemon_def('wiglett', 25).
pokemon_def('wugtrio', 50).
pokemon_def('bombirdier', 85).
pokemon_def('finizen', 40).
pokemon_def('palafin', 72).
pokemon_def('palafinhero', 97).
pokemon_def('varoom', 63).
pokemon_def('revavroom', 90).
pokemon_def('cyclizar', 65).
pokemon_def('orthworm', 145).
pokemon_def('glimmet', 42).
pokemon_def('glimmora', 90).
pokemon_def('greavard', 60).
pokemon_def('houndstone', 100).
pokemon_def('flamigo', 74).
pokemon_def('cetoddle', 45).
pokemon_def('cetitan', 65).
pokemon_def('veluza', 73).
pokemon_def('dondozo', 115).
pokemon_def('tatsugiri', 60).
pokemon_def('tatsugiridroopy', 60).
pokemon_def('tatsugiristretchy', 60).
pokemon_def('annihilape', 80).
pokemon_def('clodsire', 60).
pokemon_def('farigiraf', 70).
pokemon_def('dudunsparce', 80).
pokemon_def('dudunsparcethreesegment', 80).
pokemon_def('kingambit', 120).
pokemon_def('greattusk', 131).
pokemon_def('screamtail', 99).
pokemon_def('brutebonnet', 99).
pokemon_def('fluttermane', 55).
pokemon_def('slitherwing', 79).
pokemon_def('sandyshocks', 97).
pokemon_def('irontreads', 120).
pokemon_def('ironbundle', 114).
pokemon_def('ironhands', 108).
pokemon_def('ironjugulis', 86).
pokemon_def('ironmoth', 60).
pokemon_def('ironthorns', 110).
pokemon_def('frigibax', 45).
pokemon_def('arctibax', 66).
pokemon_def('baxcalibur', 92).
pokemon_def('gimmighoul', 70).
pokemon_def('gimmighoulroaming', 25).
pokemon_def('gholdengo', 95).
pokemon_def('wochien', 100).
pokemon_def('chienpao', 80).
pokemon_def('tinglu', 125).
pokemon_def('chiyu', 80).
pokemon_def('roaringmoon', 71).
pokemon_def('ironvaliant', 90).
pokemon_def('koraidon', 115).
pokemon_def('miraidon', 100).
pokemon_def('walkingwake', 91).
pokemon_def('ironleaves', 88).
pokemon_def('dipplin', 110).
pokemon_def('poltchageist', 45).
pokemon_def('poltchageistartisan', 45).
pokemon_def('sinistcha', 106).
pokemon_def('sinistchamasterpiece', 106).
pokemon_def('okidogi', 115).
pokemon_def('munkidori', 66).
pokemon_def('fezandipiti', 82).
pokemon_def('ogerpon', 84).
pokemon_def('ogerponwellspring', 84).
pokemon_def('ogerponhearthflame', 84).
pokemon_def('ogerponcornerstone', 84).
pokemon_def('ogerpontealtera', 84).
pokemon_def('ogerponwellspringtera', 84).
pokemon_def('ogerponhearthflametera', 84).
pokemon_def('ogerponcornerstonetera', 84).
pokemon_def('archaludon', 130).
pokemon_def('hydrapple', 110).
pokemon_def('gougingfire', 121).
pokemon_def('ragingbolt', 91).
pokemon_def('ironboulder', 80).
pokemon_def('ironcrown', 100).
pokemon_def('terapagos', 85).
pokemon_def('terapagosterastal', 110).
pokemon_def('terapagosstellar', 110).
pokemon_def('pecharunt', 160).
pokemon_def('missingno', 0).
pokemon_def('ramnarok', 0).
pokemon_def('ramnarokradiant', 0).
pokemon_def('pokestarsmeargle', 35).
pokemon_def('pokestarufo', 100).
pokemon_def('pokestarufo2', 100).
pokemon_def('pokestarbrycenman', 100).
pokemon_def('pokestarmt', 100).
pokemon_def('pokestarmt2', 100).
pokemon_def('pokestartransport', 100).
pokemon_def('pokestargiant', 100).
pokemon_def('pokestarhumanoid', 100).
pokemon_def('pokestarmonster', 100).
pokemon_def('pokestarf00', 100).
pokemon_def('pokestarf002', 100).
pokemon_def('pokestarspirit', 100).
pokemon_def('pokestarblackdoor', 100).
pokemon_def('pokestarwhitedoor', 100).
pokemon_def('pokestarblackbelt', 100).
pokemon_def('pokestarufopropu2', 100).
pokemon_spa('bulbasaur', 65).
pokemon_spa('ivysaur', 80).
pokemon_spa('venusaur', 100).
pokemon_spa('venusaurmega', 122).
pokemon_spa('venusaurgmax', 100).
pokemon_spa('charmander', 60).
pokemon_spa('charmeleon', 80).
pokemon_spa('charizard', 109).
pokemon_spa('charizardmegax', 130).
pokemon_spa('charizardmegay', 159).
pokemon_spa('charizardgmax', 109).
pokemon_spa('squirtle', 50).
pokemon_spa('wartortle', 65).
pokemon_spa('blastoise', 85).
pokemon_spa('blastoisemega', 135).
pokemon_spa('blastoisegmax', 85).
pokemon_spa('caterpie', 20).
pokemon_spa('metapod', 25).
pokemon_spa('butterfree', 90).
pokemon_spa('butterfreegmax', 90).
pokemon_spa('weedle', 20).
pokemon_spa('kakuna', 25).
pokemon_spa('beedrill', 45).
pokemon_spa('beedrillmega', 15).
pokemon_spa('pidgey', 35).
pokemon_spa('pidgeotto', 50).
pokemon_spa('pidgeot', 70).
pokemon_spa('pidgeotmega', 135).
pokemon_spa('rattata', 25).
pokemon_spa('rattataalola', 25).
pokemon_spa('raticate', 50).
pokemon_spa('raticatealola', 40).
pokemon_spa('raticatealolatotem', 40).
pokemon_spa('spearow', 31).
pokemon_spa('fearow', 61).
pokemon_spa('ekans', 40).
pokemon_spa('arbok', 65).
pokemon_spa('pikachu', 50).
pokemon_spa('pikachucosplay', 50).
pokemon_spa('pikachurockstar', 50).
pokemon_spa('pikachubelle', 50).
pokemon_spa('pikachupopstar', 50).
pokemon_spa('pikachuphd', 50).
pokemon_spa('pikachulibre', 50).
pokemon_spa('pikachuoriginal', 50).
pokemon_spa('pikachuhoenn', 50).
pokemon_spa('pikachusinnoh', 50).
pokemon_spa('pikachuunova', 50).
pokemon_spa('pikachukalos', 50).
pokemon_spa('pikachualola', 50).
pokemon_spa('pikachupartner', 50).
pokemon_spa('pikachustarter', 75).
pokemon_spa('pikachugmax', 50).
pokemon_spa('pikachuworld', 50).
pokemon_spa('raichu', 90).
pokemon_spa('raichualola', 95).
pokemon_spa('sandshrew', 20).
pokemon_spa('sandshrewalola', 10).
pokemon_spa('sandslash', 45).
pokemon_spa('sandslashalola', 25).
pokemon_spa('nidoranf', 40).
pokemon_spa('nidorina', 55).
pokemon_spa('nidoqueen', 75).
pokemon_spa('nidoranm', 40).
pokemon_spa('nidorino', 55).
pokemon_spa('nidoking', 85).
pokemon_spa('clefairy', 60).
pokemon_spa('clefable', 95).
pokemon_spa('clefablemega', 0).
pokemon_spa('vulpix', 50).
pokemon_spa('vulpixalola', 50).
pokemon_spa('ninetales', 81).
pokemon_spa('ninetalesalola', 81).
pokemon_spa('jigglypuff', 45).
pokemon_spa('wigglytuff', 85).
pokemon_spa('zubat', 30).
pokemon_spa('golbat', 65).
pokemon_spa('oddish', 75).
pokemon_spa('gloom', 85).
pokemon_spa('vileplume', 110).
pokemon_spa('paras', 45).
pokemon_spa('parasect', 60).
pokemon_spa('venonat', 40).
pokemon_spa('venomoth', 90).
pokemon_spa('diglett', 35).
pokemon_spa('diglettalola', 35).
pokemon_spa('dugtrio', 50).
pokemon_spa('dugtrioalola', 50).
pokemon_spa('meowth', 40).
pokemon_spa('meowthalola', 50).
pokemon_spa('meowthgalar', 40).
pokemon_spa('meowthgmax', 40).
pokemon_spa('persian', 65).
pokemon_spa('persianalola', 75).
pokemon_spa('psyduck', 65).
pokemon_spa('golduck', 95).
pokemon_spa('mankey', 35).
pokemon_spa('primeape', 60).
pokemon_spa('growlithe', 70).
pokemon_spa('growlithehisui', 65).
pokemon_spa('arcanine', 100).
pokemon_spa('arcaninehisui', 95).
pokemon_spa('poliwag', 40).
pokemon_spa('poliwhirl', 50).
pokemon_spa('poliwrath', 70).
pokemon_spa('abra', 105).
pokemon_spa('kadabra', 120).
pokemon_spa('alakazam', 135).
pokemon_spa('alakazammega', 175).
pokemon_spa('machop', 35).
pokemon_spa('machoke', 50).
pokemon_spa('machamp', 65).
pokemon_spa('machampgmax', 65).
pokemon_spa('bellsprout', 70).
pokemon_spa('weepinbell', 85).
pokemon_spa('victreebel', 100).
pokemon_spa('victreebelmega', 0).
pokemon_spa('tentacool', 50).
pokemon_spa('tentacruel', 80).
pokemon_spa('geodude', 30).
pokemon_spa('geodudealola', 30).
pokemon_spa('graveler', 45).
pokemon_spa('graveleralola', 45).
pokemon_spa('golem', 55).
pokemon_spa('golemalola', 55).
pokemon_spa('ponyta', 65).
pokemon_spa('ponytagalar', 65).
pokemon_spa('rapidash', 80).
pokemon_spa('rapidashgalar', 80).
pokemon_spa('slowpoke', 40).
pokemon_spa('slowpokegalar', 40).
pokemon_spa('slowbro', 100).
pokemon_spa('slowbromega', 130).
pokemon_spa('slowbrogalar', 100).
pokemon_spa('magnemite', 95).
pokemon_spa('magneton', 120).
pokemon_spa('farfetchd', 58).
pokemon_spa('farfetchdgalar', 58).
pokemon_spa('doduo', 35).
pokemon_spa('dodrio', 60).
pokemon_spa('seel', 45).
pokemon_spa('dewgong', 70).
pokemon_spa('grimer', 40).
pokemon_spa('grimeralola', 40).
pokemon_spa('muk', 65).
pokemon_spa('mukalola', 65).
pokemon_spa('shellder', 45).
pokemon_spa('cloyster', 85).
pokemon_spa('gastly', 100).
pokemon_spa('haunter', 115).
pokemon_spa('gengar', 130).
pokemon_spa('gengarmega', 170).
pokemon_spa('gengargmax', 130).
pokemon_spa('onix', 30).
pokemon_spa('drowzee', 43).
pokemon_spa('hypno', 73).
pokemon_spa('krabby', 25).
pokemon_spa('kingler', 50).
pokemon_spa('kinglergmax', 50).
pokemon_spa('voltorb', 55).
pokemon_spa('voltorbhisui', 55).
pokemon_spa('electrode', 80).
pokemon_spa('electrodehisui', 80).
pokemon_spa('exeggcute', 60).
pokemon_spa('exeggutor', 125).
pokemon_spa('exeggutoralola', 125).
pokemon_spa('cubone', 40).
pokemon_spa('marowak', 50).
pokemon_spa('marowakalola', 50).
pokemon_spa('marowakalolatotem', 50).
pokemon_spa('hitmonlee', 35).
pokemon_spa('hitmonchan', 35).
pokemon_spa('lickitung', 60).
pokemon_spa('koffing', 60).
pokemon_spa('weezing', 85).
pokemon_spa('weezinggalar', 85).
pokemon_spa('rhyhorn', 30).
pokemon_spa('rhydon', 45).
pokemon_spa('chansey', 35).
pokemon_spa('tangela', 100).
pokemon_spa('kangaskhan', 40).
pokemon_spa('kangaskhanmega', 60).
pokemon_spa('horsea', 70).
pokemon_spa('seadra', 95).
pokemon_spa('goldeen', 35).
pokemon_spa('seaking', 65).
pokemon_spa('staryu', 70).
pokemon_spa('starmie', 100).
pokemon_spa('starmiemega', 0).
pokemon_spa('mrmime', 100).
pokemon_spa('mrmimegalar', 90).
pokemon_spa('scyther', 55).
pokemon_spa('jynx', 115).
pokemon_spa('electabuzz', 95).
pokemon_spa('magmar', 100).
pokemon_spa('pinsir', 55).
pokemon_spa('pinsirmega', 65).
pokemon_spa('tauros', 40).
pokemon_spa('taurospaldeacombat', 30).
pokemon_spa('taurospaldeablaze', 30).
pokemon_spa('taurospaldeaaqua', 30).
pokemon_spa('magikarp', 15).
pokemon_spa('gyarados', 60).
pokemon_spa('gyaradosmega', 70).
pokemon_spa('lapras', 85).
pokemon_spa('laprasgmax', 85).
pokemon_spa('ditto', 48).
pokemon_spa('eevee', 45).
pokemon_spa('eeveestarter', 65).
pokemon_spa('eeveegmax', 45).
pokemon_spa('vaporeon', 110).
pokemon_spa('jolteon', 110).
pokemon_spa('flareon', 95).
pokemon_spa('porygon', 85).
pokemon_spa('omanyte', 90).
pokemon_spa('omastar', 115).
pokemon_spa('kabuto', 55).
pokemon_spa('kabutops', 65).
pokemon_spa('aerodactyl', 60).
pokemon_spa('aerodactylmega', 70).
pokemon_spa('snorlax', 65).
pokemon_spa('snorlaxgmax', 65).
pokemon_spa('articuno', 95).
pokemon_spa('articunogalar', 125).
pokemon_spa('zapdos', 125).
pokemon_spa('zapdosgalar', 85).
pokemon_spa('moltres', 125).
pokemon_spa('moltresgalar', 100).
pokemon_spa('dratini', 50).
pokemon_spa('dragonair', 70).
pokemon_spa('dragonite', 100).
pokemon_spa('dragonitemega', 0).
pokemon_spa('mewtwo', 154).
pokemon_spa('mewtwomegax', 154).
pokemon_spa('mewtwomegay', 194).
pokemon_spa('mew', 100).
pokemon_spa('chikorita', 49).
pokemon_spa('bayleef', 63).
pokemon_spa('meganium', 83).
pokemon_spa('meganiummega', 0).
pokemon_spa('cyndaquil', 60).
pokemon_spa('quilava', 80).
pokemon_spa('typhlosion', 109).
pokemon_spa('typhlosionhisui', 119).
pokemon_spa('totodile', 44).
pokemon_spa('croconaw', 59).
pokemon_spa('feraligatr', 79).
pokemon_spa('feraligatrmega', 0).
pokemon_spa('sentret', 35).
pokemon_spa('furret', 45).
pokemon_spa('hoothoot', 36).
pokemon_spa('noctowl', 86).
pokemon_spa('ledyba', 40).
pokemon_spa('ledian', 55).
pokemon_spa('spinarak', 40).
pokemon_spa('ariados', 60).
pokemon_spa('crobat', 70).
pokemon_spa('chinchou', 56).
pokemon_spa('lanturn', 76).
pokemon_spa('pichu', 35).
pokemon_spa('pichuspikyeared', 35).
pokemon_spa('cleffa', 45).
pokemon_spa('igglybuff', 40).
pokemon_spa('togepi', 40).
pokemon_spa('togetic', 80).
pokemon_spa('natu', 70).
pokemon_spa('xatu', 95).
pokemon_spa('mareep', 65).
pokemon_spa('flaaffy', 80).
pokemon_spa('ampharos', 115).
pokemon_spa('ampharosmega', 165).
pokemon_spa('bellossom', 90).
pokemon_spa('marill', 20).
pokemon_spa('azumarill', 60).
pokemon_spa('sudowoodo', 30).
pokemon_spa('politoed', 90).
pokemon_spa('hoppip', 35).
pokemon_spa('skiploom', 45).
pokemon_spa('jumpluff', 55).
pokemon_spa('aipom', 40).
pokemon_spa('sunkern', 30).
pokemon_spa('sunflora', 105).
pokemon_spa('yanma', 75).
pokemon_spa('wooper', 25).
pokemon_spa('wooperpaldea', 25).
pokemon_spa('quagsire', 65).
pokemon_spa('espeon', 130).
pokemon_spa('umbreon', 60).
pokemon_spa('murkrow', 85).
pokemon_spa('slowking', 100).
pokemon_spa('slowkinggalar', 110).
pokemon_spa('misdreavus', 85).
pokemon_spa('unown', 72).
pokemon_spa('wobbuffet', 33).
pokemon_spa('girafarig', 90).
pokemon_spa('pineco', 35).
pokemon_spa('forretress', 60).
pokemon_spa('dunsparce', 65).
pokemon_spa('gligar', 35).
pokemon_spa('steelix', 55).
pokemon_spa('steelixmega', 55).
pokemon_spa('snubbull', 40).
pokemon_spa('granbull', 60).
pokemon_spa('qwilfish', 55).
pokemon_spa('qwilfishhisui', 55).
pokemon_spa('scizor', 55).
pokemon_spa('scizormega', 65).
pokemon_spa('shuckle', 10).
pokemon_spa('heracross', 40).
pokemon_spa('heracrossmega', 40).
pokemon_spa('sneasel', 35).
pokemon_spa('sneaselhisui', 35).
pokemon_spa('teddiursa', 50).
pokemon_spa('ursaring', 75).
pokemon_spa('slugma', 70).
pokemon_spa('magcargo', 90).
pokemon_spa('swinub', 30).
pokemon_spa('piloswine', 60).
pokemon_spa('corsola', 65).
pokemon_spa('corsolagalar', 65).
pokemon_spa('remoraid', 65).
pokemon_spa('octillery', 105).
pokemon_spa('delibird', 65).
pokemon_spa('mantine', 80).
pokemon_spa('skarmory', 40).
pokemon_spa('skarmorymega', 0).
pokemon_spa('houndour', 80).
pokemon_spa('houndoom', 110).
pokemon_spa('houndoommega', 140).
pokemon_spa('kingdra', 95).
pokemon_spa('phanpy', 40).
pokemon_spa('donphan', 60).
pokemon_spa('porygon2', 105).
pokemon_spa('stantler', 85).
pokemon_spa('smeargle', 20).
pokemon_spa('tyrogue', 35).
pokemon_spa('hitmontop', 35).
pokemon_spa('smoochum', 85).
pokemon_spa('elekid', 65).
pokemon_spa('magby', 70).
pokemon_spa('miltank', 40).
pokemon_spa('blissey', 75).
pokemon_spa('raikou', 115).
pokemon_spa('entei', 90).
pokemon_spa('suicune', 90).
pokemon_spa('larvitar', 45).
pokemon_spa('pupitar', 65).
pokemon_spa('tyranitar', 95).
pokemon_spa('tyranitarmega', 95).
pokemon_spa('lugia', 90).
pokemon_spa('hooh', 110).
pokemon_spa('celebi', 100).
pokemon_spa('treecko', 65).
pokemon_spa('grovyle', 85).
pokemon_spa('sceptile', 105).
pokemon_spa('sceptilemega', 145).
pokemon_spa('torchic', 70).
pokemon_spa('combusken', 85).
pokemon_spa('blaziken', 110).
pokemon_spa('blazikenmega', 130).
pokemon_spa('mudkip', 50).
pokemon_spa('marshtomp', 60).
pokemon_spa('swampert', 85).
pokemon_spa('swampertmega', 95).
pokemon_spa('poochyena', 30).
pokemon_spa('mightyena', 60).
pokemon_spa('zigzagoon', 30).
pokemon_spa('zigzagoongalar', 30).
pokemon_spa('linoone', 50).
pokemon_spa('linoonegalar', 50).
pokemon_spa('wurmple', 20).
pokemon_spa('silcoon', 25).
pokemon_spa('beautifly', 100).
pokemon_spa('cascoon', 25).
pokemon_spa('dustox', 50).
pokemon_spa('lotad', 40).
pokemon_spa('lombre', 60).
pokemon_spa('ludicolo', 90).
pokemon_spa('seedot', 30).
pokemon_spa('nuzleaf', 60).
pokemon_spa('shiftry', 90).
pokemon_spa('taillow', 30).
pokemon_spa('swellow', 75).
pokemon_spa('wingull', 55).
pokemon_spa('pelipper', 95).
pokemon_spa('ralts', 45).
pokemon_spa('kirlia', 65).
pokemon_spa('gardevoir', 125).
pokemon_spa('gardevoirmega', 165).
pokemon_spa('surskit', 50).
pokemon_spa('masquerain', 100).
pokemon_spa('shroomish', 40).
pokemon_spa('breloom', 60).
pokemon_spa('slakoth', 35).
pokemon_spa('vigoroth', 55).
pokemon_spa('slaking', 95).
pokemon_spa('nincada', 30).
pokemon_spa('ninjask', 50).
pokemon_spa('shedinja', 30).
pokemon_spa('whismur', 51).
pokemon_spa('loudred', 71).
pokemon_spa('exploud', 91).
pokemon_spa('makuhita', 20).
pokemon_spa('hariyama', 40).
pokemon_spa('azurill', 20).
pokemon_spa('nosepass', 45).
pokemon_spa('skitty', 35).
pokemon_spa('delcatty', 55).
pokemon_spa('sableye', 65).
pokemon_spa('sableyemega', 85).
pokemon_spa('mawile', 55).
pokemon_spa('mawilemega', 55).
pokemon_spa('aron', 40).
pokemon_spa('lairon', 50).
pokemon_spa('aggron', 60).
pokemon_spa('aggronmega', 60).
pokemon_spa('meditite', 40).
pokemon_spa('medicham', 60).
pokemon_spa('medichammega', 80).
pokemon_spa('electrike', 65).
pokemon_spa('manectric', 105).
pokemon_spa('manectricmega', 135).
pokemon_spa('plusle', 85).
pokemon_spa('minun', 75).
pokemon_spa('volbeat', 47).
pokemon_spa('illumise', 73).
pokemon_spa('roselia', 100).
pokemon_spa('gulpin', 43).
pokemon_spa('swalot', 73).
pokemon_spa('carvanha', 65).
pokemon_spa('sharpedo', 95).
pokemon_spa('sharpedomega', 110).
pokemon_spa('wailmer', 70).
pokemon_spa('wailord', 90).
pokemon_spa('numel', 65).
pokemon_spa('camerupt', 105).
pokemon_spa('cameruptmega', 145).
pokemon_spa('torkoal', 85).
pokemon_spa('spoink', 70).
pokemon_spa('grumpig', 90).
pokemon_spa('spinda', 60).
pokemon_spa('trapinch', 45).
pokemon_spa('vibrava', 50).
pokemon_spa('flygon', 80).
pokemon_spa('cacnea', 85).
pokemon_spa('cacturne', 115).
pokemon_spa('swablu', 40).
pokemon_spa('altaria', 70).
pokemon_spa('altariamega', 110).
pokemon_spa('zangoose', 60).
pokemon_spa('seviper', 100).
pokemon_spa('lunatone', 95).
pokemon_spa('solrock', 55).
pokemon_spa('barboach', 46).
pokemon_spa('whiscash', 76).
pokemon_spa('corphish', 50).
pokemon_spa('crawdaunt', 90).
pokemon_spa('baltoy', 40).
pokemon_spa('claydol', 70).
pokemon_spa('lileep', 61).
pokemon_spa('cradily', 81).
pokemon_spa('anorith', 40).
pokemon_spa('armaldo', 70).
pokemon_spa('feebas', 10).
pokemon_spa('milotic', 100).
pokemon_spa('castform', 70).
pokemon_spa('castformsunny', 70).
pokemon_spa('castformrainy', 70).
pokemon_spa('castformsnowy', 70).
pokemon_spa('kecleon', 60).
pokemon_spa('shuppet', 63).
pokemon_spa('banette', 83).
pokemon_spa('banettemega', 93).
pokemon_spa('duskull', 30).
pokemon_spa('dusclops', 60).
pokemon_spa('tropius', 72).
pokemon_spa('chimecho', 95).
pokemon_spa('absol', 75).
pokemon_spa('absolmega', 115).
pokemon_spa('wynaut', 23).
pokemon_spa('snorunt', 50).
pokemon_spa('glalie', 80).
pokemon_spa('glaliemega', 120).
pokemon_spa('spheal', 55).
pokemon_spa('sealeo', 75).
pokemon_spa('walrein', 95).
pokemon_spa('clamperl', 74).
pokemon_spa('huntail', 94).
pokemon_spa('gorebyss', 114).
pokemon_spa('relicanth', 45).
pokemon_spa('luvdisc', 40).
pokemon_spa('bagon', 40).
pokemon_spa('shelgon', 60).
pokemon_spa('salamence', 110).
pokemon_spa('salamencemega', 120).
pokemon_spa('beldum', 35).
pokemon_spa('metang', 55).
pokemon_spa('metagross', 95).
pokemon_spa('metagrossmega', 105).
pokemon_spa('regirock', 50).
pokemon_spa('regice', 100).
pokemon_spa('registeel', 75).
pokemon_spa('latias', 110).
pokemon_spa('latiasmega', 140).
pokemon_spa('latios', 130).
pokemon_spa('latiosmega', 160).
pokemon_spa('kyogre', 150).
pokemon_spa('kyogreprimal', 180).
pokemon_spa('groudon', 100).
pokemon_spa('groudonprimal', 150).
pokemon_spa('rayquaza', 150).
pokemon_spa('rayquazamega', 180).
pokemon_spa('jirachi', 100).
pokemon_spa('deoxys', 150).
pokemon_spa('deoxysattack', 180).
pokemon_spa('deoxysdefense', 70).
pokemon_spa('deoxysspeed', 95).
pokemon_spa('turtwig', 45).
pokemon_spa('grotle', 55).
pokemon_spa('torterra', 75).
pokemon_spa('chimchar', 58).
pokemon_spa('monferno', 78).
pokemon_spa('infernape', 104).
pokemon_spa('piplup', 61).
pokemon_spa('prinplup', 81).
pokemon_spa('empoleon', 111).
pokemon_spa('starly', 30).
pokemon_spa('staravia', 40).
pokemon_spa('staraptor', 50).
pokemon_spa('bidoof', 35).
pokemon_spa('bibarel', 55).
pokemon_spa('kricketot', 25).
pokemon_spa('kricketune', 55).
pokemon_spa('shinx', 40).
pokemon_spa('luxio', 60).
pokemon_spa('luxray', 95).
pokemon_spa('budew', 50).
pokemon_spa('roserade', 125).
pokemon_spa('cranidos', 30).
pokemon_spa('rampardos', 65).
pokemon_spa('shieldon', 42).
pokemon_spa('bastiodon', 47).
pokemon_spa('burmy', 29).
pokemon_spa('burmysandy', 29).
pokemon_spa('burmytrash', 29).
pokemon_spa('wormadam', 79).
pokemon_spa('wormadamsandy', 59).
pokemon_spa('wormadamtrash', 69).
pokemon_spa('mothim', 94).
pokemon_spa('combee', 30).
pokemon_spa('vespiquen', 80).
pokemon_spa('pachirisu', 45).
pokemon_spa('buizel', 60).
pokemon_spa('floatzel', 85).
pokemon_spa('cherubi', 62).
pokemon_spa('cherrim', 87).
pokemon_spa('cherrimsunshine', 87).
pokemon_spa('shellos', 57).
pokemon_spa('shelloseast', 57).
pokemon_spa('gastrodon', 92).
pokemon_spa('gastrodoneast', 92).
pokemon_spa('ambipom', 60).
pokemon_spa('drifloon', 60).
pokemon_spa('drifblim', 90).
pokemon_spa('buneary', 44).
pokemon_spa('lopunny', 54).
pokemon_spa('lopunnymega', 54).
pokemon_spa('mismagius', 105).
pokemon_spa('honchkrow', 105).
pokemon_spa('glameow', 42).
pokemon_spa('purugly', 64).
pokemon_spa('chingling', 65).
pokemon_spa('stunky', 41).
pokemon_spa('skuntank', 71).
pokemon_spa('bronzor', 24).
pokemon_spa('bronzong', 79).
pokemon_spa('bonsly', 10).
pokemon_spa('mimejr', 70).
pokemon_spa('happiny', 15).
pokemon_spa('chatot', 92).
pokemon_spa('spiritomb', 92).
pokemon_spa('gible', 40).
pokemon_spa('gabite', 50).
pokemon_spa('garchomp', 80).
pokemon_spa('garchompmega', 120).
pokemon_spa('munchlax', 40).
pokemon_spa('riolu', 35).
pokemon_spa('lucario', 115).
pokemon_spa('lucariomega', 140).
pokemon_spa('hippopotas', 38).
pokemon_spa('hippowdon', 68).
pokemon_spa('skorupi', 30).
pokemon_spa('drapion', 60).
pokemon_spa('croagunk', 61).
pokemon_spa('toxicroak', 86).
pokemon_spa('carnivine', 90).
pokemon_spa('finneon', 49).
pokemon_spa('lumineon', 69).
pokemon_spa('mantyke', 60).
pokemon_spa('snover', 62).
pokemon_spa('abomasnow', 92).
pokemon_spa('abomasnowmega', 132).
pokemon_spa('weavile', 45).
pokemon_spa('magnezone', 130).
pokemon_spa('lickilicky', 80).
pokemon_spa('rhyperior', 55).
pokemon_spa('tangrowth', 110).
pokemon_spa('electivire', 95).
pokemon_spa('magmortar', 125).
pokemon_spa('togekiss', 120).
pokemon_spa('yanmega', 116).
pokemon_spa('leafeon', 60).
pokemon_spa('glaceon', 130).
pokemon_spa('gliscor', 45).
pokemon_spa('mamoswine', 70).
pokemon_spa('porygonz', 135).
pokemon_spa('gallade', 65).
pokemon_spa('gallademega', 65).
pokemon_spa('probopass', 75).
pokemon_spa('dusknoir', 65).
pokemon_spa('froslass', 80).
pokemon_spa('froslassmega', 0).
pokemon_spa('rotom', 95).
pokemon_spa('rotomheat', 105).
pokemon_spa('rotomwash', 105).
pokemon_spa('rotomfrost', 105).
pokemon_spa('rotomfan', 105).
pokemon_spa('rotommow', 105).
pokemon_spa('uxie', 75).
pokemon_spa('mesprit', 105).
pokemon_spa('azelf', 125).
pokemon_spa('dialga', 150).
pokemon_spa('dialgaorigin', 150).
pokemon_spa('palkia', 150).
pokemon_spa('palkiaorigin', 150).
pokemon_spa('heatran', 130).
pokemon_spa('regigigas', 80).
pokemon_spa('giratina', 100).
pokemon_spa('giratinaorigin', 120).
pokemon_spa('cresselia', 75).
pokemon_spa('phione', 80).
pokemon_spa('manaphy', 100).
pokemon_spa('darkrai', 135).
pokemon_spa('shaymin', 100).
pokemon_spa('shayminsky', 120).
pokemon_spa('arceus', 120).
pokemon_spa('arceusbug', 120).
pokemon_spa('arceusdark', 120).
pokemon_spa('arceusdragon', 120).
pokemon_spa('arceuselectric', 120).
pokemon_spa('arceusfairy', 120).
pokemon_spa('arceusfighting', 120).
pokemon_spa('arceusfire', 120).
pokemon_spa('arceusflying', 120).
pokemon_spa('arceusghost', 120).
pokemon_spa('arceusgrass', 120).
pokemon_spa('arceusground', 120).
pokemon_spa('arceusice', 120).
pokemon_spa('arceuspoison', 120).
pokemon_spa('arceuspsychic', 120).
pokemon_spa('arceusrock', 120).
pokemon_spa('arceussteel', 120).
pokemon_spa('arceuswater', 120).
pokemon_spa('victini', 100).
pokemon_spa('snivy', 45).
pokemon_spa('servine', 60).
pokemon_spa('serperior', 75).
pokemon_spa('tepig', 45).
pokemon_spa('pignite', 70).
pokemon_spa('emboar', 100).
pokemon_spa('emboarmega', 0).
pokemon_spa('oshawott', 63).
pokemon_spa('dewott', 83).
pokemon_spa('samurott', 108).
pokemon_spa('samurotthisui', 100).
pokemon_spa('patrat', 35).
pokemon_spa('watchog', 60).
pokemon_spa('lillipup', 25).
pokemon_spa('herdier', 35).
pokemon_spa('stoutland', 45).
pokemon_spa('purrloin', 50).
pokemon_spa('liepard', 88).
pokemon_spa('pansage', 53).
pokemon_spa('simisage', 98).
pokemon_spa('pansear', 53).
pokemon_spa('simisear', 98).
pokemon_spa('panpour', 53).
pokemon_spa('simipour', 98).
pokemon_spa('munna', 67).
pokemon_spa('musharna', 107).
pokemon_spa('pidove', 36).
pokemon_spa('tranquill', 50).
pokemon_spa('unfezant', 65).
pokemon_spa('blitzle', 50).
pokemon_spa('zebstrika', 80).
pokemon_spa('roggenrola', 25).
pokemon_spa('boldore', 50).
pokemon_spa('gigalith', 60).
pokemon_spa('woobat', 55).
pokemon_spa('swoobat', 77).
pokemon_spa('drilbur', 30).
pokemon_spa('excadrill', 50).
pokemon_spa('excadrillmega', 0).
pokemon_spa('audino', 60).
pokemon_spa('audinomega', 80).
pokemon_spa('timburr', 25).
pokemon_spa('gurdurr', 40).
pokemon_spa('conkeldurr', 55).
pokemon_spa('tympole', 50).
pokemon_spa('palpitoad', 65).
pokemon_spa('seismitoad', 85).
pokemon_spa('throh', 30).
pokemon_spa('sawk', 30).
pokemon_spa('sewaddle', 40).
pokemon_spa('swadloon', 50).
pokemon_spa('leavanny', 70).
pokemon_spa('venipede', 30).
pokemon_spa('whirlipede', 40).
pokemon_spa('scolipede', 55).
pokemon_spa('scolipedemega', 0).
pokemon_spa('cottonee', 37).
pokemon_spa('whimsicott', 77).
pokemon_spa('petilil', 70).
pokemon_spa('lilligant', 110).
pokemon_spa('lilliganthisui', 50).
pokemon_spa('basculin', 80).
pokemon_spa('basculinbluestriped', 80).
pokemon_spa('basculinwhitestriped', 80).
pokemon_spa('sandile', 35).
pokemon_spa('krokorok', 45).
pokemon_spa('krookodile', 65).
pokemon_spa('darumaka', 15).
pokemon_spa('darumakagalar', 15).
pokemon_spa('darmanitan', 30).
pokemon_spa('darmanitanzen', 140).
pokemon_spa('darmanitangalar', 30).
pokemon_spa('darmanitangalarzen', 30).
pokemon_spa('maractus', 106).
pokemon_spa('dwebble', 35).
pokemon_spa('crustle', 65).
pokemon_spa('scraggy', 35).
pokemon_spa('scrafty', 45).
pokemon_spa('scraftymega', 0).
pokemon_spa('sigilyph', 103).
pokemon_spa('yamask', 55).
pokemon_spa('yamaskgalar', 30).
pokemon_spa('cofagrigus', 95).
pokemon_spa('tirtouga', 53).
pokemon_spa('carracosta', 83).
pokemon_spa('archen', 74).
pokemon_spa('archeops', 112).
pokemon_spa('trubbish', 40).
pokemon_spa('garbodor', 60).
pokemon_spa('garbodorgmax', 60).
pokemon_spa('zorua', 80).
pokemon_spa('zoruahisui', 85).
pokemon_spa('zoroark', 120).
pokemon_spa('zoroarkhisui', 125).
pokemon_spa('minccino', 40).
pokemon_spa('cinccino', 65).
pokemon_spa('gothita', 55).
pokemon_spa('gothorita', 75).
pokemon_spa('gothitelle', 95).
pokemon_spa('solosis', 105).
pokemon_spa('duosion', 125).
pokemon_spa('reuniclus', 125).
pokemon_spa('ducklett', 44).
pokemon_spa('swanna', 87).
pokemon_spa('vanillite', 65).
pokemon_spa('vanillish', 80).
pokemon_spa('vanilluxe', 110).
pokemon_spa('deerling', 40).
pokemon_spa('deerlingsummer', 40).
pokemon_spa('deerlingautumn', 40).
pokemon_spa('deerlingwinter', 40).
pokemon_spa('sawsbuck', 60).
pokemon_spa('emolga', 75).
pokemon_spa('karrablast', 40).
pokemon_spa('escavalier', 60).
pokemon_spa('foongus', 55).
pokemon_spa('amoonguss', 85).
pokemon_spa('frillish', 65).
pokemon_spa('jellicent', 85).
pokemon_spa('alomomola', 40).
pokemon_spa('joltik', 57).
pokemon_spa('galvantula', 97).
pokemon_spa('ferroseed', 24).
pokemon_spa('ferrothorn', 54).
pokemon_spa('klink', 45).
pokemon_spa('klang', 70).
pokemon_spa('klinklang', 70).
pokemon_spa('tynamo', 45).
pokemon_spa('eelektrik', 75).
pokemon_spa('eelektross', 105).
pokemon_spa('eelektrossmega', 0).
pokemon_spa('elgyem', 85).
pokemon_spa('beheeyem', 125).
pokemon_spa('litwick', 65).
pokemon_spa('lampent', 95).
pokemon_spa('chandelure', 145).
pokemon_spa('chandeluremega', 0).
pokemon_spa('axew', 30).
pokemon_spa('fraxure', 40).
pokemon_spa('haxorus', 60).
pokemon_spa('cubchoo', 60).
pokemon_spa('beartic', 70).
pokemon_spa('cryogonal', 95).
pokemon_spa('shelmet', 40).
pokemon_spa('accelgor', 100).
pokemon_spa('stunfisk', 81).
pokemon_spa('stunfiskgalar', 66).
pokemon_spa('mienfoo', 55).
pokemon_spa('mienshao', 95).
pokemon_spa('druddigon', 60).
pokemon_spa('golett', 35).
pokemon_spa('golurk', 55).
pokemon_spa('pawniard', 40).
pokemon_spa('bisharp', 60).
pokemon_spa('bouffalant', 40).
pokemon_spa('rufflet', 37).
pokemon_spa('braviary', 57).
pokemon_spa('braviaryhisui', 112).
pokemon_spa('vullaby', 45).
pokemon_spa('mandibuzz', 55).
pokemon_spa('heatmor', 105).
pokemon_spa('durant', 48).
pokemon_spa('deino', 45).
pokemon_spa('zweilous', 65).
pokemon_spa('hydreigon', 125).
pokemon_spa('larvesta', 50).
pokemon_spa('volcarona', 135).
pokemon_spa('cobalion', 90).
pokemon_spa('terrakion', 72).
pokemon_spa('virizion', 90).
pokemon_spa('tornadus', 125).
pokemon_spa('tornadustherian', 110).
pokemon_spa('thundurus', 125).
pokemon_spa('thundurustherian', 145).
pokemon_spa('reshiram', 150).
pokemon_spa('zekrom', 120).
pokemon_spa('landorus', 115).
pokemon_spa('landorustherian', 105).
pokemon_spa('kyurem', 130).
pokemon_spa('kyuremblack', 120).
pokemon_spa('kyuremwhite', 170).
pokemon_spa('keldeo', 129).
pokemon_spa('keldeoresolute', 129).
pokemon_spa('meloetta', 128).
pokemon_spa('meloettapirouette', 77).
pokemon_spa('genesect', 120).
pokemon_spa('genesectdouse', 120).
pokemon_spa('genesectshock', 120).
pokemon_spa('genesectburn', 120).
pokemon_spa('genesectchill', 120).
pokemon_spa('chespin', 48).
pokemon_spa('quilladin', 56).
pokemon_spa('chesnaught', 74).
pokemon_spa('chesnaughtmega', 0).
pokemon_spa('fennekin', 62).
pokemon_spa('braixen', 90).
pokemon_spa('delphox', 114).
pokemon_spa('delphoxmega', 0).
pokemon_spa('froakie', 62).
pokemon_spa('frogadier', 83).
pokemon_spa('greninja', 103).
pokemon_spa('greninjabond', 103).
pokemon_spa('greninjaash', 153).
pokemon_spa('greninjamega', 0).
pokemon_spa('bunnelby', 32).
pokemon_spa('diggersby', 50).
pokemon_spa('fletchling', 40).
pokemon_spa('fletchinder', 56).
pokemon_spa('talonflame', 74).
pokemon_spa('scatterbug', 27).
pokemon_spa('spewpa', 27).
pokemon_spa('vivillon', 90).
pokemon_spa('vivillonicysnow', 90).
pokemon_spa('vivillonpolar', 90).
pokemon_spa('vivillontundra', 90).
pokemon_spa('vivilloncontinental', 90).
pokemon_spa('vivillongarden', 90).
pokemon_spa('vivillonelegant', 90).
pokemon_spa('vivillonmodern', 90).
pokemon_spa('vivillonmarine', 90).
pokemon_spa('vivillonarchipelago', 90).
pokemon_spa('vivillonhighplains', 90).
pokemon_spa('vivillonsandstorm', 90).
pokemon_spa('vivillonriver', 90).
pokemon_spa('vivillonmonsoon', 90).
pokemon_spa('vivillonsavanna', 90).
pokemon_spa('vivillonsun', 90).
pokemon_spa('vivillonocean', 90).
pokemon_spa('vivillonjungle', 90).
pokemon_spa('vivillonfancy', 90).
pokemon_spa('vivillonpokeball', 90).
pokemon_spa('litleo', 73).
pokemon_spa('pyroar', 109).
pokemon_spa('pyroarmega', 0).
pokemon_spa('flabebe', 61).
pokemon_spa('floette', 75).
pokemon_spa('floetteeternal', 125).
pokemon_spa('floettemega', 0).
pokemon_spa('florges', 112).
pokemon_spa('skiddo', 62).
pokemon_spa('gogoat', 97).
pokemon_spa('pancham', 46).
pokemon_spa('pangoro', 69).
pokemon_spa('furfrou', 65).
pokemon_spa('espurr', 63).
pokemon_spa('meowstic', 83).
pokemon_spa('meowsticf', 83).
pokemon_spa('honedge', 35).
pokemon_spa('doublade', 45).
pokemon_spa('aegislash', 50).
pokemon_spa('aegislashblade', 140).
pokemon_spa('spritzee', 63).
pokemon_spa('aromatisse', 99).
pokemon_spa('swirlix', 59).
pokemon_spa('slurpuff', 85).
pokemon_spa('inkay', 37).
pokemon_spa('malamar', 68).
pokemon_spa('malamarmega', 0).
pokemon_spa('binacle', 39).
pokemon_spa('barbaracle', 54).
pokemon_spa('barbaraclemega', 0).
pokemon_spa('skrelp', 60).
pokemon_spa('dragalge', 97).
pokemon_spa('dragalgemega', 0).
pokemon_spa('clauncher', 58).
pokemon_spa('clawitzer', 120).
pokemon_spa('helioptile', 61).
pokemon_spa('heliolisk', 109).
pokemon_spa('tyrunt', 45).
pokemon_spa('tyrantrum', 69).
pokemon_spa('amaura', 67).
pokemon_spa('aurorus', 99).
pokemon_spa('sylveon', 110).
pokemon_spa('hawlucha', 74).
pokemon_spa('hawluchamega', 0).
pokemon_spa('dedenne', 81).
pokemon_spa('carbink', 50).
pokemon_spa('goomy', 55).
pokemon_spa('sliggoo', 83).
pokemon_spa('sliggoohisui', 83).
pokemon_spa('goodra', 110).
pokemon_spa('goodrahisui', 110).
pokemon_spa('klefki', 80).
pokemon_spa('phantump', 50).
pokemon_spa('trevenant', 65).
pokemon_spa('pumpkaboo', 44).
pokemon_spa('pumpkaboosmall', 44).
pokemon_spa('pumpkaboolarge', 44).
pokemon_spa('pumpkaboosuper', 44).
pokemon_spa('gourgeist', 58).
pokemon_spa('gourgeistsmall', 58).
pokemon_spa('gourgeistlarge', 58).
pokemon_spa('gourgeistsuper', 58).
pokemon_spa('bergmite', 32).
pokemon_spa('avalugg', 44).
pokemon_spa('avalugghisui', 34).
pokemon_spa('noibat', 45).
pokemon_spa('noivern', 97).
pokemon_spa('xerneas', 131).
pokemon_spa('xerneasneutral', 131).
pokemon_spa('yveltal', 131).
pokemon_spa('zygarde', 81).
pokemon_spa('zygarde10', 61).
pokemon_spa('zygardecomplete', 91).
pokemon_spa('zygardemega', 0).
pokemon_spa('diancie', 100).
pokemon_spa('dianciemega', 160).
pokemon_spa('hoopa', 150).
pokemon_spa('hoopaunbound', 170).
pokemon_spa('volcanion', 130).
pokemon_spa('rowlet', 50).
pokemon_spa('dartrix', 70).
pokemon_spa('decidueye', 100).
pokemon_spa('decidueyehisui', 95).
pokemon_spa('litten', 60).
pokemon_spa('torracat', 80).
pokemon_spa('incineroar', 80).
pokemon_spa('popplio', 66).
pokemon_spa('brionne', 91).
pokemon_spa('primarina', 126).
pokemon_spa('pikipek', 30).
pokemon_spa('trumbeak', 40).
pokemon_spa('toucannon', 75).
pokemon_spa('yungoos', 30).
pokemon_spa('gumshoos', 55).
pokemon_spa('gumshoostotem', 55).
pokemon_spa('grubbin', 55).
pokemon_spa('charjabug', 55).
pokemon_spa('vikavolt', 145).
pokemon_spa('vikavolttotem', 145).
pokemon_spa('crabrawler', 42).
pokemon_spa('crabominable', 62).
pokemon_spa('oricorio', 98).
pokemon_spa('oricoriopompom', 98).
pokemon_spa('oricoriopau', 98).
pokemon_spa('oricoriosensu', 98).
pokemon_spa('cutiefly', 55).
pokemon_spa('ribombee', 95).
pokemon_spa('ribombeetotem', 95).
pokemon_spa('rockruff', 30).
pokemon_spa('rockruffdusk', 30).
pokemon_spa('lycanroc', 55).
pokemon_spa('lycanrocmidnight', 55).
pokemon_spa('lycanrocdusk', 55).
pokemon_spa('wishiwashi', 25).
pokemon_spa('wishiwashischool', 140).
pokemon_spa('mareanie', 43).
pokemon_spa('toxapex', 53).
pokemon_spa('mudbray', 45).
pokemon_spa('mudsdale', 55).
pokemon_spa('dewpider', 40).
pokemon_spa('araquanid', 50).
pokemon_spa('araquanidtotem', 50).
pokemon_spa('fomantis', 50).
pokemon_spa('lurantis', 80).
pokemon_spa('lurantistotem', 80).
pokemon_spa('morelull', 65).
pokemon_spa('shiinotic', 90).
pokemon_spa('salandit', 71).
pokemon_spa('salazzle', 111).
pokemon_spa('salazzletotem', 111).
pokemon_spa('stufful', 45).
pokemon_spa('bewear', 55).
pokemon_spa('bounsweet', 30).
pokemon_spa('steenee', 40).
pokemon_spa('tsareena', 50).
pokemon_spa('comfey', 82).
pokemon_spa('oranguru', 90).
pokemon_spa('passimian', 40).
pokemon_spa('wimpod', 20).
pokemon_spa('golisopod', 60).
pokemon_spa('sandygast', 70).
pokemon_spa('palossand', 100).
pokemon_spa('pyukumuku', 30).
pokemon_spa('typenull', 95).
pokemon_spa('silvally', 95).
pokemon_spa('silvallybug', 95).
pokemon_spa('silvallydark', 95).
pokemon_spa('silvallydragon', 95).
pokemon_spa('silvallyelectric', 95).
pokemon_spa('silvallyfairy', 95).
pokemon_spa('silvallyfighting', 95).
pokemon_spa('silvallyfire', 95).
pokemon_spa('silvallyflying', 95).
pokemon_spa('silvallyghost', 95).
pokemon_spa('silvallygrass', 95).
pokemon_spa('silvallyground', 95).
pokemon_spa('silvallyice', 95).
pokemon_spa('silvallypoison', 95).
pokemon_spa('silvallypsychic', 95).
pokemon_spa('silvallyrock', 95).
pokemon_spa('silvallysteel', 95).
pokemon_spa('silvallywater', 95).
pokemon_spa('minior', 100).
pokemon_spa('miniororange', 100).
pokemon_spa('minioryellow', 100).
pokemon_spa('miniorgreen', 100).
pokemon_spa('miniorblue', 100).
pokemon_spa('miniorindigo', 100).
pokemon_spa('miniorviolet', 100).
pokemon_spa('miniormeteor', 60).
pokemon_spa('komala', 75).
pokemon_spa('turtonator', 91).
pokemon_spa('togedemaru', 40).
pokemon_spa('togedemarutotem', 40).
pokemon_spa('mimikyu', 50).
pokemon_spa('mimikyubusted', 50).
pokemon_spa('mimikyutotem', 50).
pokemon_spa('mimikyubustedtotem', 50).
pokemon_spa('bruxish', 70).
pokemon_spa('drampa', 135).
pokemon_spa('drampamega', 0).
pokemon_spa('dhelmise', 86).
pokemon_spa('jangmoo', 45).
pokemon_spa('hakamoo', 65).
pokemon_spa('kommoo', 100).
pokemon_spa('kommoototem', 100).
pokemon_spa('tapukoko', 95).
pokemon_spa('tapulele', 130).
pokemon_spa('tapubulu', 85).
pokemon_spa('tapufini', 95).
pokemon_spa('cosmog', 29).
pokemon_spa('cosmoem', 29).
pokemon_spa('solgaleo', 113).
pokemon_spa('lunala', 137).
pokemon_spa('nihilego', 127).
pokemon_spa('buzzwole', 53).
pokemon_spa('pheromosa', 137).
pokemon_spa('xurkitree', 173).
pokemon_spa('celesteela', 107).
pokemon_spa('kartana', 59).
pokemon_spa('guzzlord', 97).
pokemon_spa('necrozma', 127).
pokemon_spa('necrozmaduskmane', 113).
pokemon_spa('necrozmadawnwings', 157).
pokemon_spa('necrozmaultra', 167).
pokemon_spa('magearna', 130).
pokemon_spa('magearnaoriginal', 130).
pokemon_spa('marshadow', 90).
pokemon_spa('poipole', 73).
pokemon_spa('naganadel', 127).
pokemon_spa('stakataka', 53).
pokemon_spa('blacephalon', 151).
pokemon_spa('zeraora', 102).
pokemon_spa('meltan', 55).
pokemon_spa('melmetal', 80).
pokemon_spa('melmetalgmax', 80).
pokemon_spa('grookey', 40).
pokemon_spa('thwackey', 55).
pokemon_spa('rillaboom', 60).
pokemon_spa('rillaboomgmax', 60).
pokemon_spa('scorbunny', 40).
pokemon_spa('raboot', 55).
pokemon_spa('cinderace', 65).
pokemon_spa('cinderacegmax', 65).
pokemon_spa('sobble', 70).
pokemon_spa('drizzile', 95).
pokemon_spa('inteleon', 125).
pokemon_spa('inteleongmax', 125).
pokemon_spa('skwovet', 35).
pokemon_spa('greedent', 55).
pokemon_spa('rookidee', 33).
pokemon_spa('corvisquire', 43).
pokemon_spa('corviknight', 53).
pokemon_spa('corviknightgmax', 53).
pokemon_spa('blipbug', 25).
pokemon_spa('dottler', 50).
pokemon_spa('orbeetle', 80).
pokemon_spa('orbeetlegmax', 80).
pokemon_spa('nickit', 47).
pokemon_spa('thievul', 87).
pokemon_spa('gossifleur', 40).
pokemon_spa('eldegoss', 80).
pokemon_spa('wooloo', 40).
pokemon_spa('dubwool', 60).
pokemon_spa('chewtle', 38).
pokemon_spa('drednaw', 48).
pokemon_spa('drednawgmax', 48).
pokemon_spa('yamper', 40).
pokemon_spa('boltund', 90).
pokemon_spa('rolycoly', 40).
pokemon_spa('carkol', 60).
pokemon_spa('coalossal', 80).
pokemon_spa('coalossalgmax', 80).
pokemon_spa('applin', 40).
pokemon_spa('flapple', 95).
pokemon_spa('flapplegmax', 95).
pokemon_spa('appletun', 100).
pokemon_spa('appletungmax', 100).
pokemon_spa('silicobra', 35).
pokemon_spa('sandaconda', 65).
pokemon_spa('sandacondagmax', 65).
pokemon_spa('cramorant', 85).
pokemon_spa('cramorantgulping', 85).
pokemon_spa('cramorantgorging', 85).
pokemon_spa('arrokuda', 40).
pokemon_spa('barraskewda', 60).
pokemon_spa('toxel', 54).
pokemon_spa('toxtricity', 114).
pokemon_spa('toxtricitylowkey', 114).
pokemon_spa('toxtricitygmax', 114).
pokemon_spa('toxtricitylowkeygmax', 114).
pokemon_spa('sizzlipede', 50).
pokemon_spa('centiskorch', 90).
pokemon_spa('centiskorchgmax', 90).
pokemon_spa('clobbopus', 50).
pokemon_spa('grapploct', 70).
pokemon_spa('sinistea', 74).
pokemon_spa('sinisteaantique', 74).
pokemon_spa('polteageist', 134).
pokemon_spa('polteageistantique', 134).
pokemon_spa('hatenna', 56).
pokemon_spa('hattrem', 86).
pokemon_spa('hatterene', 136).
pokemon_spa('hatterenegmax', 136).
pokemon_spa('impidimp', 55).
pokemon_spa('morgrem', 75).
pokemon_spa('grimmsnarl', 95).
pokemon_spa('grimmsnarlgmax', 95).
pokemon_spa('obstagoon', 60).
pokemon_spa('perrserker', 50).
pokemon_spa('cursola', 145).
pokemon_spa('sirfetchd', 68).
pokemon_spa('mrrime', 110).
pokemon_spa('runerigus', 50).
pokemon_spa('milcery', 50).
pokemon_spa('alcremie', 110).
pokemon_spa('alcremierubycream', 110).
pokemon_spa('alcremiematchacream', 110).
pokemon_spa('alcremiemintcream', 110).
pokemon_spa('alcremielemoncream', 110).
pokemon_spa('alcremierubyswirl', 110).
pokemon_spa('alcremiecaramelswirl', 110).
pokemon_spa('alcremierainbowswirl', 110).
pokemon_spa('alcremiegmax', 110).
pokemon_spa('falinks', 70).
pokemon_spa('falinksmega', 0).
pokemon_spa('pincurchin', 91).
pokemon_spa('snom', 45).
pokemon_spa('frosmoth', 125).
pokemon_spa('stonjourner', 20).
pokemon_spa('eiscue', 65).
pokemon_spa('eiscuenoice', 65).
pokemon_spa('indeedee', 105).
pokemon_spa('indeedeef', 95).
pokemon_spa('morpeko', 70).
pokemon_spa('morpekohangry', 70).
pokemon_spa('cufant', 40).
pokemon_spa('copperajah', 80).
pokemon_spa('copperajahgmax', 80).
pokemon_spa('dracozolt', 80).
pokemon_spa('arctozolt', 90).
pokemon_spa('dracovish', 70).
pokemon_spa('arctovish', 80).
pokemon_spa('duraludon', 120).
pokemon_spa('duraludongmax', 120).
pokemon_spa('dreepy', 40).
pokemon_spa('drakloak', 60).
pokemon_spa('dragapult', 100).
pokemon_spa('zacian', 80).
pokemon_spa('zaciancrowned', 80).
pokemon_spa('zamazenta', 80).
pokemon_spa('zamazentacrowned', 80).
pokemon_spa('eternatus', 145).
pokemon_spa('eternatuseternamax', 125).
pokemon_spa('kubfu', 53).
pokemon_spa('urshifu', 63).
pokemon_spa('urshifurapidstrike', 63).
pokemon_spa('urshifugmax', 63).
pokemon_spa('urshifurapidstrikegmax', 63).
pokemon_spa('zarude', 70).
pokemon_spa('zarudedada', 70).
pokemon_spa('regieleki', 100).
pokemon_spa('regidrago', 100).
pokemon_spa('glastrier', 65).
pokemon_spa('spectrier', 145).
pokemon_spa('calyrex', 80).
pokemon_spa('calyrexice', 85).
pokemon_spa('calyrexshadow', 165).
pokemon_spa('wyrdeer', 105).
pokemon_spa('kleavor', 45).
pokemon_spa('ursaluna', 45).
pokemon_spa('ursalunabloodmoon', 135).
pokemon_spa('basculegion', 80).
pokemon_spa('basculegionf', 100).
pokemon_spa('sneasler', 40).
pokemon_spa('overqwil', 65).
pokemon_spa('enamorus', 135).
pokemon_spa('enamorustherian', 135).
pokemon_spa('sprigatito', 45).
pokemon_spa('floragato', 60).
pokemon_spa('meowscarada', 81).
pokemon_spa('fuecoco', 63).
pokemon_spa('crocalor', 90).
pokemon_spa('skeledirge', 110).
pokemon_spa('quaxly', 50).
pokemon_spa('quaxwell', 65).
pokemon_spa('quaquaval', 85).
pokemon_spa('lechonk', 35).
pokemon_spa('oinkologne', 59).
pokemon_spa('oinkolognef', 59).
pokemon_spa('tarountula', 29).
pokemon_spa('spidops', 52).
pokemon_spa('nymble', 21).
pokemon_spa('lokix', 52).
pokemon_spa('pawmi', 40).
pokemon_spa('pawmo', 50).
pokemon_spa('pawmot', 70).
pokemon_spa('tandemaus', 40).
pokemon_spa('maushold', 65).
pokemon_spa('mausholdfour', 65).
pokemon_spa('fidough', 30).
pokemon_spa('dachsbun', 50).
pokemon_spa('smoliv', 58).
pokemon_spa('dolliv', 78).
pokemon_spa('arboliva', 125).
pokemon_spa('squawkabilly', 45).
pokemon_spa('squawkabillyblue', 45).
pokemon_spa('squawkabillyyellow', 45).
pokemon_spa('squawkabillywhite', 45).
pokemon_spa('nacli', 35).
pokemon_spa('naclstack', 35).
pokemon_spa('garganacl', 45).
pokemon_spa('charcadet', 50).
pokemon_spa('armarouge', 125).
pokemon_spa('ceruledge', 60).
pokemon_spa('tadbulb', 59).
pokemon_spa('bellibolt', 103).
pokemon_spa('wattrel', 55).
pokemon_spa('kilowattrel', 105).
pokemon_spa('maschiff', 40).
pokemon_spa('mabosstiff', 60).
pokemon_spa('shroodle', 40).
pokemon_spa('grafaiai', 80).
pokemon_spa('bramblin', 45).
pokemon_spa('brambleghast', 80).
pokemon_spa('toedscool', 50).
pokemon_spa('toedscruel', 80).
pokemon_spa('klawf', 35).
pokemon_spa('capsakid', 62).
pokemon_spa('scovillain', 108).
pokemon_spa('rellor', 31).
pokemon_spa('rabsca', 115).
pokemon_spa('flittle', 55).
pokemon_spa('espathra', 101).
pokemon_spa('tinkatink', 35).
pokemon_spa('tinkatuff', 45).
pokemon_spa('tinkaton', 70).
pokemon_spa('wiglett', 35).
pokemon_spa('wugtrio', 50).
pokemon_spa('bombirdier', 60).
pokemon_spa('finizen', 45).
pokemon_spa('palafin', 53).
pokemon_spa('palafinhero', 106).
pokemon_spa('varoom', 30).
pokemon_spa('revavroom', 54).
pokemon_spa('cyclizar', 85).
pokemon_spa('orthworm', 60).
pokemon_spa('glimmet', 105).
pokemon_spa('glimmora', 130).
pokemon_spa('greavard', 30).
pokemon_spa('houndstone', 50).
pokemon_spa('flamigo', 75).
pokemon_spa('cetoddle', 30).
pokemon_spa('cetitan', 45).
pokemon_spa('veluza', 78).
pokemon_spa('dondozo', 65).
pokemon_spa('tatsugiri', 120).
pokemon_spa('tatsugiridroopy', 120).
pokemon_spa('tatsugiristretchy', 120).
pokemon_spa('annihilape', 50).
pokemon_spa('clodsire', 45).
pokemon_spa('farigiraf', 110).
pokemon_spa('dudunsparce', 85).
pokemon_spa('dudunsparcethreesegment', 85).
pokemon_spa('kingambit', 60).
pokemon_spa('greattusk', 53).
pokemon_spa('screamtail', 65).
pokemon_spa('brutebonnet', 79).
pokemon_spa('fluttermane', 135).
pokemon_spa('slitherwing', 85).
pokemon_spa('sandyshocks', 121).
pokemon_spa('irontreads', 72).
pokemon_spa('ironbundle', 124).
pokemon_spa('ironhands', 50).
pokemon_spa('ironjugulis', 122).
pokemon_spa('ironmoth', 140).
pokemon_spa('ironthorns', 70).
pokemon_spa('frigibax', 35).
pokemon_spa('arctibax', 45).
pokemon_spa('baxcalibur', 75).
pokemon_spa('gimmighoul', 75).
pokemon_spa('gimmighoulroaming', 75).
pokemon_spa('gholdengo', 133).
pokemon_spa('wochien', 95).
pokemon_spa('chienpao', 90).
pokemon_spa('tinglu', 55).
pokemon_spa('chiyu', 135).
pokemon_spa('roaringmoon', 55).
pokemon_spa('ironvaliant', 120).
pokemon_spa('koraidon', 85).
pokemon_spa('miraidon', 135).
pokemon_spa('walkingwake', 125).
pokemon_spa('ironleaves', 70).
pokemon_spa('dipplin', 95).
pokemon_spa('poltchageist', 74).
pokemon_spa('poltchageistartisan', 74).
pokemon_spa('sinistcha', 121).
pokemon_spa('sinistchamasterpiece', 121).
pokemon_spa('okidogi', 58).
pokemon_spa('munkidori', 130).
pokemon_spa('fezandipiti', 70).
pokemon_spa('ogerpon', 60).
pokemon_spa('ogerponwellspring', 60).
pokemon_spa('ogerponhearthflame', 60).
pokemon_spa('ogerponcornerstone', 60).
pokemon_spa('ogerpontealtera', 60).
pokemon_spa('ogerponwellspringtera', 60).
pokemon_spa('ogerponhearthflametera', 60).
pokemon_spa('ogerponcornerstonetera', 60).
pokemon_spa('archaludon', 125).
pokemon_spa('hydrapple', 120).
pokemon_spa('gougingfire', 65).
pokemon_spa('ragingbolt', 137).
pokemon_spa('ironboulder', 68).
pokemon_spa('ironcrown', 122).
pokemon_spa('terapagos', 65).
pokemon_spa('terapagosterastal', 105).
pokemon_spa('terapagosstellar', 130).
pokemon_spa('pecharunt', 88).
pokemon_spa('missingno', 6).
pokemon_spa('ramnarok', 0).
pokemon_spa('ramnarokradiant', 0).
pokemon_spa('pokestarsmeargle', 20).
pokemon_spa('pokestarufo', 100).
pokemon_spa('pokestarufo2', 100).
pokemon_spa('pokestarbrycenman', 100).
pokemon_spa('pokestarmt', 100).
pokemon_spa('pokestarmt2', 100).
pokemon_spa('pokestartransport', 100).
pokemon_spa('pokestargiant', 100).
pokemon_spa('pokestarhumanoid', 100).
pokemon_spa('pokestarmonster', 100).
pokemon_spa('pokestarf00', 100).
pokemon_spa('pokestarf002', 100).
pokemon_spa('pokestarspirit', 100).
pokemon_spa('pokestarblackdoor', 100).
pokemon_spa('pokestarwhitedoor', 100).
pokemon_spa('pokestarblackbelt', 100).
pokemon_spa('pokestarufopropu2', 100).
pokemon_spd('bulbasaur', 65).
pokemon_spd('ivysaur', 80).
pokemon_spd('venusaur', 100).
pokemon_spd('venusaurmega', 120).
pokemon_spd('venusaurgmax', 100).
pokemon_spd('charmander', 50).
pokemon_spd('charmeleon', 65).
pokemon_spd('charizard', 85).
pokemon_spd('charizardmegax', 85).
pokemon_spd('charizardmegay', 115).
pokemon_spd('charizardgmax', 85).
pokemon_spd('squirtle', 64).
pokemon_spd('wartortle', 80).
pokemon_spd('blastoise', 105).
pokemon_spd('blastoisemega', 115).
pokemon_spd('blastoisegmax', 105).
pokemon_spd('caterpie', 20).
pokemon_spd('metapod', 25).
pokemon_spd('butterfree', 80).
pokemon_spd('butterfreegmax', 80).
pokemon_spd('weedle', 20).
pokemon_spd('kakuna', 25).
pokemon_spd('beedrill', 80).
pokemon_spd('beedrillmega', 80).
pokemon_spd('pidgey', 35).
pokemon_spd('pidgeotto', 50).
pokemon_spd('pidgeot', 70).
pokemon_spd('pidgeotmega', 80).
pokemon_spd('rattata', 35).
pokemon_spd('rattataalola', 35).
pokemon_spd('raticate', 70).
pokemon_spd('raticatealola', 80).
pokemon_spd('raticatealolatotem', 80).
pokemon_spd('spearow', 31).
pokemon_spd('fearow', 61).
pokemon_spd('ekans', 54).
pokemon_spd('arbok', 79).
pokemon_spd('pikachu', 50).
pokemon_spd('pikachucosplay', 50).
pokemon_spd('pikachurockstar', 50).
pokemon_spd('pikachubelle', 50).
pokemon_spd('pikachupopstar', 50).
pokemon_spd('pikachuphd', 50).
pokemon_spd('pikachulibre', 50).
pokemon_spd('pikachuoriginal', 50).
pokemon_spd('pikachuhoenn', 50).
pokemon_spd('pikachusinnoh', 50).
pokemon_spd('pikachuunova', 50).
pokemon_spd('pikachukalos', 50).
pokemon_spd('pikachualola', 50).
pokemon_spd('pikachupartner', 50).
pokemon_spd('pikachustarter', 60).
pokemon_spd('pikachugmax', 50).
pokemon_spd('pikachuworld', 50).
pokemon_spd('raichu', 80).
pokemon_spd('raichualola', 85).
pokemon_spd('sandshrew', 30).
pokemon_spd('sandshrewalola', 35).
pokemon_spd('sandslash', 55).
pokemon_spd('sandslashalola', 65).
pokemon_spd('nidoranf', 40).
pokemon_spd('nidorina', 55).
pokemon_spd('nidoqueen', 85).
pokemon_spd('nidoranm', 40).
pokemon_spd('nidorino', 55).
pokemon_spd('nidoking', 75).
pokemon_spd('clefairy', 65).
pokemon_spd('clefable', 90).
pokemon_spd('clefablemega', 0).
pokemon_spd('vulpix', 65).
pokemon_spd('vulpixalola', 65).
pokemon_spd('ninetales', 100).
pokemon_spd('ninetalesalola', 100).
pokemon_spd('jigglypuff', 25).
pokemon_spd('wigglytuff', 50).
pokemon_spd('zubat', 40).
pokemon_spd('golbat', 75).
pokemon_spd('oddish', 65).
pokemon_spd('gloom', 75).
pokemon_spd('vileplume', 90).
pokemon_spd('paras', 55).
pokemon_spd('parasect', 80).
pokemon_spd('venonat', 55).
pokemon_spd('venomoth', 75).
pokemon_spd('diglett', 45).
pokemon_spd('diglettalola', 45).
pokemon_spd('dugtrio', 70).
pokemon_spd('dugtrioalola', 70).
pokemon_spd('meowth', 40).
pokemon_spd('meowthalola', 40).
pokemon_spd('meowthgalar', 40).
pokemon_spd('meowthgmax', 40).
pokemon_spd('persian', 65).
pokemon_spd('persianalola', 65).
pokemon_spd('psyduck', 50).
pokemon_spd('golduck', 80).
pokemon_spd('mankey', 45).
pokemon_spd('primeape', 70).
pokemon_spd('growlithe', 50).
pokemon_spd('growlithehisui', 50).
pokemon_spd('arcanine', 80).
pokemon_spd('arcaninehisui', 80).
pokemon_spd('poliwag', 40).
pokemon_spd('poliwhirl', 50).
pokemon_spd('poliwrath', 90).
pokemon_spd('abra', 55).
pokemon_spd('kadabra', 70).
pokemon_spd('alakazam', 95).
pokemon_spd('alakazammega', 105).
pokemon_spd('machop', 35).
pokemon_spd('machoke', 60).
pokemon_spd('machamp', 85).
pokemon_spd('machampgmax', 85).
pokemon_spd('bellsprout', 30).
pokemon_spd('weepinbell', 45).
pokemon_spd('victreebel', 70).
pokemon_spd('victreebelmega', 0).
pokemon_spd('tentacool', 100).
pokemon_spd('tentacruel', 120).
pokemon_spd('geodude', 30).
pokemon_spd('geodudealola', 30).
pokemon_spd('graveler', 45).
pokemon_spd('graveleralola', 45).
pokemon_spd('golem', 65).
pokemon_spd('golemalola', 65).
pokemon_spd('ponyta', 65).
pokemon_spd('ponytagalar', 65).
pokemon_spd('rapidash', 80).
pokemon_spd('rapidashgalar', 80).
pokemon_spd('slowpoke', 40).
pokemon_spd('slowpokegalar', 40).
pokemon_spd('slowbro', 80).
pokemon_spd('slowbromega', 80).
pokemon_spd('slowbrogalar', 70).
pokemon_spd('magnemite', 55).
pokemon_spd('magneton', 70).
pokemon_spd('farfetchd', 62).
pokemon_spd('farfetchdgalar', 62).
pokemon_spd('doduo', 35).
pokemon_spd('dodrio', 60).
pokemon_spd('seel', 70).
pokemon_spd('dewgong', 95).
pokemon_spd('grimer', 50).
pokemon_spd('grimeralola', 50).
pokemon_spd('muk', 100).
pokemon_spd('mukalola', 100).
pokemon_spd('shellder', 25).
pokemon_spd('cloyster', 45).
pokemon_spd('gastly', 35).
pokemon_spd('haunter', 55).
pokemon_spd('gengar', 75).
pokemon_spd('gengarmega', 95).
pokemon_spd('gengargmax', 75).
pokemon_spd('onix', 45).
pokemon_spd('drowzee', 90).
pokemon_spd('hypno', 115).
pokemon_spd('krabby', 25).
pokemon_spd('kingler', 50).
pokemon_spd('kinglergmax', 50).
pokemon_spd('voltorb', 55).
pokemon_spd('voltorbhisui', 55).
pokemon_spd('electrode', 80).
pokemon_spd('electrodehisui', 80).
pokemon_spd('exeggcute', 45).
pokemon_spd('exeggutor', 75).
pokemon_spd('exeggutoralola', 75).
pokemon_spd('cubone', 50).
pokemon_spd('marowak', 80).
pokemon_spd('marowakalola', 80).
pokemon_spd('marowakalolatotem', 80).
pokemon_spd('hitmonlee', 110).
pokemon_spd('hitmonchan', 110).
pokemon_spd('lickitung', 75).
pokemon_spd('koffing', 45).
pokemon_spd('weezing', 70).
pokemon_spd('weezinggalar', 70).
pokemon_spd('rhyhorn', 30).
pokemon_spd('rhydon', 45).
pokemon_spd('chansey', 105).
pokemon_spd('tangela', 40).
pokemon_spd('kangaskhan', 80).
pokemon_spd('kangaskhanmega', 100).
pokemon_spd('horsea', 25).
pokemon_spd('seadra', 45).
pokemon_spd('goldeen', 50).
pokemon_spd('seaking', 80).
pokemon_spd('staryu', 55).
pokemon_spd('starmie', 85).
pokemon_spd('starmiemega', 0).
pokemon_spd('mrmime', 120).
pokemon_spd('mrmimegalar', 90).
pokemon_spd('scyther', 80).
pokemon_spd('jynx', 95).
pokemon_spd('electabuzz', 85).
pokemon_spd('magmar', 85).
pokemon_spd('pinsir', 70).
pokemon_spd('pinsirmega', 90).
pokemon_spd('tauros', 70).
pokemon_spd('taurospaldeacombat', 70).
pokemon_spd('taurospaldeablaze', 70).
pokemon_spd('taurospaldeaaqua', 70).
pokemon_spd('magikarp', 20).
pokemon_spd('gyarados', 100).
pokemon_spd('gyaradosmega', 130).
pokemon_spd('lapras', 95).
pokemon_spd('laprasgmax', 95).
pokemon_spd('ditto', 48).
pokemon_spd('eevee', 65).
pokemon_spd('eeveestarter', 85).
pokemon_spd('eeveegmax', 65).
pokemon_spd('vaporeon', 95).
pokemon_spd('jolteon', 95).
pokemon_spd('flareon', 110).
pokemon_spd('porygon', 75).
pokemon_spd('omanyte', 55).
pokemon_spd('omastar', 70).
pokemon_spd('kabuto', 45).
pokemon_spd('kabutops', 70).
pokemon_spd('aerodactyl', 75).
pokemon_spd('aerodactylmega', 95).
pokemon_spd('snorlax', 110).
pokemon_spd('snorlaxgmax', 110).
pokemon_spd('articuno', 125).
pokemon_spd('articunogalar', 100).
pokemon_spd('zapdos', 90).
pokemon_spd('zapdosgalar', 90).
pokemon_spd('moltres', 85).
pokemon_spd('moltresgalar', 125).
pokemon_spd('dratini', 50).
pokemon_spd('dragonair', 70).
pokemon_spd('dragonite', 100).
pokemon_spd('dragonitemega', 0).
pokemon_spd('mewtwo', 90).
pokemon_spd('mewtwomegax', 100).
pokemon_spd('mewtwomegay', 120).
pokemon_spd('mew', 100).
pokemon_spd('chikorita', 65).
pokemon_spd('bayleef', 80).
pokemon_spd('meganium', 100).
pokemon_spd('meganiummega', 0).
pokemon_spd('cyndaquil', 50).
pokemon_spd('quilava', 65).
pokemon_spd('typhlosion', 85).
pokemon_spd('typhlosionhisui', 85).
pokemon_spd('totodile', 48).
pokemon_spd('croconaw', 63).
pokemon_spd('feraligatr', 83).
pokemon_spd('feraligatrmega', 0).
pokemon_spd('sentret', 45).
pokemon_spd('furret', 55).
pokemon_spd('hoothoot', 56).
pokemon_spd('noctowl', 96).
pokemon_spd('ledyba', 80).
pokemon_spd('ledian', 110).
pokemon_spd('spinarak', 40).
pokemon_spd('ariados', 70).
pokemon_spd('crobat', 80).
pokemon_spd('chinchou', 56).
pokemon_spd('lanturn', 76).
pokemon_spd('pichu', 35).
pokemon_spd('pichuspikyeared', 35).
pokemon_spd('cleffa', 55).
pokemon_spd('igglybuff', 20).
pokemon_spd('togepi', 65).
pokemon_spd('togetic', 105).
pokemon_spd('natu', 45).
pokemon_spd('xatu', 70).
pokemon_spd('mareep', 45).
pokemon_spd('flaaffy', 60).
pokemon_spd('ampharos', 90).
pokemon_spd('ampharosmega', 110).
pokemon_spd('bellossom', 100).
pokemon_spd('marill', 50).
pokemon_spd('azumarill', 80).
pokemon_spd('sudowoodo', 65).
pokemon_spd('politoed', 100).
pokemon_spd('hoppip', 55).
pokemon_spd('skiploom', 65).
pokemon_spd('jumpluff', 95).
pokemon_spd('aipom', 55).
pokemon_spd('sunkern', 30).
pokemon_spd('sunflora', 85).
pokemon_spd('yanma', 45).
pokemon_spd('wooper', 25).
pokemon_spd('wooperpaldea', 25).
pokemon_spd('quagsire', 65).
pokemon_spd('espeon', 95).
pokemon_spd('umbreon', 130).
pokemon_spd('murkrow', 42).
pokemon_spd('slowking', 110).
pokemon_spd('slowkinggalar', 110).
pokemon_spd('misdreavus', 85).
pokemon_spd('unown', 48).
pokemon_spd('wobbuffet', 58).
pokemon_spd('girafarig', 65).
pokemon_spd('pineco', 35).
pokemon_spd('forretress', 60).
pokemon_spd('dunsparce', 65).
pokemon_spd('gligar', 65).
pokemon_spd('steelix', 65).
pokemon_spd('steelixmega', 95).
pokemon_spd('snubbull', 40).
pokemon_spd('granbull', 60).
pokemon_spd('qwilfish', 55).
pokemon_spd('qwilfishhisui', 55).
pokemon_spd('scizor', 80).
pokemon_spd('scizormega', 100).
pokemon_spd('shuckle', 230).
pokemon_spd('heracross', 95).
pokemon_spd('heracrossmega', 105).
pokemon_spd('sneasel', 75).
pokemon_spd('sneaselhisui', 75).
pokemon_spd('teddiursa', 50).
pokemon_spd('ursaring', 75).
pokemon_spd('slugma', 40).
pokemon_spd('magcargo', 80).
pokemon_spd('swinub', 30).
pokemon_spd('piloswine', 60).
pokemon_spd('corsola', 95).
pokemon_spd('corsolagalar', 100).
pokemon_spd('remoraid', 35).
pokemon_spd('octillery', 75).
pokemon_spd('delibird', 45).
pokemon_spd('mantine', 140).
pokemon_spd('skarmory', 70).
pokemon_spd('skarmorymega', 0).
pokemon_spd('houndour', 50).
pokemon_spd('houndoom', 80).
pokemon_spd('houndoommega', 90).
pokemon_spd('kingdra', 95).
pokemon_spd('phanpy', 40).
pokemon_spd('donphan', 60).
pokemon_spd('porygon2', 95).
pokemon_spd('stantler', 65).
pokemon_spd('smeargle', 45).
pokemon_spd('tyrogue', 35).
pokemon_spd('hitmontop', 110).
pokemon_spd('smoochum', 65).
pokemon_spd('elekid', 55).
pokemon_spd('magby', 55).
pokemon_spd('miltank', 70).
pokemon_spd('blissey', 135).
pokemon_spd('raikou', 100).
pokemon_spd('entei', 75).
pokemon_spd('suicune', 115).
pokemon_spd('larvitar', 50).
pokemon_spd('pupitar', 70).
pokemon_spd('tyranitar', 100).
pokemon_spd('tyranitarmega', 120).
pokemon_spd('lugia', 154).
pokemon_spd('hooh', 154).
pokemon_spd('celebi', 100).
pokemon_spd('treecko', 55).
pokemon_spd('grovyle', 65).
pokemon_spd('sceptile', 85).
pokemon_spd('sceptilemega', 85).
pokemon_spd('torchic', 50).
pokemon_spd('combusken', 60).
pokemon_spd('blaziken', 70).
pokemon_spd('blazikenmega', 80).
pokemon_spd('mudkip', 50).
pokemon_spd('marshtomp', 70).
pokemon_spd('swampert', 90).
pokemon_spd('swampertmega', 110).
pokemon_spd('poochyena', 30).
pokemon_spd('mightyena', 60).
pokemon_spd('zigzagoon', 41).
pokemon_spd('zigzagoongalar', 41).
pokemon_spd('linoone', 61).
pokemon_spd('linoonegalar', 61).
pokemon_spd('wurmple', 30).
pokemon_spd('silcoon', 25).
pokemon_spd('beautifly', 50).
pokemon_spd('cascoon', 25).
pokemon_spd('dustox', 90).
pokemon_spd('lotad', 50).
pokemon_spd('lombre', 70).
pokemon_spd('ludicolo', 100).
pokemon_spd('seedot', 30).
pokemon_spd('nuzleaf', 40).
pokemon_spd('shiftry', 60).
pokemon_spd('taillow', 30).
pokemon_spd('swellow', 50).
pokemon_spd('wingull', 30).
pokemon_spd('pelipper', 70).
pokemon_spd('ralts', 35).
pokemon_spd('kirlia', 55).
pokemon_spd('gardevoir', 115).
pokemon_spd('gardevoirmega', 135).
pokemon_spd('surskit', 52).
pokemon_spd('masquerain', 82).
pokemon_spd('shroomish', 60).
pokemon_spd('breloom', 60).
pokemon_spd('slakoth', 35).
pokemon_spd('vigoroth', 55).
pokemon_spd('slaking', 65).
pokemon_spd('nincada', 30).
pokemon_spd('ninjask', 50).
pokemon_spd('shedinja', 30).
pokemon_spd('whismur', 23).
pokemon_spd('loudred', 43).
pokemon_spd('exploud', 73).
pokemon_spd('makuhita', 30).
pokemon_spd('hariyama', 60).
pokemon_spd('azurill', 40).
pokemon_spd('nosepass', 90).
pokemon_spd('skitty', 35).
pokemon_spd('delcatty', 55).
pokemon_spd('sableye', 65).
pokemon_spd('sableyemega', 115).
pokemon_spd('mawile', 55).
pokemon_spd('mawilemega', 95).
pokemon_spd('aron', 40).
pokemon_spd('lairon', 50).
pokemon_spd('aggron', 60).
pokemon_spd('aggronmega', 80).
pokemon_spd('meditite', 55).
pokemon_spd('medicham', 75).
pokemon_spd('medichammega', 85).
pokemon_spd('electrike', 40).
pokemon_spd('manectric', 60).
pokemon_spd('manectricmega', 80).
pokemon_spd('plusle', 75).
pokemon_spd('minun', 85).
pokemon_spd('volbeat', 85).
pokemon_spd('illumise', 85).
pokemon_spd('roselia', 80).
pokemon_spd('gulpin', 53).
pokemon_spd('swalot', 83).
pokemon_spd('carvanha', 20).
pokemon_spd('sharpedo', 40).
pokemon_spd('sharpedomega', 65).
pokemon_spd('wailmer', 35).
pokemon_spd('wailord', 45).
pokemon_spd('numel', 45).
pokemon_spd('camerupt', 75).
pokemon_spd('cameruptmega', 105).
pokemon_spd('torkoal', 70).
pokemon_spd('spoink', 80).
pokemon_spd('grumpig', 110).
pokemon_spd('spinda', 60).
pokemon_spd('trapinch', 45).
pokemon_spd('vibrava', 50).
pokemon_spd('flygon', 80).
pokemon_spd('cacnea', 40).
pokemon_spd('cacturne', 60).
pokemon_spd('swablu', 75).
pokemon_spd('altaria', 105).
pokemon_spd('altariamega', 105).
pokemon_spd('zangoose', 60).
pokemon_spd('seviper', 60).
pokemon_spd('lunatone', 85).
pokemon_spd('solrock', 65).
pokemon_spd('barboach', 41).
pokemon_spd('whiscash', 71).
pokemon_spd('corphish', 35).
pokemon_spd('crawdaunt', 55).
pokemon_spd('baltoy', 70).
pokemon_spd('claydol', 120).
pokemon_spd('lileep', 87).
pokemon_spd('cradily', 107).
pokemon_spd('anorith', 50).
pokemon_spd('armaldo', 80).
pokemon_spd('feebas', 55).
pokemon_spd('milotic', 125).
pokemon_spd('castform', 70).
pokemon_spd('castformsunny', 70).
pokemon_spd('castformrainy', 70).
pokemon_spd('castformsnowy', 70).
pokemon_spd('kecleon', 120).
pokemon_spd('shuppet', 33).
pokemon_spd('banette', 63).
pokemon_spd('banettemega', 83).
pokemon_spd('duskull', 90).
pokemon_spd('dusclops', 130).
pokemon_spd('tropius', 87).
pokemon_spd('chimecho', 90).
pokemon_spd('absol', 60).
pokemon_spd('absolmega', 60).
pokemon_spd('wynaut', 48).
pokemon_spd('snorunt', 50).
pokemon_spd('glalie', 80).
pokemon_spd('glaliemega', 80).
pokemon_spd('spheal', 50).
pokemon_spd('sealeo', 70).
pokemon_spd('walrein', 90).
pokemon_spd('clamperl', 55).
pokemon_spd('huntail', 75).
pokemon_spd('gorebyss', 75).
pokemon_spd('relicanth', 65).
pokemon_spd('luvdisc', 65).
pokemon_spd('bagon', 30).
pokemon_spd('shelgon', 50).
pokemon_spd('salamence', 80).
pokemon_spd('salamencemega', 90).
pokemon_spd('beldum', 60).
pokemon_spd('metang', 80).
pokemon_spd('metagross', 90).
pokemon_spd('metagrossmega', 110).
pokemon_spd('regirock', 100).
pokemon_spd('regice', 200).
pokemon_spd('registeel', 150).
pokemon_spd('latias', 130).
pokemon_spd('latiasmega', 150).
pokemon_spd('latios', 110).
pokemon_spd('latiosmega', 120).
pokemon_spd('kyogre', 140).
pokemon_spd('kyogreprimal', 160).
pokemon_spd('groudon', 90).
pokemon_spd('groudonprimal', 90).
pokemon_spd('rayquaza', 90).
pokemon_spd('rayquazamega', 100).
pokemon_spd('jirachi', 100).
pokemon_spd('deoxys', 50).
pokemon_spd('deoxysattack', 20).
pokemon_spd('deoxysdefense', 160).
pokemon_spd('deoxysspeed', 90).
pokemon_spd('turtwig', 55).
pokemon_spd('grotle', 65).
pokemon_spd('torterra', 85).
pokemon_spd('chimchar', 44).
pokemon_spd('monferno', 52).
pokemon_spd('infernape', 71).
pokemon_spd('piplup', 56).
pokemon_spd('prinplup', 76).
pokemon_spd('empoleon', 101).
pokemon_spd('starly', 30).
pokemon_spd('staravia', 40).
pokemon_spd('staraptor', 60).
pokemon_spd('bidoof', 40).
pokemon_spd('bibarel', 60).
pokemon_spd('kricketot', 41).
pokemon_spd('kricketune', 51).
pokemon_spd('shinx', 34).
pokemon_spd('luxio', 49).
pokemon_spd('luxray', 79).
pokemon_spd('budew', 70).
pokemon_spd('roserade', 105).
pokemon_spd('cranidos', 30).
pokemon_spd('rampardos', 50).
pokemon_spd('shieldon', 88).
pokemon_spd('bastiodon', 138).
pokemon_spd('burmy', 45).
pokemon_spd('burmysandy', 45).
pokemon_spd('burmytrash', 45).
pokemon_spd('wormadam', 105).
pokemon_spd('wormadamsandy', 85).
pokemon_spd('wormadamtrash', 95).
pokemon_spd('mothim', 50).
pokemon_spd('combee', 42).
pokemon_spd('vespiquen', 102).
pokemon_spd('pachirisu', 90).
pokemon_spd('buizel', 30).
pokemon_spd('floatzel', 50).
pokemon_spd('cherubi', 53).
pokemon_spd('cherrim', 78).
pokemon_spd('cherrimsunshine', 78).
pokemon_spd('shellos', 62).
pokemon_spd('shelloseast', 62).
pokemon_spd('gastrodon', 82).
pokemon_spd('gastrodoneast', 82).
pokemon_spd('ambipom', 66).
pokemon_spd('drifloon', 44).
pokemon_spd('drifblim', 54).
pokemon_spd('buneary', 56).
pokemon_spd('lopunny', 96).
pokemon_spd('lopunnymega', 96).
pokemon_spd('mismagius', 105).
pokemon_spd('honchkrow', 52).
pokemon_spd('glameow', 37).
pokemon_spd('purugly', 59).
pokemon_spd('chingling', 50).
pokemon_spd('stunky', 41).
pokemon_spd('skuntank', 61).
pokemon_spd('bronzor', 86).
pokemon_spd('bronzong', 116).
pokemon_spd('bonsly', 45).
pokemon_spd('mimejr', 90).
pokemon_spd('happiny', 65).
pokemon_spd('chatot', 42).
pokemon_spd('spiritomb', 108).
pokemon_spd('gible', 45).
pokemon_spd('gabite', 55).
pokemon_spd('garchomp', 85).
pokemon_spd('garchompmega', 95).
pokemon_spd('munchlax', 85).
pokemon_spd('riolu', 40).
pokemon_spd('lucario', 70).
pokemon_spd('lucariomega', 70).
pokemon_spd('hippopotas', 42).
pokemon_spd('hippowdon', 72).
pokemon_spd('skorupi', 55).
pokemon_spd('drapion', 75).
pokemon_spd('croagunk', 40).
pokemon_spd('toxicroak', 65).
pokemon_spd('carnivine', 72).
pokemon_spd('finneon', 61).
pokemon_spd('lumineon', 86).
pokemon_spd('mantyke', 120).
pokemon_spd('snover', 60).
pokemon_spd('abomasnow', 85).
pokemon_spd('abomasnowmega', 105).
pokemon_spd('weavile', 85).
pokemon_spd('magnezone', 90).
pokemon_spd('lickilicky', 95).
pokemon_spd('rhyperior', 55).
pokemon_spd('tangrowth', 50).
pokemon_spd('electivire', 85).
pokemon_spd('magmortar', 95).
pokemon_spd('togekiss', 115).
pokemon_spd('yanmega', 56).
pokemon_spd('leafeon', 65).
pokemon_spd('glaceon', 95).
pokemon_spd('gliscor', 75).
pokemon_spd('mamoswine', 60).
pokemon_spd('porygonz', 75).
pokemon_spd('gallade', 115).
pokemon_spd('gallademega', 115).
pokemon_spd('probopass', 150).
pokemon_spd('dusknoir', 135).
pokemon_spd('froslass', 70).
pokemon_spd('froslassmega', 0).
pokemon_spd('rotom', 77).
pokemon_spd('rotomheat', 107).
pokemon_spd('rotomwash', 107).
pokemon_spd('rotomfrost', 107).
pokemon_spd('rotomfan', 107).
pokemon_spd('rotommow', 107).
pokemon_spd('uxie', 130).
pokemon_spd('mesprit', 105).
pokemon_spd('azelf', 70).
pokemon_spd('dialga', 100).
pokemon_spd('dialgaorigin', 120).
pokemon_spd('palkia', 120).
pokemon_spd('palkiaorigin', 120).
pokemon_spd('heatran', 106).
pokemon_spd('regigigas', 110).
pokemon_spd('giratina', 120).
pokemon_spd('giratinaorigin', 100).
pokemon_spd('cresselia', 120).
pokemon_spd('phione', 80).
pokemon_spd('manaphy', 100).
pokemon_spd('darkrai', 90).
pokemon_spd('shaymin', 100).
pokemon_spd('shayminsky', 75).
pokemon_spd('arceus', 120).
pokemon_spd('arceusbug', 120).
pokemon_spd('arceusdark', 120).
pokemon_spd('arceusdragon', 120).
pokemon_spd('arceuselectric', 120).
pokemon_spd('arceusfairy', 120).
pokemon_spd('arceusfighting', 120).
pokemon_spd('arceusfire', 120).
pokemon_spd('arceusflying', 120).
pokemon_spd('arceusghost', 120).
pokemon_spd('arceusgrass', 120).
pokemon_spd('arceusground', 120).
pokemon_spd('arceusice', 120).
pokemon_spd('arceuspoison', 120).
pokemon_spd('arceuspsychic', 120).
pokemon_spd('arceusrock', 120).
pokemon_spd('arceussteel', 120).
pokemon_spd('arceuswater', 120).
pokemon_spd('victini', 100).
pokemon_spd('snivy', 55).
pokemon_spd('servine', 75).
pokemon_spd('serperior', 95).
pokemon_spd('tepig', 45).
pokemon_spd('pignite', 55).
pokemon_spd('emboar', 65).
pokemon_spd('emboarmega', 0).
pokemon_spd('oshawott', 45).
pokemon_spd('dewott', 60).
pokemon_spd('samurott', 70).
pokemon_spd('samurotthisui', 65).
pokemon_spd('patrat', 39).
pokemon_spd('watchog', 69).
pokemon_spd('lillipup', 45).
pokemon_spd('herdier', 65).
pokemon_spd('stoutland', 90).
pokemon_spd('purrloin', 37).
pokemon_spd('liepard', 50).
pokemon_spd('pansage', 48).
pokemon_spd('simisage', 63).
pokemon_spd('pansear', 48).
pokemon_spd('simisear', 63).
pokemon_spd('panpour', 48).
pokemon_spd('simipour', 63).
pokemon_spd('munna', 55).
pokemon_spd('musharna', 95).
pokemon_spd('pidove', 30).
pokemon_spd('tranquill', 42).
pokemon_spd('unfezant', 55).
pokemon_spd('blitzle', 32).
pokemon_spd('zebstrika', 63).
pokemon_spd('roggenrola', 25).
pokemon_spd('boldore', 40).
pokemon_spd('gigalith', 80).
pokemon_spd('woobat', 43).
pokemon_spd('swoobat', 55).
pokemon_spd('drilbur', 45).
pokemon_spd('excadrill', 65).
pokemon_spd('excadrillmega', 0).
pokemon_spd('audino', 86).
pokemon_spd('audinomega', 126).
pokemon_spd('timburr', 35).
pokemon_spd('gurdurr', 50).
pokemon_spd('conkeldurr', 65).
pokemon_spd('tympole', 40).
pokemon_spd('palpitoad', 55).
pokemon_spd('seismitoad', 75).
pokemon_spd('throh', 85).
pokemon_spd('sawk', 75).
pokemon_spd('sewaddle', 60).
pokemon_spd('swadloon', 80).
pokemon_spd('leavanny', 80).
pokemon_spd('venipede', 39).
pokemon_spd('whirlipede', 79).
pokemon_spd('scolipede', 69).
pokemon_spd('scolipedemega', 0).
pokemon_spd('cottonee', 50).
pokemon_spd('whimsicott', 75).
pokemon_spd('petilil', 50).
pokemon_spd('lilligant', 75).
pokemon_spd('lilliganthisui', 75).
pokemon_spd('basculin', 55).
pokemon_spd('basculinbluestriped', 55).
pokemon_spd('basculinwhitestriped', 55).
pokemon_spd('sandile', 35).
pokemon_spd('krokorok', 45).
pokemon_spd('krookodile', 70).
pokemon_spd('darumaka', 45).
pokemon_spd('darumakagalar', 45).
pokemon_spd('darmanitan', 55).
pokemon_spd('darmanitanzen', 105).
pokemon_spd('darmanitangalar', 55).
pokemon_spd('darmanitangalarzen', 55).
pokemon_spd('maractus', 67).
pokemon_spd('dwebble', 35).
pokemon_spd('crustle', 75).
pokemon_spd('scraggy', 70).
pokemon_spd('scrafty', 115).
pokemon_spd('scraftymega', 0).
pokemon_spd('sigilyph', 80).
pokemon_spd('yamask', 65).
pokemon_spd('yamaskgalar', 65).
pokemon_spd('cofagrigus', 105).
pokemon_spd('tirtouga', 45).
pokemon_spd('carracosta', 65).
pokemon_spd('archen', 45).
pokemon_spd('archeops', 65).
pokemon_spd('trubbish', 62).
pokemon_spd('garbodor', 82).
pokemon_spd('garbodorgmax', 82).
pokemon_spd('zorua', 40).
pokemon_spd('zoruahisui', 40).
pokemon_spd('zoroark', 60).
pokemon_spd('zoroarkhisui', 60).
pokemon_spd('minccino', 40).
pokemon_spd('cinccino', 60).
pokemon_spd('gothita', 65).
pokemon_spd('gothorita', 85).
pokemon_spd('gothitelle', 110).
pokemon_spd('solosis', 50).
pokemon_spd('duosion', 60).
pokemon_spd('reuniclus', 85).
pokemon_spd('ducklett', 50).
pokemon_spd('swanna', 63).
pokemon_spd('vanillite', 60).
pokemon_spd('vanillish', 75).
pokemon_spd('vanilluxe', 95).
pokemon_spd('deerling', 50).
pokemon_spd('deerlingsummer', 50).
pokemon_spd('deerlingautumn', 50).
pokemon_spd('deerlingwinter', 50).
pokemon_spd('sawsbuck', 70).
pokemon_spd('emolga', 60).
pokemon_spd('karrablast', 45).
pokemon_spd('escavalier', 105).
pokemon_spd('foongus', 55).
pokemon_spd('amoonguss', 80).
pokemon_spd('frillish', 85).
pokemon_spd('jellicent', 105).
pokemon_spd('alomomola', 45).
pokemon_spd('joltik', 50).
pokemon_spd('galvantula', 60).
pokemon_spd('ferroseed', 86).
pokemon_spd('ferrothorn', 116).
pokemon_spd('klink', 60).
pokemon_spd('klang', 85).
pokemon_spd('klinklang', 85).
pokemon_spd('tynamo', 40).
pokemon_spd('eelektrik', 70).
pokemon_spd('eelektross', 80).
pokemon_spd('eelektrossmega', 0).
pokemon_spd('elgyem', 55).
pokemon_spd('beheeyem', 95).
pokemon_spd('litwick', 55).
pokemon_spd('lampent', 60).
pokemon_spd('chandelure', 90).
pokemon_spd('chandeluremega', 0).
pokemon_spd('axew', 40).
pokemon_spd('fraxure', 50).
pokemon_spd('haxorus', 70).
pokemon_spd('cubchoo', 40).
pokemon_spd('beartic', 80).
pokemon_spd('cryogonal', 135).
pokemon_spd('shelmet', 65).
pokemon_spd('accelgor', 60).
pokemon_spd('stunfisk', 99).
pokemon_spd('stunfiskgalar', 84).
pokemon_spd('mienfoo', 50).
pokemon_spd('mienshao', 60).
pokemon_spd('druddigon', 90).
pokemon_spd('golett', 50).
pokemon_spd('golurk', 80).
pokemon_spd('pawniard', 40).
pokemon_spd('bisharp', 70).
pokemon_spd('bouffalant', 95).
pokemon_spd('rufflet', 50).
pokemon_spd('braviary', 75).
pokemon_spd('braviaryhisui', 70).
pokemon_spd('vullaby', 65).
pokemon_spd('mandibuzz', 95).
pokemon_spd('heatmor', 66).
pokemon_spd('durant', 48).
pokemon_spd('deino', 50).
pokemon_spd('zweilous', 70).
pokemon_spd('hydreigon', 90).
pokemon_spd('larvesta', 55).
pokemon_spd('volcarona', 105).
pokemon_spd('cobalion', 72).
pokemon_spd('terrakion', 90).
pokemon_spd('virizion', 129).
pokemon_spd('tornadus', 80).
pokemon_spd('tornadustherian', 90).
pokemon_spd('thundurus', 80).
pokemon_spd('thundurustherian', 80).
pokemon_spd('reshiram', 120).
pokemon_spd('zekrom', 100).
pokemon_spd('landorus', 80).
pokemon_spd('landorustherian', 80).
pokemon_spd('kyurem', 90).
pokemon_spd('kyuremblack', 90).
pokemon_spd('kyuremwhite', 100).
pokemon_spd('keldeo', 90).
pokemon_spd('keldeoresolute', 90).
pokemon_spd('meloetta', 128).
pokemon_spd('meloettapirouette', 77).
pokemon_spd('genesect', 95).
pokemon_spd('genesectdouse', 95).
pokemon_spd('genesectshock', 95).
pokemon_spd('genesectburn', 95).
pokemon_spd('genesectchill', 95).
pokemon_spd('chespin', 45).
pokemon_spd('quilladin', 58).
pokemon_spd('chesnaught', 75).
pokemon_spd('chesnaughtmega', 0).
pokemon_spd('fennekin', 60).
pokemon_spd('braixen', 70).
pokemon_spd('delphox', 100).
pokemon_spd('delphoxmega', 0).
pokemon_spd('froakie', 44).
pokemon_spd('frogadier', 56).
pokemon_spd('greninja', 71).
pokemon_spd('greninjabond', 71).
pokemon_spd('greninjaash', 71).
pokemon_spd('greninjamega', 0).
pokemon_spd('bunnelby', 36).
pokemon_spd('diggersby', 77).
pokemon_spd('fletchling', 38).
pokemon_spd('fletchinder', 52).
pokemon_spd('talonflame', 69).
pokemon_spd('scatterbug', 25).
pokemon_spd('spewpa', 30).
pokemon_spd('vivillon', 50).
pokemon_spd('vivillonicysnow', 50).
pokemon_spd('vivillonpolar', 50).
pokemon_spd('vivillontundra', 50).
pokemon_spd('vivilloncontinental', 50).
pokemon_spd('vivillongarden', 50).
pokemon_spd('vivillonelegant', 50).
pokemon_spd('vivillonmodern', 50).
pokemon_spd('vivillonmarine', 50).
pokemon_spd('vivillonarchipelago', 50).
pokemon_spd('vivillonhighplains', 50).
pokemon_spd('vivillonsandstorm', 50).
pokemon_spd('vivillonriver', 50).
pokemon_spd('vivillonmonsoon', 50).
pokemon_spd('vivillonsavanna', 50).
pokemon_spd('vivillonsun', 50).
pokemon_spd('vivillonocean', 50).
pokemon_spd('vivillonjungle', 50).
pokemon_spd('vivillonfancy', 50).
pokemon_spd('vivillonpokeball', 50).
pokemon_spd('litleo', 54).
pokemon_spd('pyroar', 66).
pokemon_spd('pyroarmega', 0).
pokemon_spd('flabebe', 79).
pokemon_spd('floette', 98).
pokemon_spd('floetteeternal', 128).
pokemon_spd('floettemega', 0).
pokemon_spd('florges', 154).
pokemon_spd('skiddo', 57).
pokemon_spd('gogoat', 81).
pokemon_spd('pancham', 48).
pokemon_spd('pangoro', 71).
pokemon_spd('furfrou', 90).
pokemon_spd('espurr', 60).
pokemon_spd('meowstic', 81).
pokemon_spd('meowsticf', 81).
pokemon_spd('honedge', 37).
pokemon_spd('doublade', 49).
pokemon_spd('aegislash', 140).
pokemon_spd('aegislashblade', 50).
pokemon_spd('spritzee', 65).
pokemon_spd('aromatisse', 89).
pokemon_spd('swirlix', 57).
pokemon_spd('slurpuff', 75).
pokemon_spd('inkay', 46).
pokemon_spd('malamar', 75).
pokemon_spd('malamarmega', 0).
pokemon_spd('binacle', 56).
pokemon_spd('barbaracle', 86).
pokemon_spd('barbaraclemega', 0).
pokemon_spd('skrelp', 60).
pokemon_spd('dragalge', 123).
pokemon_spd('dragalgemega', 0).
pokemon_spd('clauncher', 63).
pokemon_spd('clawitzer', 89).
pokemon_spd('helioptile', 43).
pokemon_spd('heliolisk', 94).
pokemon_spd('tyrunt', 45).
pokemon_spd('tyrantrum', 59).
pokemon_spd('amaura', 63).
pokemon_spd('aurorus', 92).
pokemon_spd('sylveon', 130).
pokemon_spd('hawlucha', 63).
pokemon_spd('hawluchamega', 0).
pokemon_spd('dedenne', 67).
pokemon_spd('carbink', 150).
pokemon_spd('goomy', 75).
pokemon_spd('sliggoo', 113).
pokemon_spd('sliggoohisui', 113).
pokemon_spd('goodra', 150).
pokemon_spd('goodrahisui', 150).
pokemon_spd('klefki', 87).
pokemon_spd('phantump', 60).
pokemon_spd('trevenant', 82).
pokemon_spd('pumpkaboo', 55).
pokemon_spd('pumpkaboosmall', 55).
pokemon_spd('pumpkaboolarge', 55).
pokemon_spd('pumpkaboosuper', 55).
pokemon_spd('gourgeist', 75).
pokemon_spd('gourgeistsmall', 75).
pokemon_spd('gourgeistlarge', 75).
pokemon_spd('gourgeistsuper', 75).
pokemon_spd('bergmite', 35).
pokemon_spd('avalugg', 46).
pokemon_spd('avalugghisui', 36).
pokemon_spd('noibat', 40).
pokemon_spd('noivern', 80).
pokemon_spd('xerneas', 98).
pokemon_spd('xerneasneutral', 98).
pokemon_spd('yveltal', 98).
pokemon_spd('zygarde', 95).
pokemon_spd('zygarde10', 85).
pokemon_spd('zygardecomplete', 95).
pokemon_spd('zygardemega', 0).
pokemon_spd('diancie', 150).
pokemon_spd('dianciemega', 110).
pokemon_spd('hoopa', 130).
pokemon_spd('hoopaunbound', 130).
pokemon_spd('volcanion', 90).
pokemon_spd('rowlet', 50).
pokemon_spd('dartrix', 70).
pokemon_spd('decidueye', 100).
pokemon_spd('decidueyehisui', 95).
pokemon_spd('litten', 40).
pokemon_spd('torracat', 50).
pokemon_spd('incineroar', 90).
pokemon_spd('popplio', 56).
pokemon_spd('brionne', 81).
pokemon_spd('primarina', 116).
pokemon_spd('pikipek', 30).
pokemon_spd('trumbeak', 50).
pokemon_spd('toucannon', 75).
pokemon_spd('yungoos', 30).
pokemon_spd('gumshoos', 60).
pokemon_spd('gumshoostotem', 60).
pokemon_spd('grubbin', 45).
pokemon_spd('charjabug', 75).
pokemon_spd('vikavolt', 75).
pokemon_spd('vikavolttotem', 75).
pokemon_spd('crabrawler', 47).
pokemon_spd('crabominable', 67).
pokemon_spd('oricorio', 70).
pokemon_spd('oricoriopompom', 70).
pokemon_spd('oricoriopau', 70).
pokemon_spd('oricoriosensu', 70).
pokemon_spd('cutiefly', 40).
pokemon_spd('ribombee', 70).
pokemon_spd('ribombeetotem', 70).
pokemon_spd('rockruff', 40).
pokemon_spd('rockruffdusk', 40).
pokemon_spd('lycanroc', 65).
pokemon_spd('lycanrocmidnight', 75).
pokemon_spd('lycanrocdusk', 65).
pokemon_spd('wishiwashi', 25).
pokemon_spd('wishiwashischool', 135).
pokemon_spd('mareanie', 52).
pokemon_spd('toxapex', 142).
pokemon_spd('mudbray', 55).
pokemon_spd('mudsdale', 85).
pokemon_spd('dewpider', 72).
pokemon_spd('araquanid', 132).
pokemon_spd('araquanidtotem', 132).
pokemon_spd('fomantis', 35).
pokemon_spd('lurantis', 90).
pokemon_spd('lurantistotem', 90).
pokemon_spd('morelull', 75).
pokemon_spd('shiinotic', 100).
pokemon_spd('salandit', 40).
pokemon_spd('salazzle', 60).
pokemon_spd('salazzletotem', 60).
pokemon_spd('stufful', 50).
pokemon_spd('bewear', 60).
pokemon_spd('bounsweet', 38).
pokemon_spd('steenee', 48).
pokemon_spd('tsareena', 98).
pokemon_spd('comfey', 110).
pokemon_spd('oranguru', 110).
pokemon_spd('passimian', 60).
pokemon_spd('wimpod', 30).
pokemon_spd('golisopod', 90).
pokemon_spd('sandygast', 45).
pokemon_spd('palossand', 75).
pokemon_spd('pyukumuku', 130).
pokemon_spd('typenull', 95).
pokemon_spd('silvally', 95).
pokemon_spd('silvallybug', 95).
pokemon_spd('silvallydark', 95).
pokemon_spd('silvallydragon', 95).
pokemon_spd('silvallyelectric', 95).
pokemon_spd('silvallyfairy', 95).
pokemon_spd('silvallyfighting', 95).
pokemon_spd('silvallyfire', 95).
pokemon_spd('silvallyflying', 95).
pokemon_spd('silvallyghost', 95).
pokemon_spd('silvallygrass', 95).
pokemon_spd('silvallyground', 95).
pokemon_spd('silvallyice', 95).
pokemon_spd('silvallypoison', 95).
pokemon_spd('silvallypsychic', 95).
pokemon_spd('silvallyrock', 95).
pokemon_spd('silvallysteel', 95).
pokemon_spd('silvallywater', 95).
pokemon_spd('minior', 60).
pokemon_spd('miniororange', 60).
pokemon_spd('minioryellow', 60).
pokemon_spd('miniorgreen', 60).
pokemon_spd('miniorblue', 60).
pokemon_spd('miniorindigo', 60).
pokemon_spd('miniorviolet', 60).
pokemon_spd('miniormeteor', 100).
pokemon_spd('komala', 95).
pokemon_spd('turtonator', 85).
pokemon_spd('togedemaru', 73).
pokemon_spd('togedemarutotem', 73).
pokemon_spd('mimikyu', 105).
pokemon_spd('mimikyubusted', 105).
pokemon_spd('mimikyutotem', 105).
pokemon_spd('mimikyubustedtotem', 105).
pokemon_spd('bruxish', 70).
pokemon_spd('drampa', 91).
pokemon_spd('drampamega', 0).
pokemon_spd('dhelmise', 90).
pokemon_spd('jangmoo', 45).
pokemon_spd('hakamoo', 70).
pokemon_spd('kommoo', 105).
pokemon_spd('kommoototem', 105).
pokemon_spd('tapukoko', 75).
pokemon_spd('tapulele', 115).
pokemon_spd('tapubulu', 95).
pokemon_spd('tapufini', 130).
pokemon_spd('cosmog', 31).
pokemon_spd('cosmoem', 131).
pokemon_spd('solgaleo', 89).
pokemon_spd('lunala', 107).
pokemon_spd('nihilego', 131).
pokemon_spd('buzzwole', 53).
pokemon_spd('pheromosa', 37).
pokemon_spd('xurkitree', 71).
pokemon_spd('celesteela', 101).
pokemon_spd('kartana', 31).
pokemon_spd('guzzlord', 53).
pokemon_spd('necrozma', 89).
pokemon_spd('necrozmaduskmane', 109).
pokemon_spd('necrozmadawnwings', 127).
pokemon_spd('necrozmaultra', 97).
pokemon_spd('magearna', 115).
pokemon_spd('magearnaoriginal', 115).
pokemon_spd('marshadow', 90).
pokemon_spd('poipole', 67).
pokemon_spd('naganadel', 73).
pokemon_spd('stakataka', 101).
pokemon_spd('blacephalon', 79).
pokemon_spd('zeraora', 80).
pokemon_spd('meltan', 35).
pokemon_spd('melmetal', 65).
pokemon_spd('melmetalgmax', 65).
pokemon_spd('grookey', 40).
pokemon_spd('thwackey', 60).
pokemon_spd('rillaboom', 70).
pokemon_spd('rillaboomgmax', 70).
pokemon_spd('scorbunny', 40).
pokemon_spd('raboot', 60).
pokemon_spd('cinderace', 75).
pokemon_spd('cinderacegmax', 75).
pokemon_spd('sobble', 40).
pokemon_spd('drizzile', 55).
pokemon_spd('inteleon', 65).
pokemon_spd('inteleongmax', 65).
pokemon_spd('skwovet', 35).
pokemon_spd('greedent', 75).
pokemon_spd('rookidee', 35).
pokemon_spd('corvisquire', 55).
pokemon_spd('corviknight', 85).
pokemon_spd('corviknightgmax', 85).
pokemon_spd('blipbug', 45).
pokemon_spd('dottler', 90).
pokemon_spd('orbeetle', 120).
pokemon_spd('orbeetlegmax', 120).
pokemon_spd('nickit', 52).
pokemon_spd('thievul', 92).
pokemon_spd('gossifleur', 60).
pokemon_spd('eldegoss', 120).
pokemon_spd('wooloo', 45).
pokemon_spd('dubwool', 90).
pokemon_spd('chewtle', 38).
pokemon_spd('drednaw', 68).
pokemon_spd('drednawgmax', 68).
pokemon_spd('yamper', 50).
pokemon_spd('boltund', 60).
pokemon_spd('rolycoly', 50).
pokemon_spd('carkol', 70).
pokemon_spd('coalossal', 90).
pokemon_spd('coalossalgmax', 90).
pokemon_spd('applin', 40).
pokemon_spd('flapple', 60).
pokemon_spd('flapplegmax', 60).
pokemon_spd('appletun', 80).
pokemon_spd('appletungmax', 80).
pokemon_spd('silicobra', 50).
pokemon_spd('sandaconda', 70).
pokemon_spd('sandacondagmax', 70).
pokemon_spd('cramorant', 95).
pokemon_spd('cramorantgulping', 95).
pokemon_spd('cramorantgorging', 95).
pokemon_spd('arrokuda', 30).
pokemon_spd('barraskewda', 50).
pokemon_spd('toxel', 35).
pokemon_spd('toxtricity', 70).
pokemon_spd('toxtricitylowkey', 70).
pokemon_spd('toxtricitygmax', 70).
pokemon_spd('toxtricitylowkeygmax', 70).
pokemon_spd('sizzlipede', 50).
pokemon_spd('centiskorch', 90).
pokemon_spd('centiskorchgmax', 90).
pokemon_spd('clobbopus', 50).
pokemon_spd('grapploct', 80).
pokemon_spd('sinistea', 54).
pokemon_spd('sinisteaantique', 54).
pokemon_spd('polteageist', 114).
pokemon_spd('polteageistantique', 114).
pokemon_spd('hatenna', 53).
pokemon_spd('hattrem', 73).
pokemon_spd('hatterene', 103).
pokemon_spd('hatterenegmax', 103).
pokemon_spd('impidimp', 40).
pokemon_spd('morgrem', 55).
pokemon_spd('grimmsnarl', 75).
pokemon_spd('grimmsnarlgmax', 75).
pokemon_spd('obstagoon', 81).
pokemon_spd('perrserker', 60).
pokemon_spd('cursola', 130).
pokemon_spd('sirfetchd', 82).
pokemon_spd('mrrime', 100).
pokemon_spd('runerigus', 105).
pokemon_spd('milcery', 61).
pokemon_spd('alcremie', 121).
pokemon_spd('alcremierubycream', 121).
pokemon_spd('alcremiematchacream', 121).
pokemon_spd('alcremiemintcream', 121).
pokemon_spd('alcremielemoncream', 121).
pokemon_spd('alcremierubyswirl', 121).
pokemon_spd('alcremiecaramelswirl', 121).
pokemon_spd('alcremierainbowswirl', 121).
pokemon_spd('alcremiegmax', 121).
pokemon_spd('falinks', 60).
pokemon_spd('falinksmega', 0).
pokemon_spd('pincurchin', 85).
pokemon_spd('snom', 30).
pokemon_spd('frosmoth', 90).
pokemon_spd('stonjourner', 20).
pokemon_spd('eiscue', 90).
pokemon_spd('eiscuenoice', 50).
pokemon_spd('indeedee', 95).
pokemon_spd('indeedeef', 105).
pokemon_spd('morpeko', 58).
pokemon_spd('morpekohangry', 58).
pokemon_spd('cufant', 49).
pokemon_spd('copperajah', 69).
pokemon_spd('copperajahgmax', 69).
pokemon_spd('dracozolt', 70).
pokemon_spd('arctozolt', 80).
pokemon_spd('dracovish', 80).
pokemon_spd('arctovish', 90).
pokemon_spd('duraludon', 50).
pokemon_spd('duraludongmax', 50).
pokemon_spd('dreepy', 30).
pokemon_spd('drakloak', 50).
pokemon_spd('dragapult', 75).
pokemon_spd('zacian', 115).
pokemon_spd('zaciancrowned', 115).
pokemon_spd('zamazenta', 115).
pokemon_spd('zamazentacrowned', 140).
pokemon_spd('eternatus', 95).
pokemon_spd('eternatuseternamax', 250).
pokemon_spd('kubfu', 50).
pokemon_spd('urshifu', 60).
pokemon_spd('urshifurapidstrike', 60).
pokemon_spd('urshifugmax', 60).
pokemon_spd('urshifurapidstrikegmax', 60).
pokemon_spd('zarude', 95).
pokemon_spd('zarudedada', 95).
pokemon_spd('regieleki', 50).
pokemon_spd('regidrago', 50).
pokemon_spd('glastrier', 110).
pokemon_spd('spectrier', 80).
pokemon_spd('calyrex', 80).
pokemon_spd('calyrexice', 130).
pokemon_spd('calyrexshadow', 100).
pokemon_spd('wyrdeer', 75).
pokemon_spd('kleavor', 70).
pokemon_spd('ursaluna', 80).
pokemon_spd('ursalunabloodmoon', 65).
pokemon_spd('basculegion', 75).
pokemon_spd('basculegionf', 75).
pokemon_spd('sneasler', 80).
pokemon_spd('overqwil', 65).
pokemon_spd('enamorus', 80).
pokemon_spd('enamorustherian', 100).
pokemon_spd('sprigatito', 45).
pokemon_spd('floragato', 63).
pokemon_spd('meowscarada', 70).
pokemon_spd('fuecoco', 40).
pokemon_spd('crocalor', 58).
pokemon_spd('skeledirge', 75).
pokemon_spd('quaxly', 45).
pokemon_spd('quaxwell', 60).
pokemon_spd('quaquaval', 75).
pokemon_spd('lechonk', 45).
pokemon_spd('oinkologne', 80).
pokemon_spd('oinkolognef', 90).
pokemon_spd('tarountula', 40).
pokemon_spd('spidops', 86).
pokemon_spd('nymble', 25).
pokemon_spd('lokix', 55).
pokemon_spd('pawmi', 25).
pokemon_spd('pawmo', 40).
pokemon_spd('pawmot', 60).
pokemon_spd('tandemaus', 45).
pokemon_spd('maushold', 75).
pokemon_spd('mausholdfour', 75).
pokemon_spd('fidough', 55).
pokemon_spd('dachsbun', 80).
pokemon_spd('smoliv', 51).
pokemon_spd('dolliv', 78).
pokemon_spd('arboliva', 109).
pokemon_spd('squawkabilly', 51).
pokemon_spd('squawkabillyblue', 51).
pokemon_spd('squawkabillyyellow', 51).
pokemon_spd('squawkabillywhite', 51).
pokemon_spd('nacli', 35).
pokemon_spd('naclstack', 65).
pokemon_spd('garganacl', 90).
pokemon_spd('charcadet', 40).
pokemon_spd('armarouge', 80).
pokemon_spd('ceruledge', 100).
pokemon_spd('tadbulb', 35).
pokemon_spd('bellibolt', 83).
pokemon_spd('wattrel', 40).
pokemon_spd('kilowattrel', 60).
pokemon_spd('maschiff', 51).
pokemon_spd('mabosstiff', 70).
pokemon_spd('shroodle', 35).
pokemon_spd('grafaiai', 72).
pokemon_spd('bramblin', 35).
pokemon_spd('brambleghast', 70).
pokemon_spd('toedscool', 100).
pokemon_spd('toedscruel', 120).
pokemon_spd('klawf', 55).
pokemon_spd('capsakid', 40).
pokemon_spd('scovillain', 65).
pokemon_spd('rellor', 58).
pokemon_spd('rabsca', 100).
pokemon_spd('flittle', 30).
pokemon_spd('espathra', 60).
pokemon_spd('tinkatink', 64).
pokemon_spd('tinkatuff', 82).
pokemon_spd('tinkaton', 105).
pokemon_spd('wiglett', 25).
pokemon_spd('wugtrio', 70).
pokemon_spd('bombirdier', 85).
pokemon_spd('finizen', 40).
pokemon_spd('palafin', 62).
pokemon_spd('palafinhero', 87).
pokemon_spd('varoom', 45).
pokemon_spd('revavroom', 67).
pokemon_spd('cyclizar', 65).
pokemon_spd('orthworm', 55).
pokemon_spd('glimmet', 60).
pokemon_spd('glimmora', 81).
pokemon_spd('greavard', 55).
pokemon_spd('houndstone', 97).
pokemon_spd('flamigo', 64).
pokemon_spd('cetoddle', 40).
pokemon_spd('cetitan', 55).
pokemon_spd('veluza', 65).
pokemon_spd('dondozo', 65).
pokemon_spd('tatsugiri', 95).
pokemon_spd('tatsugiridroopy', 95).
pokemon_spd('tatsugiristretchy', 95).
pokemon_spd('annihilape', 90).
pokemon_spd('clodsire', 100).
pokemon_spd('farigiraf', 70).
pokemon_spd('dudunsparce', 75).
pokemon_spd('dudunsparcethreesegment', 75).
pokemon_spd('kingambit', 85).
pokemon_spd('greattusk', 53).
pokemon_spd('screamtail', 115).
pokemon_spd('brutebonnet', 99).
pokemon_spd('fluttermane', 135).
pokemon_spd('slitherwing', 105).
pokemon_spd('sandyshocks', 85).
pokemon_spd('irontreads', 70).
pokemon_spd('ironbundle', 60).
pokemon_spd('ironhands', 68).
pokemon_spd('ironjugulis', 80).
pokemon_spd('ironmoth', 110).
pokemon_spd('ironthorns', 84).
pokemon_spd('frigibax', 45).
pokemon_spd('arctibax', 65).
pokemon_spd('baxcalibur', 86).
pokemon_spd('gimmighoul', 70).
pokemon_spd('gimmighoulroaming', 45).
pokemon_spd('gholdengo', 91).
pokemon_spd('wochien', 135).
pokemon_spd('chienpao', 65).
pokemon_spd('tinglu', 80).
pokemon_spd('chiyu', 120).
pokemon_spd('roaringmoon', 101).
pokemon_spd('ironvaliant', 60).
pokemon_spd('koraidon', 100).
pokemon_spd('miraidon', 115).
pokemon_spd('walkingwake', 83).
pokemon_spd('ironleaves', 108).
pokemon_spd('dipplin', 80).
pokemon_spd('poltchageist', 54).
pokemon_spd('poltchageistartisan', 54).
pokemon_spd('sinistcha', 80).
pokemon_spd('sinistchamasterpiece', 80).
pokemon_spd('okidogi', 86).
pokemon_spd('munkidori', 90).
pokemon_spd('fezandipiti', 125).
pokemon_spd('ogerpon', 96).
pokemon_spd('ogerponwellspring', 96).
pokemon_spd('ogerponhearthflame', 96).
pokemon_spd('ogerponcornerstone', 96).
pokemon_spd('ogerpontealtera', 96).
pokemon_spd('ogerponwellspringtera', 96).
pokemon_spd('ogerponhearthflametera', 96).
pokemon_spd('ogerponcornerstonetera', 96).
pokemon_spd('archaludon', 65).
pokemon_spd('hydrapple', 80).
pokemon_spd('gougingfire', 93).
pokemon_spd('ragingbolt', 89).
pokemon_spd('ironboulder', 108).
pokemon_spd('ironcrown', 108).
pokemon_spd('terapagos', 85).
pokemon_spd('terapagosterastal', 110).
pokemon_spd('terapagosstellar', 110).
pokemon_spd('pecharunt', 88).
pokemon_spd('missingno', 6).
pokemon_spd('ramnarok', 0).
pokemon_spd('ramnarokradiant', 0).
pokemon_spd('pokestarsmeargle', 45).
pokemon_spd('pokestarufo', 100).
pokemon_spd('pokestarufo2', 100).
pokemon_spd('pokestarbrycenman', 100).
pokemon_spd('pokestarmt', 100).
pokemon_spd('pokestarmt2', 100).
pokemon_spd('pokestartransport', 100).
pokemon_spd('pokestargiant', 100).
pokemon_spd('pokestarhumanoid', 100).
pokemon_spd('pokestarmonster', 100).
pokemon_spd('pokestarf00', 100).
pokemon_spd('pokestarf002', 100).
pokemon_spd('pokestarspirit', 100).
pokemon_spd('pokestarblackdoor', 100).
pokemon_spd('pokestarwhitedoor', 100).
pokemon_spd('pokestarblackbelt', 100).
pokemon_spd('pokestarufopropu2', 100).
pokemon_spe('bulbasaur', 45).
pokemon_spe('ivysaur', 60).
pokemon_spe('venusaur', 80).
pokemon_spe('venusaurmega', 80).
pokemon_spe('venusaurgmax', 80).
pokemon_spe('charmander', 65).
pokemon_spe('charmeleon', 80).
pokemon_spe('charizard', 100).
pokemon_spe('charizardmegax', 100).
pokemon_spe('charizardmegay', 100).
pokemon_spe('charizardgmax', 100).
pokemon_spe('squirtle', 43).
pokemon_spe('wartortle', 58).
pokemon_spe('blastoise', 78).
pokemon_spe('blastoisemega', 78).
pokemon_spe('blastoisegmax', 78).
pokemon_spe('caterpie', 45).
pokemon_spe('metapod', 30).
pokemon_spe('butterfree', 70).
pokemon_spe('butterfreegmax', 70).
pokemon_spe('weedle', 50).
pokemon_spe('kakuna', 35).
pokemon_spe('beedrill', 75).
pokemon_spe('beedrillmega', 145).
pokemon_spe('pidgey', 56).
pokemon_spe('pidgeotto', 71).
pokemon_spe('pidgeot', 101).
pokemon_spe('pidgeotmega', 121).
pokemon_spe('rattata', 72).
pokemon_spe('rattataalola', 72).
pokemon_spe('raticate', 97).
pokemon_spe('raticatealola', 77).
pokemon_spe('raticatealolatotem', 77).
pokemon_spe('spearow', 70).
pokemon_spe('fearow', 100).
pokemon_spe('ekans', 55).
pokemon_spe('arbok', 80).
pokemon_spe('pikachu', 90).
pokemon_spe('pikachucosplay', 90).
pokemon_spe('pikachurockstar', 90).
pokemon_spe('pikachubelle', 90).
pokemon_spe('pikachupopstar', 90).
pokemon_spe('pikachuphd', 90).
pokemon_spe('pikachulibre', 90).
pokemon_spe('pikachuoriginal', 90).
pokemon_spe('pikachuhoenn', 90).
pokemon_spe('pikachusinnoh', 90).
pokemon_spe('pikachuunova', 90).
pokemon_spe('pikachukalos', 90).
pokemon_spe('pikachualola', 90).
pokemon_spe('pikachupartner', 90).
pokemon_spe('pikachustarter', 120).
pokemon_spe('pikachugmax', 90).
pokemon_spe('pikachuworld', 90).
pokemon_spe('raichu', 110).
pokemon_spe('raichualola', 110).
pokemon_spe('sandshrew', 40).
pokemon_spe('sandshrewalola', 40).
pokemon_spe('sandslash', 65).
pokemon_spe('sandslashalola', 65).
pokemon_spe('nidoranf', 41).
pokemon_spe('nidorina', 56).
pokemon_spe('nidoqueen', 76).
pokemon_spe('nidoranm', 50).
pokemon_spe('nidorino', 65).
pokemon_spe('nidoking', 85).
pokemon_spe('clefairy', 35).
pokemon_spe('clefable', 60).
pokemon_spe('clefablemega', 0).
pokemon_spe('vulpix', 65).
pokemon_spe('vulpixalola', 65).
pokemon_spe('ninetales', 100).
pokemon_spe('ninetalesalola', 109).
pokemon_spe('jigglypuff', 20).
pokemon_spe('wigglytuff', 45).
pokemon_spe('zubat', 55).
pokemon_spe('golbat', 90).
pokemon_spe('oddish', 30).
pokemon_spe('gloom', 40).
pokemon_spe('vileplume', 50).
pokemon_spe('paras', 25).
pokemon_spe('parasect', 30).
pokemon_spe('venonat', 45).
pokemon_spe('venomoth', 90).
pokemon_spe('diglett', 95).
pokemon_spe('diglettalola', 90).
pokemon_spe('dugtrio', 120).
pokemon_spe('dugtrioalola', 110).
pokemon_spe('meowth', 90).
pokemon_spe('meowthalola', 90).
pokemon_spe('meowthgalar', 40).
pokemon_spe('meowthgmax', 90).
pokemon_spe('persian', 115).
pokemon_spe('persianalola', 115).
pokemon_spe('psyduck', 55).
pokemon_spe('golduck', 85).
pokemon_spe('mankey', 70).
pokemon_spe('primeape', 95).
pokemon_spe('growlithe', 60).
pokemon_spe('growlithehisui', 55).
pokemon_spe('arcanine', 95).
pokemon_spe('arcaninehisui', 90).
pokemon_spe('poliwag', 90).
pokemon_spe('poliwhirl', 90).
pokemon_spe('poliwrath', 70).
pokemon_spe('abra', 90).
pokemon_spe('kadabra', 105).
pokemon_spe('alakazam', 120).
pokemon_spe('alakazammega', 150).
pokemon_spe('machop', 35).
pokemon_spe('machoke', 45).
pokemon_spe('machamp', 55).
pokemon_spe('machampgmax', 55).
pokemon_spe('bellsprout', 40).
pokemon_spe('weepinbell', 55).
pokemon_spe('victreebel', 70).
pokemon_spe('victreebelmega', 0).
pokemon_spe('tentacool', 70).
pokemon_spe('tentacruel', 100).
pokemon_spe('geodude', 20).
pokemon_spe('geodudealola', 20).
pokemon_spe('graveler', 35).
pokemon_spe('graveleralola', 35).
pokemon_spe('golem', 45).
pokemon_spe('golemalola', 45).
pokemon_spe('ponyta', 90).
pokemon_spe('ponytagalar', 90).
pokemon_spe('rapidash', 105).
pokemon_spe('rapidashgalar', 105).
pokemon_spe('slowpoke', 15).
pokemon_spe('slowpokegalar', 15).
pokemon_spe('slowbro', 30).
pokemon_spe('slowbromega', 30).
pokemon_spe('slowbrogalar', 30).
pokemon_spe('magnemite', 45).
pokemon_spe('magneton', 70).
pokemon_spe('farfetchd', 60).
pokemon_spe('farfetchdgalar', 55).
pokemon_spe('doduo', 75).
pokemon_spe('dodrio', 110).
pokemon_spe('seel', 45).
pokemon_spe('dewgong', 70).
pokemon_spe('grimer', 25).
pokemon_spe('grimeralola', 25).
pokemon_spe('muk', 50).
pokemon_spe('mukalola', 50).
pokemon_spe('shellder', 40).
pokemon_spe('cloyster', 70).
pokemon_spe('gastly', 80).
pokemon_spe('haunter', 95).
pokemon_spe('gengar', 110).
pokemon_spe('gengarmega', 130).
pokemon_spe('gengargmax', 110).
pokemon_spe('onix', 70).
pokemon_spe('drowzee', 42).
pokemon_spe('hypno', 67).
pokemon_spe('krabby', 50).
pokemon_spe('kingler', 75).
pokemon_spe('kinglergmax', 75).
pokemon_spe('voltorb', 100).
pokemon_spe('voltorbhisui', 100).
pokemon_spe('electrode', 150).
pokemon_spe('electrodehisui', 150).
pokemon_spe('exeggcute', 40).
pokemon_spe('exeggutor', 55).
pokemon_spe('exeggutoralola', 45).
pokemon_spe('cubone', 35).
pokemon_spe('marowak', 45).
pokemon_spe('marowakalola', 45).
pokemon_spe('marowakalolatotem', 45).
pokemon_spe('hitmonlee', 87).
pokemon_spe('hitmonchan', 76).
pokemon_spe('lickitung', 30).
pokemon_spe('koffing', 35).
pokemon_spe('weezing', 60).
pokemon_spe('weezinggalar', 60).
pokemon_spe('rhyhorn', 25).
pokemon_spe('rhydon', 40).
pokemon_spe('chansey', 50).
pokemon_spe('tangela', 60).
pokemon_spe('kangaskhan', 90).
pokemon_spe('kangaskhanmega', 100).
pokemon_spe('horsea', 60).
pokemon_spe('seadra', 85).
pokemon_spe('goldeen', 63).
pokemon_spe('seaking', 68).
pokemon_spe('staryu', 85).
pokemon_spe('starmie', 115).
pokemon_spe('starmiemega', 0).
pokemon_spe('mrmime', 90).
pokemon_spe('mrmimegalar', 100).
pokemon_spe('scyther', 105).
pokemon_spe('jynx', 95).
pokemon_spe('electabuzz', 105).
pokemon_spe('magmar', 93).
pokemon_spe('pinsir', 85).
pokemon_spe('pinsirmega', 105).
pokemon_spe('tauros', 110).
pokemon_spe('taurospaldeacombat', 100).
pokemon_spe('taurospaldeablaze', 100).
pokemon_spe('taurospaldeaaqua', 100).
pokemon_spe('magikarp', 80).
pokemon_spe('gyarados', 81).
pokemon_spe('gyaradosmega', 81).
pokemon_spe('lapras', 60).
pokemon_spe('laprasgmax', 60).
pokemon_spe('ditto', 48).
pokemon_spe('eevee', 55).
pokemon_spe('eeveestarter', 75).
pokemon_spe('eeveegmax', 55).
pokemon_spe('vaporeon', 65).
pokemon_spe('jolteon', 130).
pokemon_spe('flareon', 65).
pokemon_spe('porygon', 40).
pokemon_spe('omanyte', 35).
pokemon_spe('omastar', 55).
pokemon_spe('kabuto', 55).
pokemon_spe('kabutops', 80).
pokemon_spe('aerodactyl', 130).
pokemon_spe('aerodactylmega', 150).
pokemon_spe('snorlax', 30).
pokemon_spe('snorlaxgmax', 30).
pokemon_spe('articuno', 85).
pokemon_spe('articunogalar', 95).
pokemon_spe('zapdos', 100).
pokemon_spe('zapdosgalar', 100).
pokemon_spe('moltres', 90).
pokemon_spe('moltresgalar', 90).
pokemon_spe('dratini', 50).
pokemon_spe('dragonair', 70).
pokemon_spe('dragonite', 80).
pokemon_spe('dragonitemega', 0).
pokemon_spe('mewtwo', 130).
pokemon_spe('mewtwomegax', 130).
pokemon_spe('mewtwomegay', 140).
pokemon_spe('mew', 100).
pokemon_spe('chikorita', 45).
pokemon_spe('bayleef', 60).
pokemon_spe('meganium', 80).
pokemon_spe('meganiummega', 0).
pokemon_spe('cyndaquil', 65).
pokemon_spe('quilava', 80).
pokemon_spe('typhlosion', 100).
pokemon_spe('typhlosionhisui', 95).
pokemon_spe('totodile', 43).
pokemon_spe('croconaw', 58).
pokemon_spe('feraligatr', 78).
pokemon_spe('feraligatrmega', 0).
pokemon_spe('sentret', 20).
pokemon_spe('furret', 90).
pokemon_spe('hoothoot', 50).
pokemon_spe('noctowl', 70).
pokemon_spe('ledyba', 55).
pokemon_spe('ledian', 85).
pokemon_spe('spinarak', 30).
pokemon_spe('ariados', 40).
pokemon_spe('crobat', 130).
pokemon_spe('chinchou', 67).
pokemon_spe('lanturn', 67).
pokemon_spe('pichu', 60).
pokemon_spe('pichuspikyeared', 60).
pokemon_spe('cleffa', 15).
pokemon_spe('igglybuff', 15).
pokemon_spe('togepi', 20).
pokemon_spe('togetic', 40).
pokemon_spe('natu', 70).
pokemon_spe('xatu', 95).
pokemon_spe('mareep', 35).
pokemon_spe('flaaffy', 45).
pokemon_spe('ampharos', 55).
pokemon_spe('ampharosmega', 45).
pokemon_spe('bellossom', 50).
pokemon_spe('marill', 40).
pokemon_spe('azumarill', 50).
pokemon_spe('sudowoodo', 30).
pokemon_spe('politoed', 70).
pokemon_spe('hoppip', 50).
pokemon_spe('skiploom', 80).
pokemon_spe('jumpluff', 110).
pokemon_spe('aipom', 85).
pokemon_spe('sunkern', 30).
pokemon_spe('sunflora', 30).
pokemon_spe('yanma', 95).
pokemon_spe('wooper', 15).
pokemon_spe('wooperpaldea', 15).
pokemon_spe('quagsire', 35).
pokemon_spe('espeon', 110).
pokemon_spe('umbreon', 65).
pokemon_spe('murkrow', 91).
pokemon_spe('slowking', 30).
pokemon_spe('slowkinggalar', 30).
pokemon_spe('misdreavus', 85).
pokemon_spe('unown', 48).
pokemon_spe('wobbuffet', 33).
pokemon_spe('girafarig', 85).
pokemon_spe('pineco', 15).
pokemon_spe('forretress', 40).
pokemon_spe('dunsparce', 45).
pokemon_spe('gligar', 85).
pokemon_spe('steelix', 30).
pokemon_spe('steelixmega', 30).
pokemon_spe('snubbull', 30).
pokemon_spe('granbull', 45).
pokemon_spe('qwilfish', 85).
pokemon_spe('qwilfishhisui', 85).
pokemon_spe('scizor', 65).
pokemon_spe('scizormega', 75).
pokemon_spe('shuckle', 5).
pokemon_spe('heracross', 85).
pokemon_spe('heracrossmega', 75).
pokemon_spe('sneasel', 115).
pokemon_spe('sneaselhisui', 115).
pokemon_spe('teddiursa', 40).
pokemon_spe('ursaring', 55).
pokemon_spe('slugma', 20).
pokemon_spe('magcargo', 30).
pokemon_spe('swinub', 50).
pokemon_spe('piloswine', 50).
pokemon_spe('corsola', 35).
pokemon_spe('corsolagalar', 30).
pokemon_spe('remoraid', 65).
pokemon_spe('octillery', 45).
pokemon_spe('delibird', 75).
pokemon_spe('mantine', 70).
pokemon_spe('skarmory', 70).
pokemon_spe('skarmorymega', 0).
pokemon_spe('houndour', 65).
pokemon_spe('houndoom', 95).
pokemon_spe('houndoommega', 115).
pokemon_spe('kingdra', 85).
pokemon_spe('phanpy', 40).
pokemon_spe('donphan', 50).
pokemon_spe('porygon2', 60).
pokemon_spe('stantler', 85).
pokemon_spe('smeargle', 75).
pokemon_spe('tyrogue', 35).
pokemon_spe('hitmontop', 70).
pokemon_spe('smoochum', 65).
pokemon_spe('elekid', 95).
pokemon_spe('magby', 83).
pokemon_spe('miltank', 100).
pokemon_spe('blissey', 55).
pokemon_spe('raikou', 115).
pokemon_spe('entei', 100).
pokemon_spe('suicune', 85).
pokemon_spe('larvitar', 41).
pokemon_spe('pupitar', 51).
pokemon_spe('tyranitar', 61).
pokemon_spe('tyranitarmega', 71).
pokemon_spe('lugia', 110).
pokemon_spe('hooh', 90).
pokemon_spe('celebi', 100).
pokemon_spe('treecko', 70).
pokemon_spe('grovyle', 95).
pokemon_spe('sceptile', 120).
pokemon_spe('sceptilemega', 145).
pokemon_spe('torchic', 45).
pokemon_spe('combusken', 55).
pokemon_spe('blaziken', 80).
pokemon_spe('blazikenmega', 100).
pokemon_spe('mudkip', 40).
pokemon_spe('marshtomp', 50).
pokemon_spe('swampert', 60).
pokemon_spe('swampertmega', 70).
pokemon_spe('poochyena', 35).
pokemon_spe('mightyena', 70).
pokemon_spe('zigzagoon', 60).
pokemon_spe('zigzagoongalar', 60).
pokemon_spe('linoone', 100).
pokemon_spe('linoonegalar', 100).
pokemon_spe('wurmple', 20).
pokemon_spe('silcoon', 15).
pokemon_spe('beautifly', 65).
pokemon_spe('cascoon', 15).
pokemon_spe('dustox', 65).
pokemon_spe('lotad', 30).
pokemon_spe('lombre', 50).
pokemon_spe('ludicolo', 70).
pokemon_spe('seedot', 30).
pokemon_spe('nuzleaf', 60).
pokemon_spe('shiftry', 80).
pokemon_spe('taillow', 85).
pokemon_spe('swellow', 125).
pokemon_spe('wingull', 85).
pokemon_spe('pelipper', 65).
pokemon_spe('ralts', 40).
pokemon_spe('kirlia', 50).
pokemon_spe('gardevoir', 80).
pokemon_spe('gardevoirmega', 100).
pokemon_spe('surskit', 65).
pokemon_spe('masquerain', 80).
pokemon_spe('shroomish', 35).
pokemon_spe('breloom', 70).
pokemon_spe('slakoth', 30).
pokemon_spe('vigoroth', 90).
pokemon_spe('slaking', 100).
pokemon_spe('nincada', 40).
pokemon_spe('ninjask', 160).
pokemon_spe('shedinja', 40).
pokemon_spe('whismur', 28).
pokemon_spe('loudred', 48).
pokemon_spe('exploud', 68).
pokemon_spe('makuhita', 25).
pokemon_spe('hariyama', 50).
pokemon_spe('azurill', 20).
pokemon_spe('nosepass', 30).
pokemon_spe('skitty', 50).
pokemon_spe('delcatty', 90).
pokemon_spe('sableye', 50).
pokemon_spe('sableyemega', 20).
pokemon_spe('mawile', 50).
pokemon_spe('mawilemega', 50).
pokemon_spe('aron', 30).
pokemon_spe('lairon', 40).
pokemon_spe('aggron', 50).
pokemon_spe('aggronmega', 50).
pokemon_spe('meditite', 60).
pokemon_spe('medicham', 80).
pokemon_spe('medichammega', 100).
pokemon_spe('electrike', 65).
pokemon_spe('manectric', 105).
pokemon_spe('manectricmega', 135).
pokemon_spe('plusle', 95).
pokemon_spe('minun', 95).
pokemon_spe('volbeat', 85).
pokemon_spe('illumise', 85).
pokemon_spe('roselia', 65).
pokemon_spe('gulpin', 40).
pokemon_spe('swalot', 55).
pokemon_spe('carvanha', 65).
pokemon_spe('sharpedo', 95).
pokemon_spe('sharpedomega', 105).
pokemon_spe('wailmer', 60).
pokemon_spe('wailord', 60).
pokemon_spe('numel', 35).
pokemon_spe('camerupt', 40).
pokemon_spe('cameruptmega', 20).
pokemon_spe('torkoal', 20).
pokemon_spe('spoink', 60).
pokemon_spe('grumpig', 80).
pokemon_spe('spinda', 60).
pokemon_spe('trapinch', 10).
pokemon_spe('vibrava', 70).
pokemon_spe('flygon', 100).
pokemon_spe('cacnea', 35).
pokemon_spe('cacturne', 55).
pokemon_spe('swablu', 50).
pokemon_spe('altaria', 80).
pokemon_spe('altariamega', 80).
pokemon_spe('zangoose', 90).
pokemon_spe('seviper', 65).
pokemon_spe('lunatone', 70).
pokemon_spe('solrock', 70).
pokemon_spe('barboach', 60).
pokemon_spe('whiscash', 60).
pokemon_spe('corphish', 35).
pokemon_spe('crawdaunt', 55).
pokemon_spe('baltoy', 55).
pokemon_spe('claydol', 75).
pokemon_spe('lileep', 23).
pokemon_spe('cradily', 43).
pokemon_spe('anorith', 75).
pokemon_spe('armaldo', 45).
pokemon_spe('feebas', 80).
pokemon_spe('milotic', 81).
pokemon_spe('castform', 70).
pokemon_spe('castformsunny', 70).
pokemon_spe('castformrainy', 70).
pokemon_spe('castformsnowy', 70).
pokemon_spe('kecleon', 40).
pokemon_spe('shuppet', 45).
pokemon_spe('banette', 65).
pokemon_spe('banettemega', 75).
pokemon_spe('duskull', 25).
pokemon_spe('dusclops', 25).
pokemon_spe('tropius', 51).
pokemon_spe('chimecho', 65).
pokemon_spe('absol', 75).
pokemon_spe('absolmega', 115).
pokemon_spe('wynaut', 23).
pokemon_spe('snorunt', 50).
pokemon_spe('glalie', 80).
pokemon_spe('glaliemega', 100).
pokemon_spe('spheal', 25).
pokemon_spe('sealeo', 45).
pokemon_spe('walrein', 65).
pokemon_spe('clamperl', 32).
pokemon_spe('huntail', 52).
pokemon_spe('gorebyss', 52).
pokemon_spe('relicanth', 55).
pokemon_spe('luvdisc', 97).
pokemon_spe('bagon', 50).
pokemon_spe('shelgon', 50).
pokemon_spe('salamence', 100).
pokemon_spe('salamencemega', 120).
pokemon_spe('beldum', 30).
pokemon_spe('metang', 50).
pokemon_spe('metagross', 70).
pokemon_spe('metagrossmega', 110).
pokemon_spe('regirock', 50).
pokemon_spe('regice', 50).
pokemon_spe('registeel', 50).
pokemon_spe('latias', 110).
pokemon_spe('latiasmega', 110).
pokemon_spe('latios', 110).
pokemon_spe('latiosmega', 110).
pokemon_spe('kyogre', 90).
pokemon_spe('kyogreprimal', 90).
pokemon_spe('groudon', 90).
pokemon_spe('groudonprimal', 90).
pokemon_spe('rayquaza', 95).
pokemon_spe('rayquazamega', 115).
pokemon_spe('jirachi', 100).
pokemon_spe('deoxys', 150).
pokemon_spe('deoxysattack', 150).
pokemon_spe('deoxysdefense', 90).
pokemon_spe('deoxysspeed', 180).
pokemon_spe('turtwig', 31).
pokemon_spe('grotle', 36).
pokemon_spe('torterra', 56).
pokemon_spe('chimchar', 61).
pokemon_spe('monferno', 81).
pokemon_spe('infernape', 108).
pokemon_spe('piplup', 40).
pokemon_spe('prinplup', 50).
pokemon_spe('empoleon', 60).
pokemon_spe('starly', 60).
pokemon_spe('staravia', 80).
pokemon_spe('staraptor', 100).
pokemon_spe('bidoof', 31).
pokemon_spe('bibarel', 71).
pokemon_spe('kricketot', 25).
pokemon_spe('kricketune', 65).
pokemon_spe('shinx', 45).
pokemon_spe('luxio', 60).
pokemon_spe('luxray', 70).
pokemon_spe('budew', 55).
pokemon_spe('roserade', 90).
pokemon_spe('cranidos', 58).
pokemon_spe('rampardos', 58).
pokemon_spe('shieldon', 30).
pokemon_spe('bastiodon', 30).
pokemon_spe('burmy', 36).
pokemon_spe('burmysandy', 36).
pokemon_spe('burmytrash', 36).
pokemon_spe('wormadam', 36).
pokemon_spe('wormadamsandy', 36).
pokemon_spe('wormadamtrash', 36).
pokemon_spe('mothim', 66).
pokemon_spe('combee', 70).
pokemon_spe('vespiquen', 40).
pokemon_spe('pachirisu', 95).
pokemon_spe('buizel', 85).
pokemon_spe('floatzel', 115).
pokemon_spe('cherubi', 35).
pokemon_spe('cherrim', 85).
pokemon_spe('cherrimsunshine', 85).
pokemon_spe('shellos', 34).
pokemon_spe('shelloseast', 34).
pokemon_spe('gastrodon', 39).
pokemon_spe('gastrodoneast', 39).
pokemon_spe('ambipom', 115).
pokemon_spe('drifloon', 70).
pokemon_spe('drifblim', 80).
pokemon_spe('buneary', 85).
pokemon_spe('lopunny', 105).
pokemon_spe('lopunnymega', 135).
pokemon_spe('mismagius', 105).
pokemon_spe('honchkrow', 71).
pokemon_spe('glameow', 85).
pokemon_spe('purugly', 112).
pokemon_spe('chingling', 45).
pokemon_spe('stunky', 74).
pokemon_spe('skuntank', 84).
pokemon_spe('bronzor', 23).
pokemon_spe('bronzong', 33).
pokemon_spe('bonsly', 10).
pokemon_spe('mimejr', 60).
pokemon_spe('happiny', 30).
pokemon_spe('chatot', 91).
pokemon_spe('spiritomb', 35).
pokemon_spe('gible', 42).
pokemon_spe('gabite', 82).
pokemon_spe('garchomp', 102).
pokemon_spe('garchompmega', 92).
pokemon_spe('munchlax', 5).
pokemon_spe('riolu', 60).
pokemon_spe('lucario', 90).
pokemon_spe('lucariomega', 112).
pokemon_spe('hippopotas', 32).
pokemon_spe('hippowdon', 47).
pokemon_spe('skorupi', 65).
pokemon_spe('drapion', 95).
pokemon_spe('croagunk', 50).
pokemon_spe('toxicroak', 85).
pokemon_spe('carnivine', 46).
pokemon_spe('finneon', 66).
pokemon_spe('lumineon', 91).
pokemon_spe('mantyke', 50).
pokemon_spe('snover', 40).
pokemon_spe('abomasnow', 60).
pokemon_spe('abomasnowmega', 30).
pokemon_spe('weavile', 125).
pokemon_spe('magnezone', 60).
pokemon_spe('lickilicky', 50).
pokemon_spe('rhyperior', 40).
pokemon_spe('tangrowth', 50).
pokemon_spe('electivire', 95).
pokemon_spe('magmortar', 83).
pokemon_spe('togekiss', 80).
pokemon_spe('yanmega', 95).
pokemon_spe('leafeon', 95).
pokemon_spe('glaceon', 65).
pokemon_spe('gliscor', 95).
pokemon_spe('mamoswine', 80).
pokemon_spe('porygonz', 90).
pokemon_spe('gallade', 80).
pokemon_spe('gallademega', 110).
pokemon_spe('probopass', 40).
pokemon_spe('dusknoir', 45).
pokemon_spe('froslass', 110).
pokemon_spe('froslassmega', 0).
pokemon_spe('rotom', 91).
pokemon_spe('rotomheat', 86).
pokemon_spe('rotomwash', 86).
pokemon_spe('rotomfrost', 86).
pokemon_spe('rotomfan', 86).
pokemon_spe('rotommow', 86).
pokemon_spe('uxie', 95).
pokemon_spe('mesprit', 80).
pokemon_spe('azelf', 115).
pokemon_spe('dialga', 90).
pokemon_spe('dialgaorigin', 90).
pokemon_spe('palkia', 100).
pokemon_spe('palkiaorigin', 120).
pokemon_spe('heatran', 77).
pokemon_spe('regigigas', 100).
pokemon_spe('giratina', 90).
pokemon_spe('giratinaorigin', 90).
pokemon_spe('cresselia', 85).
pokemon_spe('phione', 80).
pokemon_spe('manaphy', 100).
pokemon_spe('darkrai', 125).
pokemon_spe('shaymin', 100).
pokemon_spe('shayminsky', 127).
pokemon_spe('arceus', 120).
pokemon_spe('arceusbug', 120).
pokemon_spe('arceusdark', 120).
pokemon_spe('arceusdragon', 120).
pokemon_spe('arceuselectric', 120).
pokemon_spe('arceusfairy', 120).
pokemon_spe('arceusfighting', 120).
pokemon_spe('arceusfire', 120).
pokemon_spe('arceusflying', 120).
pokemon_spe('arceusghost', 120).
pokemon_spe('arceusgrass', 120).
pokemon_spe('arceusground', 120).
pokemon_spe('arceusice', 120).
pokemon_spe('arceuspoison', 120).
pokemon_spe('arceuspsychic', 120).
pokemon_spe('arceusrock', 120).
pokemon_spe('arceussteel', 120).
pokemon_spe('arceuswater', 120).
pokemon_spe('victini', 100).
pokemon_spe('snivy', 63).
pokemon_spe('servine', 83).
pokemon_spe('serperior', 113).
pokemon_spe('tepig', 45).
pokemon_spe('pignite', 55).
pokemon_spe('emboar', 65).
pokemon_spe('emboarmega', 0).
pokemon_spe('oshawott', 45).
pokemon_spe('dewott', 60).
pokemon_spe('samurott', 70).
pokemon_spe('samurotthisui', 85).
pokemon_spe('patrat', 42).
pokemon_spe('watchog', 77).
pokemon_spe('lillipup', 55).
pokemon_spe('herdier', 60).
pokemon_spe('stoutland', 80).
pokemon_spe('purrloin', 66).
pokemon_spe('liepard', 106).
pokemon_spe('pansage', 64).
pokemon_spe('simisage', 101).
pokemon_spe('pansear', 64).
pokemon_spe('simisear', 101).
pokemon_spe('panpour', 64).
pokemon_spe('simipour', 101).
pokemon_spe('munna', 24).
pokemon_spe('musharna', 29).
pokemon_spe('pidove', 43).
pokemon_spe('tranquill', 65).
pokemon_spe('unfezant', 93).
pokemon_spe('blitzle', 76).
pokemon_spe('zebstrika', 116).
pokemon_spe('roggenrola', 15).
pokemon_spe('boldore', 20).
pokemon_spe('gigalith', 25).
pokemon_spe('woobat', 72).
pokemon_spe('swoobat', 114).
pokemon_spe('drilbur', 68).
pokemon_spe('excadrill', 88).
pokemon_spe('excadrillmega', 0).
pokemon_spe('audino', 50).
pokemon_spe('audinomega', 50).
pokemon_spe('timburr', 35).
pokemon_spe('gurdurr', 40).
pokemon_spe('conkeldurr', 45).
pokemon_spe('tympole', 64).
pokemon_spe('palpitoad', 69).
pokemon_spe('seismitoad', 74).
pokemon_spe('throh', 45).
pokemon_spe('sawk', 85).
pokemon_spe('sewaddle', 42).
pokemon_spe('swadloon', 42).
pokemon_spe('leavanny', 92).
pokemon_spe('venipede', 57).
pokemon_spe('whirlipede', 47).
pokemon_spe('scolipede', 112).
pokemon_spe('scolipedemega', 0).
pokemon_spe('cottonee', 66).
pokemon_spe('whimsicott', 116).
pokemon_spe('petilil', 30).
pokemon_spe('lilligant', 90).
pokemon_spe('lilliganthisui', 105).
pokemon_spe('basculin', 98).
pokemon_spe('basculinbluestriped', 98).
pokemon_spe('basculinwhitestriped', 98).
pokemon_spe('sandile', 65).
pokemon_spe('krokorok', 74).
pokemon_spe('krookodile', 92).
pokemon_spe('darumaka', 50).
pokemon_spe('darumakagalar', 50).
pokemon_spe('darmanitan', 95).
pokemon_spe('darmanitanzen', 55).
pokemon_spe('darmanitangalar', 95).
pokemon_spe('darmanitangalarzen', 135).
pokemon_spe('maractus', 60).
pokemon_spe('dwebble', 55).
pokemon_spe('crustle', 45).
pokemon_spe('scraggy', 48).
pokemon_spe('scrafty', 58).
pokemon_spe('scraftymega', 0).
pokemon_spe('sigilyph', 97).
pokemon_spe('yamask', 30).
pokemon_spe('yamaskgalar', 30).
pokemon_spe('cofagrigus', 30).
pokemon_spe('tirtouga', 22).
pokemon_spe('carracosta', 32).
pokemon_spe('archen', 70).
pokemon_spe('archeops', 110).
pokemon_spe('trubbish', 65).
pokemon_spe('garbodor', 75).
pokemon_spe('garbodorgmax', 75).
pokemon_spe('zorua', 65).
pokemon_spe('zoruahisui', 70).
pokemon_spe('zoroark', 105).
pokemon_spe('zoroarkhisui', 110).
pokemon_spe('minccino', 75).
pokemon_spe('cinccino', 115).
pokemon_spe('gothita', 45).
pokemon_spe('gothorita', 55).
pokemon_spe('gothitelle', 65).
pokemon_spe('solosis', 20).
pokemon_spe('duosion', 30).
pokemon_spe('reuniclus', 30).
pokemon_spe('ducklett', 55).
pokemon_spe('swanna', 98).
pokemon_spe('vanillite', 44).
pokemon_spe('vanillish', 59).
pokemon_spe('vanilluxe', 79).
pokemon_spe('deerling', 75).
pokemon_spe('deerlingsummer', 75).
pokemon_spe('deerlingautumn', 75).
pokemon_spe('deerlingwinter', 75).
pokemon_spe('sawsbuck', 95).
pokemon_spe('emolga', 103).
pokemon_spe('karrablast', 60).
pokemon_spe('escavalier', 20).
pokemon_spe('foongus', 15).
pokemon_spe('amoonguss', 30).
pokemon_spe('frillish', 40).
pokemon_spe('jellicent', 60).
pokemon_spe('alomomola', 65).
pokemon_spe('joltik', 65).
pokemon_spe('galvantula', 108).
pokemon_spe('ferroseed', 10).
pokemon_spe('ferrothorn', 20).
pokemon_spe('klink', 30).
pokemon_spe('klang', 50).
pokemon_spe('klinklang', 90).
pokemon_spe('tynamo', 60).
pokemon_spe('eelektrik', 40).
pokemon_spe('eelektross', 50).
pokemon_spe('eelektrossmega', 0).
pokemon_spe('elgyem', 30).
pokemon_spe('beheeyem', 40).
pokemon_spe('litwick', 20).
pokemon_spe('lampent', 55).
pokemon_spe('chandelure', 80).
pokemon_spe('chandeluremega', 0).
pokemon_spe('axew', 57).
pokemon_spe('fraxure', 67).
pokemon_spe('haxorus', 97).
pokemon_spe('cubchoo', 40).
pokemon_spe('beartic', 50).
pokemon_spe('cryogonal', 105).
pokemon_spe('shelmet', 25).
pokemon_spe('accelgor', 145).
pokemon_spe('stunfisk', 32).
pokemon_spe('stunfiskgalar', 32).
pokemon_spe('mienfoo', 65).
pokemon_spe('mienshao', 105).
pokemon_spe('druddigon', 48).
pokemon_spe('golett', 35).
pokemon_spe('golurk', 55).
pokemon_spe('pawniard', 60).
pokemon_spe('bisharp', 70).
pokemon_spe('bouffalant', 55).
pokemon_spe('rufflet', 60).
pokemon_spe('braviary', 80).
pokemon_spe('braviaryhisui', 65).
pokemon_spe('vullaby', 60).
pokemon_spe('mandibuzz', 80).
pokemon_spe('heatmor', 65).
pokemon_spe('durant', 109).
pokemon_spe('deino', 38).
pokemon_spe('zweilous', 58).
pokemon_spe('hydreigon', 98).
pokemon_spe('larvesta', 60).
pokemon_spe('volcarona', 100).
pokemon_spe('cobalion', 108).
pokemon_spe('terrakion', 108).
pokemon_spe('virizion', 108).
pokemon_spe('tornadus', 111).
pokemon_spe('tornadustherian', 121).
pokemon_spe('thundurus', 111).
pokemon_spe('thundurustherian', 101).
pokemon_spe('reshiram', 90).
pokemon_spe('zekrom', 90).
pokemon_spe('landorus', 101).
pokemon_spe('landorustherian', 91).
pokemon_spe('kyurem', 95).
pokemon_spe('kyuremblack', 95).
pokemon_spe('kyuremwhite', 95).
pokemon_spe('keldeo', 108).
pokemon_spe('keldeoresolute', 108).
pokemon_spe('meloetta', 90).
pokemon_spe('meloettapirouette', 128).
pokemon_spe('genesect', 99).
pokemon_spe('genesectdouse', 99).
pokemon_spe('genesectshock', 99).
pokemon_spe('genesectburn', 99).
pokemon_spe('genesectchill', 99).
pokemon_spe('chespin', 38).
pokemon_spe('quilladin', 57).
pokemon_spe('chesnaught', 64).
pokemon_spe('chesnaughtmega', 0).
pokemon_spe('fennekin', 60).
pokemon_spe('braixen', 73).
pokemon_spe('delphox', 104).
pokemon_spe('delphoxmega', 0).
pokemon_spe('froakie', 71).
pokemon_spe('frogadier', 97).
pokemon_spe('greninja', 122).
pokemon_spe('greninjabond', 122).
pokemon_spe('greninjaash', 132).
pokemon_spe('greninjamega', 0).
pokemon_spe('bunnelby', 57).
pokemon_spe('diggersby', 78).
pokemon_spe('fletchling', 62).
pokemon_spe('fletchinder', 84).
pokemon_spe('talonflame', 126).
pokemon_spe('scatterbug', 35).
pokemon_spe('spewpa', 29).
pokemon_spe('vivillon', 89).
pokemon_spe('vivillonicysnow', 89).
pokemon_spe('vivillonpolar', 89).
pokemon_spe('vivillontundra', 89).
pokemon_spe('vivilloncontinental', 89).
pokemon_spe('vivillongarden', 89).
pokemon_spe('vivillonelegant', 89).
pokemon_spe('vivillonmodern', 89).
pokemon_spe('vivillonmarine', 89).
pokemon_spe('vivillonarchipelago', 89).
pokemon_spe('vivillonhighplains', 89).
pokemon_spe('vivillonsandstorm', 89).
pokemon_spe('vivillonriver', 89).
pokemon_spe('vivillonmonsoon', 89).
pokemon_spe('vivillonsavanna', 89).
pokemon_spe('vivillonsun', 89).
pokemon_spe('vivillonocean', 89).
pokemon_spe('vivillonjungle', 89).
pokemon_spe('vivillonfancy', 89).
pokemon_spe('vivillonpokeball', 89).
pokemon_spe('litleo', 72).
pokemon_spe('pyroar', 106).
pokemon_spe('pyroarmega', 0).
pokemon_spe('flabebe', 42).
pokemon_spe('floette', 52).
pokemon_spe('floetteeternal', 92).
pokemon_spe('floettemega', 0).
pokemon_spe('florges', 75).
pokemon_spe('skiddo', 52).
pokemon_spe('gogoat', 68).
pokemon_spe('pancham', 43).
pokemon_spe('pangoro', 58).
pokemon_spe('furfrou', 102).
pokemon_spe('espurr', 68).
pokemon_spe('meowstic', 104).
pokemon_spe('meowsticf', 104).
pokemon_spe('honedge', 28).
pokemon_spe('doublade', 35).
pokemon_spe('aegislash', 60).
pokemon_spe('aegislashblade', 60).
pokemon_spe('spritzee', 23).
pokemon_spe('aromatisse', 29).
pokemon_spe('swirlix', 49).
pokemon_spe('slurpuff', 72).
pokemon_spe('inkay', 45).
pokemon_spe('malamar', 73).
pokemon_spe('malamarmega', 0).
pokemon_spe('binacle', 50).
pokemon_spe('barbaracle', 68).
pokemon_spe('barbaraclemega', 0).
pokemon_spe('skrelp', 30).
pokemon_spe('dragalge', 44).
pokemon_spe('dragalgemega', 0).
pokemon_spe('clauncher', 44).
pokemon_spe('clawitzer', 59).
pokemon_spe('helioptile', 70).
pokemon_spe('heliolisk', 109).
pokemon_spe('tyrunt', 48).
pokemon_spe('tyrantrum', 71).
pokemon_spe('amaura', 46).
pokemon_spe('aurorus', 58).
pokemon_spe('sylveon', 60).
pokemon_spe('hawlucha', 118).
pokemon_spe('hawluchamega', 0).
pokemon_spe('dedenne', 101).
pokemon_spe('carbink', 50).
pokemon_spe('goomy', 40).
pokemon_spe('sliggoo', 60).
pokemon_spe('sliggoohisui', 40).
pokemon_spe('goodra', 80).
pokemon_spe('goodrahisui', 60).
pokemon_spe('klefki', 75).
pokemon_spe('phantump', 38).
pokemon_spe('trevenant', 56).
pokemon_spe('pumpkaboo', 51).
pokemon_spe('pumpkaboosmall', 56).
pokemon_spe('pumpkaboolarge', 46).
pokemon_spe('pumpkaboosuper', 41).
pokemon_spe('gourgeist', 84).
pokemon_spe('gourgeistsmall', 99).
pokemon_spe('gourgeistlarge', 69).
pokemon_spe('gourgeistsuper', 54).
pokemon_spe('bergmite', 28).
pokemon_spe('avalugg', 28).
pokemon_spe('avalugghisui', 38).
pokemon_spe('noibat', 55).
pokemon_spe('noivern', 123).
pokemon_spe('xerneas', 99).
pokemon_spe('xerneasneutral', 99).
pokemon_spe('yveltal', 99).
pokemon_spe('zygarde', 95).
pokemon_spe('zygarde10', 115).
pokemon_spe('zygardecomplete', 85).
pokemon_spe('zygardemega', 0).
pokemon_spe('diancie', 50).
pokemon_spe('dianciemega', 110).
pokemon_spe('hoopa', 70).
pokemon_spe('hoopaunbound', 80).
pokemon_spe('volcanion', 70).
pokemon_spe('rowlet', 42).
pokemon_spe('dartrix', 52).
pokemon_spe('decidueye', 70).
pokemon_spe('decidueyehisui', 60).
pokemon_spe('litten', 70).
pokemon_spe('torracat', 90).
pokemon_spe('incineroar', 60).
pokemon_spe('popplio', 40).
pokemon_spe('brionne', 50).
pokemon_spe('primarina', 60).
pokemon_spe('pikipek', 65).
pokemon_spe('trumbeak', 75).
pokemon_spe('toucannon', 60).
pokemon_spe('yungoos', 45).
pokemon_spe('gumshoos', 45).
pokemon_spe('gumshoostotem', 45).
pokemon_spe('grubbin', 46).
pokemon_spe('charjabug', 36).
pokemon_spe('vikavolt', 43).
pokemon_spe('vikavolttotem', 43).
pokemon_spe('crabrawler', 63).
pokemon_spe('crabominable', 43).
pokemon_spe('oricorio', 93).
pokemon_spe('oricoriopompom', 93).
pokemon_spe('oricoriopau', 93).
pokemon_spe('oricoriosensu', 93).
pokemon_spe('cutiefly', 84).
pokemon_spe('ribombee', 124).
pokemon_spe('ribombeetotem', 124).
pokemon_spe('rockruff', 60).
pokemon_spe('rockruffdusk', 60).
pokemon_spe('lycanroc', 112).
pokemon_spe('lycanrocmidnight', 82).
pokemon_spe('lycanrocdusk', 110).
pokemon_spe('wishiwashi', 40).
pokemon_spe('wishiwashischool', 30).
pokemon_spe('mareanie', 45).
pokemon_spe('toxapex', 35).
pokemon_spe('mudbray', 45).
pokemon_spe('mudsdale', 35).
pokemon_spe('dewpider', 27).
pokemon_spe('araquanid', 42).
pokemon_spe('araquanidtotem', 42).
pokemon_spe('fomantis', 35).
pokemon_spe('lurantis', 45).
pokemon_spe('lurantistotem', 45).
pokemon_spe('morelull', 15).
pokemon_spe('shiinotic', 30).
pokemon_spe('salandit', 77).
pokemon_spe('salazzle', 117).
pokemon_spe('salazzletotem', 117).
pokemon_spe('stufful', 50).
pokemon_spe('bewear', 60).
pokemon_spe('bounsweet', 32).
pokemon_spe('steenee', 62).
pokemon_spe('tsareena', 72).
pokemon_spe('comfey', 100).
pokemon_spe('oranguru', 60).
pokemon_spe('passimian', 80).
pokemon_spe('wimpod', 80).
pokemon_spe('golisopod', 40).
pokemon_spe('sandygast', 15).
pokemon_spe('palossand', 35).
pokemon_spe('pyukumuku', 5).
pokemon_spe('typenull', 59).
pokemon_spe('silvally', 95).
pokemon_spe('silvallybug', 95).
pokemon_spe('silvallydark', 95).
pokemon_spe('silvallydragon', 95).
pokemon_spe('silvallyelectric', 95).
pokemon_spe('silvallyfairy', 95).
pokemon_spe('silvallyfighting', 95).
pokemon_spe('silvallyfire', 95).
pokemon_spe('silvallyflying', 95).
pokemon_spe('silvallyghost', 95).
pokemon_spe('silvallygrass', 95).
pokemon_spe('silvallyground', 95).
pokemon_spe('silvallyice', 95).
pokemon_spe('silvallypoison', 95).
pokemon_spe('silvallypsychic', 95).
pokemon_spe('silvallyrock', 95).
pokemon_spe('silvallysteel', 95).
pokemon_spe('silvallywater', 95).
pokemon_spe('minior', 120).
pokemon_spe('miniororange', 120).
pokemon_spe('minioryellow', 120).
pokemon_spe('miniorgreen', 120).
pokemon_spe('miniorblue', 120).
pokemon_spe('miniorindigo', 120).
pokemon_spe('miniorviolet', 120).
pokemon_spe('miniormeteor', 60).
pokemon_spe('komala', 65).
pokemon_spe('turtonator', 36).
pokemon_spe('togedemaru', 96).
pokemon_spe('togedemarutotem', 96).
pokemon_spe('mimikyu', 96).
pokemon_spe('mimikyubusted', 96).
pokemon_spe('mimikyutotem', 96).
pokemon_spe('mimikyubustedtotem', 96).
pokemon_spe('bruxish', 92).
pokemon_spe('drampa', 36).
pokemon_spe('drampamega', 0).
pokemon_spe('dhelmise', 40).
pokemon_spe('jangmoo', 45).
pokemon_spe('hakamoo', 65).
pokemon_spe('kommoo', 85).
pokemon_spe('kommoototem', 85).
pokemon_spe('tapukoko', 130).
pokemon_spe('tapulele', 95).
pokemon_spe('tapubulu', 75).
pokemon_spe('tapufini', 85).
pokemon_spe('cosmog', 37).
pokemon_spe('cosmoem', 37).
pokemon_spe('solgaleo', 97).
pokemon_spe('lunala', 97).
pokemon_spe('nihilego', 103).
pokemon_spe('buzzwole', 79).
pokemon_spe('pheromosa', 151).
pokemon_spe('xurkitree', 83).
pokemon_spe('celesteela', 61).
pokemon_spe('kartana', 109).
pokemon_spe('guzzlord', 43).
pokemon_spe('necrozma', 79).
pokemon_spe('necrozmaduskmane', 77).
pokemon_spe('necrozmadawnwings', 77).
pokemon_spe('necrozmaultra', 129).
pokemon_spe('magearna', 65).
pokemon_spe('magearnaoriginal', 65).
pokemon_spe('marshadow', 125).
pokemon_spe('poipole', 73).
pokemon_spe('naganadel', 121).
pokemon_spe('stakataka', 13).
pokemon_spe('blacephalon', 107).
pokemon_spe('zeraora', 143).
pokemon_spe('meltan', 34).
pokemon_spe('melmetal', 34).
pokemon_spe('melmetalgmax', 34).
pokemon_spe('grookey', 65).
pokemon_spe('thwackey', 80).
pokemon_spe('rillaboom', 85).
pokemon_spe('rillaboomgmax', 85).
pokemon_spe('scorbunny', 69).
pokemon_spe('raboot', 94).
pokemon_spe('cinderace', 119).
pokemon_spe('cinderacegmax', 119).
pokemon_spe('sobble', 70).
pokemon_spe('drizzile', 90).
pokemon_spe('inteleon', 120).
pokemon_spe('inteleongmax', 120).
pokemon_spe('skwovet', 25).
pokemon_spe('greedent', 20).
pokemon_spe('rookidee', 57).
pokemon_spe('corvisquire', 77).
pokemon_spe('corviknight', 67).
pokemon_spe('corviknightgmax', 67).
pokemon_spe('blipbug', 45).
pokemon_spe('dottler', 30).
pokemon_spe('orbeetle', 90).
pokemon_spe('orbeetlegmax', 90).
pokemon_spe('nickit', 50).
pokemon_spe('thievul', 90).
pokemon_spe('gossifleur', 10).
pokemon_spe('eldegoss', 60).
pokemon_spe('wooloo', 48).
pokemon_spe('dubwool', 88).
pokemon_spe('chewtle', 44).
pokemon_spe('drednaw', 74).
pokemon_spe('drednawgmax', 74).
pokemon_spe('yamper', 26).
pokemon_spe('boltund', 121).
pokemon_spe('rolycoly', 30).
pokemon_spe('carkol', 50).
pokemon_spe('coalossal', 30).
pokemon_spe('coalossalgmax', 30).
pokemon_spe('applin', 20).
pokemon_spe('flapple', 70).
pokemon_spe('flapplegmax', 70).
pokemon_spe('appletun', 30).
pokemon_spe('appletungmax', 30).
pokemon_spe('silicobra', 46).
pokemon_spe('sandaconda', 71).
pokemon_spe('sandacondagmax', 71).
pokemon_spe('cramorant', 85).
pokemon_spe('cramorantgulping', 85).
pokemon_spe('cramorantgorging', 85).
pokemon_spe('arrokuda', 66).
pokemon_spe('barraskewda', 136).
pokemon_spe('toxel', 40).
pokemon_spe('toxtricity', 75).
pokemon_spe('toxtricitylowkey', 75).
pokemon_spe('toxtricitygmax', 75).
pokemon_spe('toxtricitylowkeygmax', 75).
pokemon_spe('sizzlipede', 45).
pokemon_spe('centiskorch', 65).
pokemon_spe('centiskorchgmax', 65).
pokemon_spe('clobbopus', 32).
pokemon_spe('grapploct', 42).
pokemon_spe('sinistea', 50).
pokemon_spe('sinisteaantique', 50).
pokemon_spe('polteageist', 70).
pokemon_spe('polteageistantique', 70).
pokemon_spe('hatenna', 39).
pokemon_spe('hattrem', 49).
pokemon_spe('hatterene', 29).
pokemon_spe('hatterenegmax', 29).
pokemon_spe('impidimp', 50).
pokemon_spe('morgrem', 70).
pokemon_spe('grimmsnarl', 60).
pokemon_spe('grimmsnarlgmax', 60).
pokemon_spe('obstagoon', 95).
pokemon_spe('perrserker', 50).
pokemon_spe('cursola', 30).
pokemon_spe('sirfetchd', 65).
pokemon_spe('mrrime', 70).
pokemon_spe('runerigus', 30).
pokemon_spe('milcery', 34).
pokemon_spe('alcremie', 64).
pokemon_spe('alcremierubycream', 64).
pokemon_spe('alcremiematchacream', 64).
pokemon_spe('alcremiemintcream', 64).
pokemon_spe('alcremielemoncream', 64).
pokemon_spe('alcremierubyswirl', 64).
pokemon_spe('alcremiecaramelswirl', 64).
pokemon_spe('alcremierainbowswirl', 64).
pokemon_spe('alcremiegmax', 64).
pokemon_spe('falinks', 75).
pokemon_spe('falinksmega', 0).
pokemon_spe('pincurchin', 15).
pokemon_spe('snom', 20).
pokemon_spe('frosmoth', 65).
pokemon_spe('stonjourner', 70).
pokemon_spe('eiscue', 50).
pokemon_spe('eiscuenoice', 130).
pokemon_spe('indeedee', 95).
pokemon_spe('indeedeef', 85).
pokemon_spe('morpeko', 97).
pokemon_spe('morpekohangry', 97).
pokemon_spe('cufant', 40).
pokemon_spe('copperajah', 30).
pokemon_spe('copperajahgmax', 30).
pokemon_spe('dracozolt', 75).
pokemon_spe('arctozolt', 55).
pokemon_spe('dracovish', 75).
pokemon_spe('arctovish', 55).
pokemon_spe('duraludon', 85).
pokemon_spe('duraludongmax', 85).
pokemon_spe('dreepy', 82).
pokemon_spe('drakloak', 102).
pokemon_spe('dragapult', 142).
pokemon_spe('zacian', 138).
pokemon_spe('zaciancrowned', 148).
pokemon_spe('zamazenta', 138).
pokemon_spe('zamazentacrowned', 128).
pokemon_spe('eternatus', 130).
pokemon_spe('eternatuseternamax', 130).
pokemon_spe('kubfu', 72).
pokemon_spe('urshifu', 97).
pokemon_spe('urshifurapidstrike', 97).
pokemon_spe('urshifugmax', 97).
pokemon_spe('urshifurapidstrikegmax', 97).
pokemon_spe('zarude', 105).
pokemon_spe('zarudedada', 105).
pokemon_spe('regieleki', 200).
pokemon_spe('regidrago', 80).
pokemon_spe('glastrier', 30).
pokemon_spe('spectrier', 130).
pokemon_spe('calyrex', 80).
pokemon_spe('calyrexice', 50).
pokemon_spe('calyrexshadow', 150).
pokemon_spe('wyrdeer', 65).
pokemon_spe('kleavor', 85).
pokemon_spe('ursaluna', 50).
pokemon_spe('ursalunabloodmoon', 52).
pokemon_spe('basculegion', 78).
pokemon_spe('basculegionf', 78).
pokemon_spe('sneasler', 120).
pokemon_spe('overqwil', 85).
pokemon_spe('enamorus', 106).
pokemon_spe('enamorustherian', 46).
pokemon_spe('sprigatito', 65).
pokemon_spe('floragato', 83).
pokemon_spe('meowscarada', 123).
pokemon_spe('fuecoco', 36).
pokemon_spe('crocalor', 49).
pokemon_spe('skeledirge', 66).
pokemon_spe('quaxly', 50).
pokemon_spe('quaxwell', 65).
pokemon_spe('quaquaval', 85).
pokemon_spe('lechonk', 35).
pokemon_spe('oinkologne', 65).
pokemon_spe('oinkolognef', 65).
pokemon_spe('tarountula', 20).
pokemon_spe('spidops', 35).
pokemon_spe('nymble', 45).
pokemon_spe('lokix', 92).
pokemon_spe('pawmi', 60).
pokemon_spe('pawmo', 85).
pokemon_spe('pawmot', 105).
pokemon_spe('tandemaus', 75).
pokemon_spe('maushold', 111).
pokemon_spe('mausholdfour', 111).
pokemon_spe('fidough', 65).
pokemon_spe('dachsbun', 95).
pokemon_spe('smoliv', 30).
pokemon_spe('dolliv', 33).
pokemon_spe('arboliva', 39).
pokemon_spe('squawkabilly', 92).
pokemon_spe('squawkabillyblue', 92).
pokemon_spe('squawkabillyyellow', 92).
pokemon_spe('squawkabillywhite', 92).
pokemon_spe('nacli', 25).
pokemon_spe('naclstack', 35).
pokemon_spe('garganacl', 35).
pokemon_spe('charcadet', 35).
pokemon_spe('armarouge', 75).
pokemon_spe('ceruledge', 85).
pokemon_spe('tadbulb', 45).
pokemon_spe('bellibolt', 45).
pokemon_spe('wattrel', 70).
pokemon_spe('kilowattrel', 125).
pokemon_spe('maschiff', 51).
pokemon_spe('mabosstiff', 85).
pokemon_spe('shroodle', 75).
pokemon_spe('grafaiai', 110).
pokemon_spe('bramblin', 60).
pokemon_spe('brambleghast', 90).
pokemon_spe('toedscool', 70).
pokemon_spe('toedscruel', 100).
pokemon_spe('klawf', 75).
pokemon_spe('capsakid', 50).
pokemon_spe('scovillain', 75).
pokemon_spe('rellor', 30).
pokemon_spe('rabsca', 45).
pokemon_spe('flittle', 75).
pokemon_spe('espathra', 105).
pokemon_spe('tinkatink', 58).
pokemon_spe('tinkatuff', 78).
pokemon_spe('tinkaton', 94).
pokemon_spe('wiglett', 95).
pokemon_spe('wugtrio', 120).
pokemon_spe('bombirdier', 82).
pokemon_spe('finizen', 75).
pokemon_spe('palafin', 100).
pokemon_spe('palafinhero', 100).
pokemon_spe('varoom', 47).
pokemon_spe('revavroom', 90).
pokemon_spe('cyclizar', 121).
pokemon_spe('orthworm', 65).
pokemon_spe('glimmet', 60).
pokemon_spe('glimmora', 86).
pokemon_spe('greavard', 34).
pokemon_spe('houndstone', 68).
pokemon_spe('flamigo', 90).
pokemon_spe('cetoddle', 43).
pokemon_spe('cetitan', 73).
pokemon_spe('veluza', 70).
pokemon_spe('dondozo', 35).
pokemon_spe('tatsugiri', 82).
pokemon_spe('tatsugiridroopy', 82).
pokemon_spe('tatsugiristretchy', 82).
pokemon_spe('annihilape', 90).
pokemon_spe('clodsire', 20).
pokemon_spe('farigiraf', 60).
pokemon_spe('dudunsparce', 55).
pokemon_spe('dudunsparcethreesegment', 55).
pokemon_spe('kingambit', 50).
pokemon_spe('greattusk', 87).
pokemon_spe('screamtail', 111).
pokemon_spe('brutebonnet', 55).
pokemon_spe('fluttermane', 135).
pokemon_spe('slitherwing', 81).
pokemon_spe('sandyshocks', 101).
pokemon_spe('irontreads', 106).
pokemon_spe('ironbundle', 136).
pokemon_spe('ironhands', 50).
pokemon_spe('ironjugulis', 108).
pokemon_spe('ironmoth', 110).
pokemon_spe('ironthorns', 72).
pokemon_spe('frigibax', 55).
pokemon_spe('arctibax', 62).
pokemon_spe('baxcalibur', 87).
pokemon_spe('gimmighoul', 10).
pokemon_spe('gimmighoulroaming', 80).
pokemon_spe('gholdengo', 84).
pokemon_spe('wochien', 70).
pokemon_spe('chienpao', 135).
pokemon_spe('tinglu', 45).
pokemon_spe('chiyu', 100).
pokemon_spe('roaringmoon', 119).
pokemon_spe('ironvaliant', 116).
pokemon_spe('koraidon', 135).
pokemon_spe('miraidon', 135).
pokemon_spe('walkingwake', 109).
pokemon_spe('ironleaves', 104).
pokemon_spe('dipplin', 40).
pokemon_spe('poltchageist', 50).
pokemon_spe('poltchageistartisan', 50).
pokemon_spe('sinistcha', 70).
pokemon_spe('sinistchamasterpiece', 70).
pokemon_spe('okidogi', 80).
pokemon_spe('munkidori', 106).
pokemon_spe('fezandipiti', 99).
pokemon_spe('ogerpon', 110).
pokemon_spe('ogerponwellspring', 110).
pokemon_spe('ogerponhearthflame', 110).
pokemon_spe('ogerponcornerstone', 110).
pokemon_spe('ogerpontealtera', 110).
pokemon_spe('ogerponwellspringtera', 110).
pokemon_spe('ogerponhearthflametera', 110).
pokemon_spe('ogerponcornerstonetera', 110).
pokemon_spe('archaludon', 85).
pokemon_spe('hydrapple', 44).
pokemon_spe('gougingfire', 91).
pokemon_spe('ragingbolt', 75).
pokemon_spe('ironboulder', 124).
pokemon_spe('ironcrown', 98).
pokemon_spe('terapagos', 60).
pokemon_spe('terapagosterastal', 85).
pokemon_spe('terapagosstellar', 85).
pokemon_spe('pecharunt', 88).
pokemon_spe('missingno', 29).
pokemon_spe('ramnarok', 0).
pokemon_spe('ramnarokradiant', 0).
pokemon_spe('pokestarsmeargle', 75).
pokemon_spe('pokestarufo', 100).
pokemon_spe('pokestarufo2', 100).
pokemon_spe('pokestarbrycenman', 100).
pokemon_spe('pokestarmt', 100).
pokemon_spe('pokestarmt2', 100).
pokemon_spe('pokestartransport', 100).
pokemon_spe('pokestargiant', 100).
pokemon_spe('pokestarhumanoid', 100).
pokemon_spe('pokestarmonster', 100).
pokemon_spe('pokestarf00', 100).
pokemon_spe('pokestarf002', 100).
pokemon_spe('pokestarspirit', 100).
pokemon_spe('pokestarblackdoor', 100).
pokemon_spe('pokestarwhitedoor', 100).
pokemon_spe('pokestarblackbelt', 100).
pokemon_spe('pokestarufopropu2', 100).
type('bulbasaur', 'grass').
type('bulbasaur', 'poison').
type('ivysaur', 'grass').
type('ivysaur', 'poison').
type('venusaur', 'grass').
type('venusaur', 'poison').
type('venusaurmega', 'grass').
type('venusaurmega', 'poison').
type('venusaurgmax', 'grass').
type('venusaurgmax', 'poison').
type('charmander', 'fire').
type('charmeleon', 'fire').
type('charizard', 'fire').
type('charizard', 'flying').
type('charizardmegax', 'fire').
type('charizardmegax', 'dragon').
type('charizardmegay', 'fire').
type('charizardmegay', 'flying').
type('charizardgmax', 'fire').
type('charizardgmax', 'flying').
type('squirtle', 'water').
type('wartortle', 'water').
type('blastoise', 'water').
type('blastoisemega', 'water').
type('blastoisegmax', 'water').
type('caterpie', 'bug').
type('metapod', 'bug').
type('butterfree', 'bug').
type('butterfree', 'flying').
type('butterfreegmax', 'bug').
type('butterfreegmax', 'flying').
type('weedle', 'bug').
type('weedle', 'poison').
type('kakuna', 'bug').
type('kakuna', 'poison').
type('beedrill', 'bug').
type('beedrill', 'poison').
type('beedrillmega', 'bug').
type('beedrillmega', 'poison').
type('pidgey', 'normal').
type('pidgey', 'flying').
type('pidgeotto', 'normal').
type('pidgeotto', 'flying').
type('pidgeot', 'normal').
type('pidgeot', 'flying').
type('pidgeotmega', 'normal').
type('pidgeotmega', 'flying').
type('rattata', 'normal').
type('rattataalola', 'dark').
type('rattataalola', 'normal').
type('raticate', 'normal').
type('raticatealola', 'dark').
type('raticatealola', 'normal').
type('raticatealolatotem', 'dark').
type('raticatealolatotem', 'normal').
type('spearow', 'normal').
type('spearow', 'flying').
type('fearow', 'normal').
type('fearow', 'flying').
type('ekans', 'poison').
type('arbok', 'poison').
type('pikachu', 'electric').
type('pikachucosplay', 'electric').
type('pikachurockstar', 'electric').
type('pikachubelle', 'electric').
type('pikachupopstar', 'electric').
type('pikachuphd', 'electric').
type('pikachulibre', 'electric').
type('pikachuoriginal', 'electric').
type('pikachuhoenn', 'electric').
type('pikachusinnoh', 'electric').
type('pikachuunova', 'electric').
type('pikachukalos', 'electric').
type('pikachualola', 'electric').
type('pikachupartner', 'electric').
type('pikachustarter', 'electric').
type('pikachugmax', 'electric').
type('pikachuworld', 'electric').
type('raichu', 'electric').
type('raichualola', 'electric').
type('raichualola', 'psychic').
type('sandshrew', 'ground').
type('sandshrewalola', 'ice').
type('sandshrewalola', 'steel').
type('sandslash', 'ground').
type('sandslashalola', 'ice').
type('sandslashalola', 'steel').
type('nidoranf', 'poison').
type('nidorina', 'poison').
type('nidoqueen', 'poison').
type('nidoqueen', 'ground').
type('nidoranm', 'poison').
type('nidorino', 'poison').
type('nidoking', 'poison').
type('nidoking', 'ground').
type('clefairy', 'fairy').
type('clefable', 'fairy').
type('clefablemega', '???').
type('vulpix', 'fire').
type('vulpixalola', 'ice').
type('ninetales', 'fire').
type('ninetalesalola', 'ice').
type('ninetalesalola', 'fairy').
type('jigglypuff', 'normal').
type('jigglypuff', 'fairy').
type('wigglytuff', 'normal').
type('wigglytuff', 'fairy').
type('zubat', 'poison').
type('zubat', 'flying').
type('golbat', 'poison').
type('golbat', 'flying').
type('oddish', 'grass').
type('oddish', 'poison').
type('gloom', 'grass').
type('gloom', 'poison').
type('vileplume', 'grass').
type('vileplume', 'poison').
type('paras', 'bug').
type('paras', 'grass').
type('parasect', 'bug').
type('parasect', 'grass').
type('venonat', 'bug').
type('venonat', 'poison').
type('venomoth', 'bug').
type('venomoth', 'poison').
type('diglett', 'ground').
type('diglettalola', 'ground').
type('diglettalola', 'steel').
type('dugtrio', 'ground').
type('dugtrioalola', 'ground').
type('dugtrioalola', 'steel').
type('meowth', 'normal').
type('meowthalola', 'dark').
type('meowthgalar', 'steel').
type('meowthgmax', 'normal').
type('persian', 'normal').
type('persianalola', 'dark').
type('psyduck', 'water').
type('golduck', 'water').
type('mankey', 'fighting').
type('primeape', 'fighting').
type('growlithe', 'fire').
type('growlithehisui', 'fire').
type('growlithehisui', 'rock').
type('arcanine', 'fire').
type('arcaninehisui', 'fire').
type('arcaninehisui', 'rock').
type('poliwag', 'water').
type('poliwhirl', 'water').
type('poliwrath', 'water').
type('poliwrath', 'fighting').
type('abra', 'psychic').
type('kadabra', 'psychic').
type('alakazam', 'psychic').
type('alakazammega', 'psychic').
type('machop', 'fighting').
type('machoke', 'fighting').
type('machamp', 'fighting').
type('machampgmax', 'fighting').
type('bellsprout', 'grass').
type('bellsprout', 'poison').
type('weepinbell', 'grass').
type('weepinbell', 'poison').
type('victreebel', 'grass').
type('victreebel', 'poison').
type('victreebelmega', '???').
type('tentacool', 'water').
type('tentacool', 'poison').
type('tentacruel', 'water').
type('tentacruel', 'poison').
type('geodude', 'rock').
type('geodude', 'ground').
type('geodudealola', 'rock').
type('geodudealola', 'electric').
type('graveler', 'rock').
type('graveler', 'ground').
type('graveleralola', 'rock').
type('graveleralola', 'electric').
type('golem', 'rock').
type('golem', 'ground').
type('golemalola', 'rock').
type('golemalola', 'electric').
type('ponyta', 'fire').
type('ponytagalar', 'psychic').
type('rapidash', 'fire').
type('rapidashgalar', 'psychic').
type('rapidashgalar', 'fairy').
type('slowpoke', 'water').
type('slowpoke', 'psychic').
type('slowpokegalar', 'psychic').
type('slowbro', 'water').
type('slowbro', 'psychic').
type('slowbromega', 'water').
type('slowbromega', 'psychic').
type('slowbrogalar', 'poison').
type('slowbrogalar', 'psychic').
type('magnemite', 'electric').
type('magnemite', 'steel').
type('magneton', 'electric').
type('magneton', 'steel').
type('farfetchd', 'normal').
type('farfetchd', 'flying').
type('farfetchdgalar', 'fighting').
type('doduo', 'normal').
type('doduo', 'flying').
type('dodrio', 'normal').
type('dodrio', 'flying').
type('seel', 'water').
type('dewgong', 'water').
type('dewgong', 'ice').
type('grimer', 'poison').
type('grimeralola', 'poison').
type('grimeralola', 'dark').
type('muk', 'poison').
type('mukalola', 'poison').
type('mukalola', 'dark').
type('shellder', 'water').
type('cloyster', 'water').
type('cloyster', 'ice').
type('gastly', 'ghost').
type('gastly', 'poison').
type('haunter', 'ghost').
type('haunter', 'poison').
type('gengar', 'ghost').
type('gengar', 'poison').
type('gengarmega', 'ghost').
type('gengarmega', 'poison').
type('gengargmax', 'ghost').
type('gengargmax', 'poison').
type('onix', 'rock').
type('onix', 'ground').
type('drowzee', 'psychic').
type('hypno', 'psychic').
type('krabby', 'water').
type('kingler', 'water').
type('kinglergmax', 'water').
type('voltorb', 'electric').
type('voltorbhisui', 'electric').
type('voltorbhisui', 'grass').
type('electrode', 'electric').
type('electrodehisui', 'electric').
type('electrodehisui', 'grass').
type('exeggcute', 'grass').
type('exeggcute', 'psychic').
type('exeggutor', 'grass').
type('exeggutor', 'psychic').
type('exeggutoralola', 'grass').
type('exeggutoralola', 'dragon').
type('cubone', 'ground').
type('marowak', 'ground').
type('marowakalola', 'fire').
type('marowakalola', 'ghost').
type('marowakalolatotem', 'fire').
type('marowakalolatotem', 'ghost').
type('hitmonlee', 'fighting').
type('hitmonchan', 'fighting').
type('lickitung', 'normal').
type('koffing', 'poison').
type('weezing', 'poison').
type('weezinggalar', 'poison').
type('weezinggalar', 'fairy').
type('rhyhorn', 'ground').
type('rhyhorn', 'rock').
type('rhydon', 'ground').
type('rhydon', 'rock').
type('chansey', 'normal').
type('tangela', 'grass').
type('kangaskhan', 'normal').
type('kangaskhanmega', 'normal').
type('horsea', 'water').
type('seadra', 'water').
type('goldeen', 'water').
type('seaking', 'water').
type('staryu', 'water').
type('starmie', 'water').
type('starmie', 'psychic').
type('starmiemega', '???').
type('mrmime', 'psychic').
type('mrmime', 'fairy').
type('mrmimegalar', 'ice').
type('mrmimegalar', 'psychic').
type('scyther', 'bug').
type('scyther', 'flying').
type('jynx', 'ice').
type('jynx', 'psychic').
type('electabuzz', 'electric').
type('magmar', 'fire').
type('pinsir', 'bug').
type('pinsirmega', 'bug').
type('pinsirmega', 'flying').
type('tauros', 'normal').
type('taurospaldeacombat', 'fighting').
type('taurospaldeablaze', 'fighting').
type('taurospaldeablaze', 'fire').
type('taurospaldeaaqua', 'fighting').
type('taurospaldeaaqua', 'water').
type('magikarp', 'water').
type('gyarados', 'water').
type('gyarados', 'flying').
type('gyaradosmega', 'water').
type('gyaradosmega', 'dark').
type('lapras', 'water').
type('lapras', 'ice').
type('laprasgmax', 'water').
type('laprasgmax', 'ice').
type('ditto', 'normal').
type('eevee', 'normal').
type('eeveestarter', 'normal').
type('eeveegmax', 'normal').
type('vaporeon', 'water').
type('jolteon', 'electric').
type('flareon', 'fire').
type('porygon', 'normal').
type('omanyte', 'rock').
type('omanyte', 'water').
type('omastar', 'rock').
type('omastar', 'water').
type('kabuto', 'rock').
type('kabuto', 'water').
type('kabutops', 'rock').
type('kabutops', 'water').
type('aerodactyl', 'rock').
type('aerodactyl', 'flying').
type('aerodactylmega', 'rock').
type('aerodactylmega', 'flying').
type('snorlax', 'normal').
type('snorlaxgmax', 'normal').
type('articuno', 'ice').
type('articuno', 'flying').
type('articunogalar', 'psychic').
type('articunogalar', 'flying').
type('zapdos', 'electric').
type('zapdos', 'flying').
type('zapdosgalar', 'fighting').
type('zapdosgalar', 'flying').
type('moltres', 'fire').
type('moltres', 'flying').
type('moltresgalar', 'dark').
type('moltresgalar', 'flying').
type('dratini', 'dragon').
type('dragonair', 'dragon').
type('dragonite', 'dragon').
type('dragonite', 'flying').
type('dragonitemega', '???').
type('mewtwo', 'psychic').
type('mewtwomegax', 'psychic').
type('mewtwomegax', 'fighting').
type('mewtwomegay', 'psychic').
type('mew', 'psychic').
type('chikorita', 'grass').
type('bayleef', 'grass').
type('meganium', 'grass').
type('meganiummega', '???').
type('cyndaquil', 'fire').
type('quilava', 'fire').
type('typhlosion', 'fire').
type('typhlosionhisui', 'fire').
type('typhlosionhisui', 'ghost').
type('totodile', 'water').
type('croconaw', 'water').
type('feraligatr', 'water').
type('feraligatrmega', '???').
type('sentret', 'normal').
type('furret', 'normal').
type('hoothoot', 'normal').
type('hoothoot', 'flying').
type('noctowl', 'normal').
type('noctowl', 'flying').
type('ledyba', 'bug').
type('ledyba', 'flying').
type('ledian', 'bug').
type('ledian', 'flying').
type('spinarak', 'bug').
type('spinarak', 'poison').
type('ariados', 'bug').
type('ariados', 'poison').
type('crobat', 'poison').
type('crobat', 'flying').
type('chinchou', 'water').
type('chinchou', 'electric').
type('lanturn', 'water').
type('lanturn', 'electric').
type('pichu', 'electric').
type('pichuspikyeared', 'electric').
type('cleffa', 'fairy').
type('igglybuff', 'normal').
type('igglybuff', 'fairy').
type('togepi', 'fairy').
type('togetic', 'fairy').
type('togetic', 'flying').
type('natu', 'psychic').
type('natu', 'flying').
type('xatu', 'psychic').
type('xatu', 'flying').
type('mareep', 'electric').
type('flaaffy', 'electric').
type('ampharos', 'electric').
type('ampharosmega', 'electric').
type('ampharosmega', 'dragon').
type('bellossom', 'grass').
type('marill', 'water').
type('marill', 'fairy').
type('azumarill', 'water').
type('azumarill', 'fairy').
type('sudowoodo', 'rock').
type('politoed', 'water').
type('hoppip', 'grass').
type('hoppip', 'flying').
type('skiploom', 'grass').
type('skiploom', 'flying').
type('jumpluff', 'grass').
type('jumpluff', 'flying').
type('aipom', 'normal').
type('sunkern', 'grass').
type('sunflora', 'grass').
type('yanma', 'bug').
type('yanma', 'flying').
type('wooper', 'water').
type('wooper', 'ground').
type('wooperpaldea', 'poison').
type('wooperpaldea', 'ground').
type('quagsire', 'water').
type('quagsire', 'ground').
type('espeon', 'psychic').
type('umbreon', 'dark').
type('murkrow', 'dark').
type('murkrow', 'flying').
type('slowking', 'water').
type('slowking', 'psychic').
type('slowkinggalar', 'poison').
type('slowkinggalar', 'psychic').
type('misdreavus', 'ghost').
type('unown', 'psychic').
type('wobbuffet', 'psychic').
type('girafarig', 'normal').
type('girafarig', 'psychic').
type('pineco', 'bug').
type('forretress', 'bug').
type('forretress', 'steel').
type('dunsparce', 'normal').
type('gligar', 'ground').
type('gligar', 'flying').
type('steelix', 'steel').
type('steelix', 'ground').
type('steelixmega', 'steel').
type('steelixmega', 'ground').
type('snubbull', 'fairy').
type('granbull', 'fairy').
type('qwilfish', 'water').
type('qwilfish', 'poison').
type('qwilfishhisui', 'dark').
type('qwilfishhisui', 'poison').
type('scizor', 'bug').
type('scizor', 'steel').
type('scizormega', 'bug').
type('scizormega', 'steel').
type('shuckle', 'bug').
type('shuckle', 'rock').
type('heracross', 'bug').
type('heracross', 'fighting').
type('heracrossmega', 'bug').
type('heracrossmega', 'fighting').
type('sneasel', 'dark').
type('sneasel', 'ice').
type('sneaselhisui', 'fighting').
type('sneaselhisui', 'poison').
type('teddiursa', 'normal').
type('ursaring', 'normal').
type('slugma', 'fire').
type('magcargo', 'fire').
type('magcargo', 'rock').
type('swinub', 'ice').
type('swinub', 'ground').
type('piloswine', 'ice').
type('piloswine', 'ground').
type('corsola', 'water').
type('corsola', 'rock').
type('corsolagalar', 'ghost').
type('remoraid', 'water').
type('octillery', 'water').
type('delibird', 'ice').
type('delibird', 'flying').
type('mantine', 'water').
type('mantine', 'flying').
type('skarmory', 'steel').
type('skarmory', 'flying').
type('skarmorymega', '???').
type('houndour', 'dark').
type('houndour', 'fire').
type('houndoom', 'dark').
type('houndoom', 'fire').
type('houndoommega', 'dark').
type('houndoommega', 'fire').
type('kingdra', 'water').
type('kingdra', 'dragon').
type('phanpy', 'ground').
type('donphan', 'ground').
type('porygon2', 'normal').
type('stantler', 'normal').
type('smeargle', 'normal').
type('tyrogue', 'fighting').
type('hitmontop', 'fighting').
type('smoochum', 'ice').
type('smoochum', 'psychic').
type('elekid', 'electric').
type('magby', 'fire').
type('miltank', 'normal').
type('blissey', 'normal').
type('raikou', 'electric').
type('entei', 'fire').
type('suicune', 'water').
type('larvitar', 'rock').
type('larvitar', 'ground').
type('pupitar', 'rock').
type('pupitar', 'ground').
type('tyranitar', 'rock').
type('tyranitar', 'dark').
type('tyranitarmega', 'rock').
type('tyranitarmega', 'dark').
type('lugia', 'psychic').
type('lugia', 'flying').
type('hooh', 'fire').
type('hooh', 'flying').
type('celebi', 'psychic').
type('celebi', 'grass').
type('treecko', 'grass').
type('grovyle', 'grass').
type('sceptile', 'grass').
type('sceptilemega', 'grass').
type('sceptilemega', 'dragon').
type('torchic', 'fire').
type('combusken', 'fire').
type('combusken', 'fighting').
type('blaziken', 'fire').
type('blaziken', 'fighting').
type('blazikenmega', 'fire').
type('blazikenmega', 'fighting').
type('mudkip', 'water').
type('marshtomp', 'water').
type('marshtomp', 'ground').
type('swampert', 'water').
type('swampert', 'ground').
type('swampertmega', 'water').
type('swampertmega', 'ground').
type('poochyena', 'dark').
type('mightyena', 'dark').
type('zigzagoon', 'normal').
type('zigzagoongalar', 'dark').
type('zigzagoongalar', 'normal').
type('linoone', 'normal').
type('linoonegalar', 'dark').
type('linoonegalar', 'normal').
type('wurmple', 'bug').
type('silcoon', 'bug').
type('beautifly', 'bug').
type('beautifly', 'flying').
type('cascoon', 'bug').
type('dustox', 'bug').
type('dustox', 'poison').
type('lotad', 'water').
type('lotad', 'grass').
type('lombre', 'water').
type('lombre', 'grass').
type('ludicolo', 'water').
type('ludicolo', 'grass').
type('seedot', 'grass').
type('nuzleaf', 'grass').
type('nuzleaf', 'dark').
type('shiftry', 'grass').
type('shiftry', 'dark').
type('taillow', 'normal').
type('taillow', 'flying').
type('swellow', 'normal').
type('swellow', 'flying').
type('wingull', 'water').
type('wingull', 'flying').
type('pelipper', 'water').
type('pelipper', 'flying').
type('ralts', 'psychic').
type('ralts', 'fairy').
type('kirlia', 'psychic').
type('kirlia', 'fairy').
type('gardevoir', 'psychic').
type('gardevoir', 'fairy').
type('gardevoirmega', 'psychic').
type('gardevoirmega', 'fairy').
type('surskit', 'bug').
type('surskit', 'water').
type('masquerain', 'bug').
type('masquerain', 'flying').
type('shroomish', 'grass').
type('breloom', 'grass').
type('breloom', 'fighting').
type('slakoth', 'normal').
type('vigoroth', 'normal').
type('slaking', 'normal').
type('nincada', 'bug').
type('nincada', 'ground').
type('ninjask', 'bug').
type('ninjask', 'flying').
type('shedinja', 'bug').
type('shedinja', 'ghost').
type('whismur', 'normal').
type('loudred', 'normal').
type('exploud', 'normal').
type('makuhita', 'fighting').
type('hariyama', 'fighting').
type('azurill', 'normal').
type('azurill', 'fairy').
type('nosepass', 'rock').
type('skitty', 'normal').
type('delcatty', 'normal').
type('sableye', 'dark').
type('sableye', 'ghost').
type('sableyemega', 'dark').
type('sableyemega', 'ghost').
type('mawile', 'steel').
type('mawile', 'fairy').
type('mawilemega', 'steel').
type('mawilemega', 'fairy').
type('aron', 'steel').
type('aron', 'rock').
type('lairon', 'steel').
type('lairon', 'rock').
type('aggron', 'steel').
type('aggron', 'rock').
type('aggronmega', 'steel').
type('meditite', 'fighting').
type('meditite', 'psychic').
type('medicham', 'fighting').
type('medicham', 'psychic').
type('medichammega', 'fighting').
type('medichammega', 'psychic').
type('electrike', 'electric').
type('manectric', 'electric').
type('manectricmega', 'electric').
type('plusle', 'electric').
type('minun', 'electric').
type('volbeat', 'bug').
type('illumise', 'bug').
type('roselia', 'grass').
type('roselia', 'poison').
type('gulpin', 'poison').
type('swalot', 'poison').
type('carvanha', 'water').
type('carvanha', 'dark').
type('sharpedo', 'water').
type('sharpedo', 'dark').
type('sharpedomega', 'water').
type('sharpedomega', 'dark').
type('wailmer', 'water').
type('wailord', 'water').
type('numel', 'fire').
type('numel', 'ground').
type('camerupt', 'fire').
type('camerupt', 'ground').
type('cameruptmega', 'fire').
type('cameruptmega', 'ground').
type('torkoal', 'fire').
type('spoink', 'psychic').
type('grumpig', 'psychic').
type('spinda', 'normal').
type('trapinch', 'ground').
type('vibrava', 'ground').
type('vibrava', 'dragon').
type('flygon', 'ground').
type('flygon', 'dragon').
type('cacnea', 'grass').
type('cacturne', 'grass').
type('cacturne', 'dark').
type('swablu', 'normal').
type('swablu', 'flying').
type('altaria', 'dragon').
type('altaria', 'flying').
type('altariamega', 'dragon').
type('altariamega', 'fairy').
type('zangoose', 'normal').
type('seviper', 'poison').
type('lunatone', 'rock').
type('lunatone', 'psychic').
type('solrock', 'rock').
type('solrock', 'psychic').
type('barboach', 'water').
type('barboach', 'ground').
type('whiscash', 'water').
type('whiscash', 'ground').
type('corphish', 'water').
type('crawdaunt', 'water').
type('crawdaunt', 'dark').
type('baltoy', 'ground').
type('baltoy', 'psychic').
type('claydol', 'ground').
type('claydol', 'psychic').
type('lileep', 'rock').
type('lileep', 'grass').
type('cradily', 'rock').
type('cradily', 'grass').
type('anorith', 'rock').
type('anorith', 'bug').
type('armaldo', 'rock').
type('armaldo', 'bug').
type('feebas', 'water').
type('milotic', 'water').
type('castform', 'normal').
type('castformsunny', 'fire').
type('castformrainy', 'water').
type('castformsnowy', 'ice').
type('kecleon', 'normal').
type('shuppet', 'ghost').
type('banette', 'ghost').
type('banettemega', 'ghost').
type('duskull', 'ghost').
type('dusclops', 'ghost').
type('tropius', 'grass').
type('tropius', 'flying').
type('chimecho', 'psychic').
type('absol', 'dark').
type('absolmega', 'dark').
type('wynaut', 'psychic').
type('snorunt', 'ice').
type('glalie', 'ice').
type('glaliemega', 'ice').
type('spheal', 'ice').
type('spheal', 'water').
type('sealeo', 'ice').
type('sealeo', 'water').
type('walrein', 'ice').
type('walrein', 'water').
type('clamperl', 'water').
type('huntail', 'water').
type('gorebyss', 'water').
type('relicanth', 'water').
type('relicanth', 'rock').
type('luvdisc', 'water').
type('bagon', 'dragon').
type('shelgon', 'dragon').
type('salamence', 'dragon').
type('salamence', 'flying').
type('salamencemega', 'dragon').
type('salamencemega', 'flying').
type('beldum', 'steel').
type('beldum', 'psychic').
type('metang', 'steel').
type('metang', 'psychic').
type('metagross', 'steel').
type('metagross', 'psychic').
type('metagrossmega', 'steel').
type('metagrossmega', 'psychic').
type('regirock', 'rock').
type('regice', 'ice').
type('registeel', 'steel').
type('latias', 'dragon').
type('latias', 'psychic').
type('latiasmega', 'dragon').
type('latiasmega', 'psychic').
type('latios', 'dragon').
type('latios', 'psychic').
type('latiosmega', 'dragon').
type('latiosmega', 'psychic').
type('kyogre', 'water').
type('kyogreprimal', 'water').
type('groudon', 'ground').
type('groudonprimal', 'ground').
type('groudonprimal', 'fire').
type('rayquaza', 'dragon').
type('rayquaza', 'flying').
type('rayquazamega', 'dragon').
type('rayquazamega', 'flying').
type('jirachi', 'steel').
type('jirachi', 'psychic').
type('deoxys', 'psychic').
type('deoxysattack', 'psychic').
type('deoxysdefense', 'psychic').
type('deoxysspeed', 'psychic').
type('turtwig', 'grass').
type('grotle', 'grass').
type('torterra', 'grass').
type('torterra', 'ground').
type('chimchar', 'fire').
type('monferno', 'fire').
type('monferno', 'fighting').
type('infernape', 'fire').
type('infernape', 'fighting').
type('piplup', 'water').
type('prinplup', 'water').
type('empoleon', 'water').
type('empoleon', 'steel').
type('starly', 'normal').
type('starly', 'flying').
type('staravia', 'normal').
type('staravia', 'flying').
type('staraptor', 'normal').
type('staraptor', 'flying').
type('bidoof', 'normal').
type('bibarel', 'normal').
type('bibarel', 'water').
type('kricketot', 'bug').
type('kricketune', 'bug').
type('shinx', 'electric').
type('luxio', 'electric').
type('luxray', 'electric').
type('budew', 'grass').
type('budew', 'poison').
type('roserade', 'grass').
type('roserade', 'poison').
type('cranidos', 'rock').
type('rampardos', 'rock').
type('shieldon', 'rock').
type('shieldon', 'steel').
type('bastiodon', 'rock').
type('bastiodon', 'steel').
type('burmy', 'bug').
type('burmysandy', 'bug').
type('burmytrash', 'bug').
type('wormadam', 'bug').
type('wormadam', 'grass').
type('wormadamsandy', 'bug').
type('wormadamsandy', 'ground').
type('wormadamtrash', 'bug').
type('wormadamtrash', 'steel').
type('mothim', 'bug').
type('mothim', 'flying').
type('combee', 'bug').
type('combee', 'flying').
type('vespiquen', 'bug').
type('vespiquen', 'flying').
type('pachirisu', 'electric').
type('buizel', 'water').
type('floatzel', 'water').
type('cherubi', 'grass').
type('cherrim', 'grass').
type('cherrimsunshine', 'grass').
type('shellos', 'water').
type('shelloseast', 'water').
type('gastrodon', 'water').
type('gastrodon', 'ground').
type('gastrodoneast', 'water').
type('gastrodoneast', 'ground').
type('ambipom', 'normal').
type('drifloon', 'ghost').
type('drifloon', 'flying').
type('drifblim', 'ghost').
type('drifblim', 'flying').
type('buneary', 'normal').
type('lopunny', 'normal').
type('lopunnymega', 'normal').
type('lopunnymega', 'fighting').
type('mismagius', 'ghost').
type('honchkrow', 'dark').
type('honchkrow', 'flying').
type('glameow', 'normal').
type('purugly', 'normal').
type('chingling', 'psychic').
type('stunky', 'poison').
type('stunky', 'dark').
type('skuntank', 'poison').
type('skuntank', 'dark').
type('bronzor', 'steel').
type('bronzor', 'psychic').
type('bronzong', 'steel').
type('bronzong', 'psychic').
type('bonsly', 'rock').
type('mimejr', 'psychic').
type('mimejr', 'fairy').
type('happiny', 'normal').
type('chatot', 'normal').
type('chatot', 'flying').
type('spiritomb', 'ghost').
type('spiritomb', 'dark').
type('gible', 'dragon').
type('gible', 'ground').
type('gabite', 'dragon').
type('gabite', 'ground').
type('garchomp', 'dragon').
type('garchomp', 'ground').
type('garchompmega', 'dragon').
type('garchompmega', 'ground').
type('munchlax', 'normal').
type('riolu', 'fighting').
type('lucario', 'fighting').
type('lucario', 'steel').
type('lucariomega', 'fighting').
type('lucariomega', 'steel').
type('hippopotas', 'ground').
type('hippowdon', 'ground').
type('skorupi', 'poison').
type('skorupi', 'bug').
type('drapion', 'poison').
type('drapion', 'dark').
type('croagunk', 'poison').
type('croagunk', 'fighting').
type('toxicroak', 'poison').
type('toxicroak', 'fighting').
type('carnivine', 'grass').
type('finneon', 'water').
type('lumineon', 'water').
type('mantyke', 'water').
type('mantyke', 'flying').
type('snover', 'grass').
type('snover', 'ice').
type('abomasnow', 'grass').
type('abomasnow', 'ice').
type('abomasnowmega', 'grass').
type('abomasnowmega', 'ice').
type('weavile', 'dark').
type('weavile', 'ice').
type('magnezone', 'electric').
type('magnezone', 'steel').
type('lickilicky', 'normal').
type('rhyperior', 'ground').
type('rhyperior', 'rock').
type('tangrowth', 'grass').
type('electivire', 'electric').
type('magmortar', 'fire').
type('togekiss', 'fairy').
type('togekiss', 'flying').
type('yanmega', 'bug').
type('yanmega', 'flying').
type('leafeon', 'grass').
type('glaceon', 'ice').
type('gliscor', 'ground').
type('gliscor', 'flying').
type('mamoswine', 'ice').
type('mamoswine', 'ground').
type('porygonz', 'normal').
type('gallade', 'psychic').
type('gallade', 'fighting').
type('gallademega', 'psychic').
type('gallademega', 'fighting').
type('probopass', 'rock').
type('probopass', 'steel').
type('dusknoir', 'ghost').
type('froslass', 'ice').
type('froslass', 'ghost').
type('froslassmega', '???').
type('rotom', 'electric').
type('rotom', 'ghost').
type('rotomheat', 'electric').
type('rotomheat', 'fire').
type('rotomwash', 'electric').
type('rotomwash', 'water').
type('rotomfrost', 'electric').
type('rotomfrost', 'ice').
type('rotomfan', 'electric').
type('rotomfan', 'flying').
type('rotommow', 'electric').
type('rotommow', 'grass').
type('uxie', 'psychic').
type('mesprit', 'psychic').
type('azelf', 'psychic').
type('dialga', 'steel').
type('dialga', 'dragon').
type('dialgaorigin', 'steel').
type('dialgaorigin', 'dragon').
type('palkia', 'water').
type('palkia', 'dragon').
type('palkiaorigin', 'water').
type('palkiaorigin', 'dragon').
type('heatran', 'fire').
type('heatran', 'steel').
type('regigigas', 'normal').
type('giratina', 'ghost').
type('giratina', 'dragon').
type('giratinaorigin', 'ghost').
type('giratinaorigin', 'dragon').
type('cresselia', 'psychic').
type('phione', 'water').
type('manaphy', 'water').
type('darkrai', 'dark').
type('shaymin', 'grass').
type('shayminsky', 'grass').
type('shayminsky', 'flying').
type('arceus', 'normal').
type('arceusbug', 'bug').
type('arceusdark', 'dark').
type('arceusdragon', 'dragon').
type('arceuselectric', 'electric').
type('arceusfairy', 'fairy').
type('arceusfighting', 'fighting').
type('arceusfire', 'fire').
type('arceusflying', 'flying').
type('arceusghost', 'ghost').
type('arceusgrass', 'grass').
type('arceusground', 'ground').
type('arceusice', 'ice').
type('arceuspoison', 'poison').
type('arceuspsychic', 'psychic').
type('arceusrock', 'rock').
type('arceussteel', 'steel').
type('arceuswater', 'water').
type('victini', 'psychic').
type('victini', 'fire').
type('snivy', 'grass').
type('servine', 'grass').
type('serperior', 'grass').
type('tepig', 'fire').
type('pignite', 'fire').
type('pignite', 'fighting').
type('emboar', 'fire').
type('emboar', 'fighting').
type('emboarmega', '???').
type('oshawott', 'water').
type('dewott', 'water').
type('samurott', 'water').
type('samurotthisui', 'water').
type('samurotthisui', 'dark').
type('patrat', 'normal').
type('watchog', 'normal').
type('lillipup', 'normal').
type('herdier', 'normal').
type('stoutland', 'normal').
type('purrloin', 'dark').
type('liepard', 'dark').
type('pansage', 'grass').
type('simisage', 'grass').
type('pansear', 'fire').
type('simisear', 'fire').
type('panpour', 'water').
type('simipour', 'water').
type('munna', 'psychic').
type('musharna', 'psychic').
type('pidove', 'normal').
type('pidove', 'flying').
type('tranquill', 'normal').
type('tranquill', 'flying').
type('unfezant', 'normal').
type('unfezant', 'flying').
type('blitzle', 'electric').
type('zebstrika', 'electric').
type('roggenrola', 'rock').
type('boldore', 'rock').
type('gigalith', 'rock').
type('woobat', 'psychic').
type('woobat', 'flying').
type('swoobat', 'psychic').
type('swoobat', 'flying').
type('drilbur', 'ground').
type('excadrill', 'ground').
type('excadrill', 'steel').
type('excadrillmega', '???').
type('audino', 'normal').
type('audinomega', 'normal').
type('audinomega', 'fairy').
type('timburr', 'fighting').
type('gurdurr', 'fighting').
type('conkeldurr', 'fighting').
type('tympole', 'water').
type('palpitoad', 'water').
type('palpitoad', 'ground').
type('seismitoad', 'water').
type('seismitoad', 'ground').
type('throh', 'fighting').
type('sawk', 'fighting').
type('sewaddle', 'bug').
type('sewaddle', 'grass').
type('swadloon', 'bug').
type('swadloon', 'grass').
type('leavanny', 'bug').
type('leavanny', 'grass').
type('venipede', 'bug').
type('venipede', 'poison').
type('whirlipede', 'bug').
type('whirlipede', 'poison').
type('scolipede', 'bug').
type('scolipede', 'poison').
type('scolipedemega', '???').
type('cottonee', 'grass').
type('cottonee', 'fairy').
type('whimsicott', 'grass').
type('whimsicott', 'fairy').
type('petilil', 'grass').
type('lilligant', 'grass').
type('lilliganthisui', 'grass').
type('lilliganthisui', 'fighting').
type('basculin', 'water').
type('basculinbluestriped', 'water').
type('basculinwhitestriped', 'water').
type('sandile', 'ground').
type('sandile', 'dark').
type('krokorok', 'ground').
type('krokorok', 'dark').
type('krookodile', 'ground').
type('krookodile', 'dark').
type('darumaka', 'fire').
type('darumakagalar', 'ice').
type('darmanitan', 'fire').
type('darmanitanzen', 'fire').
type('darmanitanzen', 'psychic').
type('darmanitangalar', 'ice').
type('darmanitangalarzen', 'ice').
type('darmanitangalarzen', 'fire').
type('maractus', 'grass').
type('dwebble', 'bug').
type('dwebble', 'rock').
type('crustle', 'bug').
type('crustle', 'rock').
type('scraggy', 'dark').
type('scraggy', 'fighting').
type('scrafty', 'dark').
type('scrafty', 'fighting').
type('scraftymega', '???').
type('sigilyph', 'psychic').
type('sigilyph', 'flying').
type('yamask', 'ghost').
type('yamaskgalar', 'ground').
type('yamaskgalar', 'ghost').
type('cofagrigus', 'ghost').
type('tirtouga', 'water').
type('tirtouga', 'rock').
type('carracosta', 'water').
type('carracosta', 'rock').
type('archen', 'rock').
type('archen', 'flying').
type('archeops', 'rock').
type('archeops', 'flying').
type('trubbish', 'poison').
type('garbodor', 'poison').
type('garbodorgmax', 'poison').
type('zorua', 'dark').
type('zoruahisui', 'normal').
type('zoruahisui', 'ghost').
type('zoroark', 'dark').
type('zoroarkhisui', 'normal').
type('zoroarkhisui', 'ghost').
type('minccino', 'normal').
type('cinccino', 'normal').
type('gothita', 'psychic').
type('gothorita', 'psychic').
type('gothitelle', 'psychic').
type('solosis', 'psychic').
type('duosion', 'psychic').
type('reuniclus', 'psychic').
type('ducklett', 'water').
type('ducklett', 'flying').
type('swanna', 'water').
type('swanna', 'flying').
type('vanillite', 'ice').
type('vanillish', 'ice').
type('vanilluxe', 'ice').
type('deerling', 'normal').
type('deerling', 'grass').
type('deerlingsummer', 'normal').
type('deerlingsummer', 'grass').
type('deerlingautumn', 'normal').
type('deerlingautumn', 'grass').
type('deerlingwinter', 'normal').
type('deerlingwinter', 'grass').
type('sawsbuck', 'normal').
type('sawsbuck', 'grass').
type('emolga', 'electric').
type('emolga', 'flying').
type('karrablast', 'bug').
type('escavalier', 'bug').
type('escavalier', 'steel').
type('foongus', 'grass').
type('foongus', 'poison').
type('amoonguss', 'grass').
type('amoonguss', 'poison').
type('frillish', 'water').
type('frillish', 'ghost').
type('jellicent', 'water').
type('jellicent', 'ghost').
type('alomomola', 'water').
type('joltik', 'bug').
type('joltik', 'electric').
type('galvantula', 'bug').
type('galvantula', 'electric').
type('ferroseed', 'grass').
type('ferroseed', 'steel').
type('ferrothorn', 'grass').
type('ferrothorn', 'steel').
type('klink', 'steel').
type('klang', 'steel').
type('klinklang', 'steel').
type('tynamo', 'electric').
type('eelektrik', 'electric').
type('eelektross', 'electric').
type('eelektrossmega', '???').
type('elgyem', 'psychic').
type('beheeyem', 'psychic').
type('litwick', 'ghost').
type('litwick', 'fire').
type('lampent', 'ghost').
type('lampent', 'fire').
type('chandelure', 'ghost').
type('chandelure', 'fire').
type('chandeluremega', '???').
type('axew', 'dragon').
type('fraxure', 'dragon').
type('haxorus', 'dragon').
type('cubchoo', 'ice').
type('beartic', 'ice').
type('cryogonal', 'ice').
type('shelmet', 'bug').
type('accelgor', 'bug').
type('stunfisk', 'ground').
type('stunfisk', 'electric').
type('stunfiskgalar', 'ground').
type('stunfiskgalar', 'steel').
type('mienfoo', 'fighting').
type('mienshao', 'fighting').
type('druddigon', 'dragon').
type('golett', 'ground').
type('golett', 'ghost').
type('golurk', 'ground').
type('golurk', 'ghost').
type('pawniard', 'dark').
type('pawniard', 'steel').
type('bisharp', 'dark').
type('bisharp', 'steel').
type('bouffalant', 'normal').
type('rufflet', 'normal').
type('rufflet', 'flying').
type('braviary', 'normal').
type('braviary', 'flying').
type('braviaryhisui', 'psychic').
type('braviaryhisui', 'flying').
type('vullaby', 'dark').
type('vullaby', 'flying').
type('mandibuzz', 'dark').
type('mandibuzz', 'flying').
type('heatmor', 'fire').
type('durant', 'bug').
type('durant', 'steel').
type('deino', 'dark').
type('deino', 'dragon').
type('zweilous', 'dark').
type('zweilous', 'dragon').
type('hydreigon', 'dark').
type('hydreigon', 'dragon').
type('larvesta', 'bug').
type('larvesta', 'fire').
type('volcarona', 'bug').
type('volcarona', 'fire').
type('cobalion', 'steel').
type('cobalion', 'fighting').
type('terrakion', 'rock').
type('terrakion', 'fighting').
type('virizion', 'grass').
type('virizion', 'fighting').
type('tornadus', 'flying').
type('tornadustherian', 'flying').
type('thundurus', 'electric').
type('thundurus', 'flying').
type('thundurustherian', 'electric').
type('thundurustherian', 'flying').
type('reshiram', 'dragon').
type('reshiram', 'fire').
type('zekrom', 'dragon').
type('zekrom', 'electric').
type('landorus', 'ground').
type('landorus', 'flying').
type('landorustherian', 'ground').
type('landorustherian', 'flying').
type('kyurem', 'dragon').
type('kyurem', 'ice').
type('kyuremblack', 'dragon').
type('kyuremblack', 'ice').
type('kyuremwhite', 'dragon').
type('kyuremwhite', 'ice').
type('keldeo', 'water').
type('keldeo', 'fighting').
type('keldeoresolute', 'water').
type('keldeoresolute', 'fighting').
type('meloetta', 'normal').
type('meloetta', 'psychic').
type('meloettapirouette', 'normal').
type('meloettapirouette', 'fighting').
type('genesect', 'bug').
type('genesect', 'steel').
type('genesectdouse', 'bug').
type('genesectdouse', 'steel').
type('genesectshock', 'bug').
type('genesectshock', 'steel').
type('genesectburn', 'bug').
type('genesectburn', 'steel').
type('genesectchill', 'bug').
type('genesectchill', 'steel').
type('chespin', 'grass').
type('quilladin', 'grass').
type('chesnaught', 'grass').
type('chesnaught', 'fighting').
type('chesnaughtmega', '???').
type('fennekin', 'fire').
type('braixen', 'fire').
type('delphox', 'fire').
type('delphox', 'psychic').
type('delphoxmega', '???').
type('froakie', 'water').
type('frogadier', 'water').
type('greninja', 'water').
type('greninja', 'dark').
type('greninjabond', 'water').
type('greninjabond', 'dark').
type('greninjaash', 'water').
type('greninjaash', 'dark').
type('greninjamega', '???').
type('bunnelby', 'normal').
type('diggersby', 'normal').
type('diggersby', 'ground').
type('fletchling', 'normal').
type('fletchling', 'flying').
type('fletchinder', 'fire').
type('fletchinder', 'flying').
type('talonflame', 'fire').
type('talonflame', 'flying').
type('scatterbug', 'bug').
type('spewpa', 'bug').
type('vivillon', 'bug').
type('vivillon', 'flying').
type('vivillonicysnow', 'bug').
type('vivillonicysnow', 'flying').
type('vivillonpolar', 'bug').
type('vivillonpolar', 'flying').
type('vivillontundra', 'bug').
type('vivillontundra', 'flying').
type('vivilloncontinental', 'bug').
type('vivilloncontinental', 'flying').
type('vivillongarden', 'bug').
type('vivillongarden', 'flying').
type('vivillonelegant', 'bug').
type('vivillonelegant', 'flying').
type('vivillonmodern', 'bug').
type('vivillonmodern', 'flying').
type('vivillonmarine', 'bug').
type('vivillonmarine', 'flying').
type('vivillonarchipelago', 'bug').
type('vivillonarchipelago', 'flying').
type('vivillonhighplains', 'bug').
type('vivillonhighplains', 'flying').
type('vivillonsandstorm', 'bug').
type('vivillonsandstorm', 'flying').
type('vivillonriver', 'bug').
type('vivillonriver', 'flying').
type('vivillonmonsoon', 'bug').
type('vivillonmonsoon', 'flying').
type('vivillonsavanna', 'bug').
type('vivillonsavanna', 'flying').
type('vivillonsun', 'bug').
type('vivillonsun', 'flying').
type('vivillonocean', 'bug').
type('vivillonocean', 'flying').
type('vivillonjungle', 'bug').
type('vivillonjungle', 'flying').
type('vivillonfancy', 'bug').
type('vivillonfancy', 'flying').
type('vivillonpokeball', 'bug').
type('vivillonpokeball', 'flying').
type('litleo', 'fire').
type('litleo', 'normal').
type('pyroar', 'fire').
type('pyroar', 'normal').
type('pyroarmega', '???').
type('flabebe', 'fairy').
type('floette', 'fairy').
type('floetteeternal', 'fairy').
type('floettemega', '???').
type('florges', 'fairy').
type('skiddo', 'grass').
type('gogoat', 'grass').
type('pancham', 'fighting').
type('pangoro', 'fighting').
type('pangoro', 'dark').
type('furfrou', 'normal').
type('espurr', 'psychic').
type('meowstic', 'psychic').
type('meowsticf', 'psychic').
type('honedge', 'steel').
type('honedge', 'ghost').
type('doublade', 'steel').
type('doublade', 'ghost').
type('aegislash', 'steel').
type('aegislash', 'ghost').
type('aegislashblade', 'steel').
type('aegislashblade', 'ghost').
type('spritzee', 'fairy').
type('aromatisse', 'fairy').
type('swirlix', 'fairy').
type('slurpuff', 'fairy').
type('inkay', 'dark').
type('inkay', 'psychic').
type('malamar', 'dark').
type('malamar', 'psychic').
type('malamarmega', '???').
type('binacle', 'rock').
type('binacle', 'water').
type('barbaracle', 'rock').
type('barbaracle', 'water').
type('barbaraclemega', '???').
type('skrelp', 'poison').
type('skrelp', 'water').
type('dragalge', 'poison').
type('dragalge', 'dragon').
type('dragalgemega', '???').
type('clauncher', 'water').
type('clawitzer', 'water').
type('helioptile', 'electric').
type('helioptile', 'normal').
type('heliolisk', 'electric').
type('heliolisk', 'normal').
type('tyrunt', 'rock').
type('tyrunt', 'dragon').
type('tyrantrum', 'rock').
type('tyrantrum', 'dragon').
type('amaura', 'rock').
type('amaura', 'ice').
type('aurorus', 'rock').
type('aurorus', 'ice').
type('sylveon', 'fairy').
type('hawlucha', 'fighting').
type('hawlucha', 'flying').
type('hawluchamega', '???').
type('dedenne', 'electric').
type('dedenne', 'fairy').
type('carbink', 'rock').
type('carbink', 'fairy').
type('goomy', 'dragon').
type('sliggoo', 'dragon').
type('sliggoohisui', 'steel').
type('sliggoohisui', 'dragon').
type('goodra', 'dragon').
type('goodrahisui', 'steel').
type('goodrahisui', 'dragon').
type('klefki', 'steel').
type('klefki', 'fairy').
type('phantump', 'ghost').
type('phantump', 'grass').
type('trevenant', 'ghost').
type('trevenant', 'grass').
type('pumpkaboo', 'ghost').
type('pumpkaboo', 'grass').
type('pumpkaboosmall', 'ghost').
type('pumpkaboosmall', 'grass').
type('pumpkaboolarge', 'ghost').
type('pumpkaboolarge', 'grass').
type('pumpkaboosuper', 'ghost').
type('pumpkaboosuper', 'grass').
type('gourgeist', 'ghost').
type('gourgeist', 'grass').
type('gourgeistsmall', 'ghost').
type('gourgeistsmall', 'grass').
type('gourgeistlarge', 'ghost').
type('gourgeistlarge', 'grass').
type('gourgeistsuper', 'ghost').
type('gourgeistsuper', 'grass').
type('bergmite', 'ice').
type('avalugg', 'ice').
type('avalugghisui', 'ice').
type('avalugghisui', 'rock').
type('noibat', 'flying').
type('noibat', 'dragon').
type('noivern', 'flying').
type('noivern', 'dragon').
type('xerneas', 'fairy').
type('xerneasneutral', 'fairy').
type('yveltal', 'dark').
type('yveltal', 'flying').
type('zygarde', 'dragon').
type('zygarde', 'ground').
type('zygarde10', 'dragon').
type('zygarde10', 'ground').
type('zygardecomplete', 'dragon').
type('zygardecomplete', 'ground').
type('zygardemega', '???').
type('diancie', 'rock').
type('diancie', 'fairy').
type('dianciemega', 'rock').
type('dianciemega', 'fairy').
type('hoopa', 'psychic').
type('hoopa', 'ghost').
type('hoopaunbound', 'psychic').
type('hoopaunbound', 'dark').
type('volcanion', 'fire').
type('volcanion', 'water').
type('rowlet', 'grass').
type('rowlet', 'flying').
type('dartrix', 'grass').
type('dartrix', 'flying').
type('decidueye', 'grass').
type('decidueye', 'ghost').
type('decidueyehisui', 'grass').
type('decidueyehisui', 'fighting').
type('litten', 'fire').
type('torracat', 'fire').
type('incineroar', 'fire').
type('incineroar', 'dark').
type('popplio', 'water').
type('brionne', 'water').
type('primarina', 'water').
type('primarina', 'fairy').
type('pikipek', 'normal').
type('pikipek', 'flying').
type('trumbeak', 'normal').
type('trumbeak', 'flying').
type('toucannon', 'normal').
type('toucannon', 'flying').
type('yungoos', 'normal').
type('gumshoos', 'normal').
type('gumshoostotem', 'normal').
type('grubbin', 'bug').
type('charjabug', 'bug').
type('charjabug', 'electric').
type('vikavolt', 'bug').
type('vikavolt', 'electric').
type('vikavolttotem', 'bug').
type('vikavolttotem', 'electric').
type('crabrawler', 'fighting').
type('crabominable', 'fighting').
type('crabominable', 'ice').
type('oricorio', 'fire').
type('oricorio', 'flying').
type('oricoriopompom', 'electric').
type('oricoriopompom', 'flying').
type('oricoriopau', 'psychic').
type('oricoriopau', 'flying').
type('oricoriosensu', 'ghost').
type('oricoriosensu', 'flying').
type('cutiefly', 'bug').
type('cutiefly', 'fairy').
type('ribombee', 'bug').
type('ribombee', 'fairy').
type('ribombeetotem', 'bug').
type('ribombeetotem', 'fairy').
type('rockruff', 'rock').
type('rockruffdusk', 'rock').
type('lycanroc', 'rock').
type('lycanrocmidnight', 'rock').
type('lycanrocdusk', 'rock').
type('wishiwashi', 'water').
type('wishiwashischool', 'water').
type('mareanie', 'poison').
type('mareanie', 'water').
type('toxapex', 'poison').
type('toxapex', 'water').
type('mudbray', 'ground').
type('mudsdale', 'ground').
type('dewpider', 'water').
type('dewpider', 'bug').
type('araquanid', 'water').
type('araquanid', 'bug').
type('araquanidtotem', 'water').
type('araquanidtotem', 'bug').
type('fomantis', 'grass').
type('lurantis', 'grass').
type('lurantistotem', 'grass').
type('morelull', 'grass').
type('morelull', 'fairy').
type('shiinotic', 'grass').
type('shiinotic', 'fairy').
type('salandit', 'poison').
type('salandit', 'fire').
type('salazzle', 'poison').
type('salazzle', 'fire').
type('salazzletotem', 'poison').
type('salazzletotem', 'fire').
type('stufful', 'normal').
type('stufful', 'fighting').
type('bewear', 'normal').
type('bewear', 'fighting').
type('bounsweet', 'grass').
type('steenee', 'grass').
type('tsareena', 'grass').
type('comfey', 'fairy').
type('oranguru', 'normal').
type('oranguru', 'psychic').
type('passimian', 'fighting').
type('wimpod', 'bug').
type('wimpod', 'water').
type('golisopod', 'bug').
type('golisopod', 'water').
type('sandygast', 'ghost').
type('sandygast', 'ground').
type('palossand', 'ghost').
type('palossand', 'ground').
type('pyukumuku', 'water').
type('typenull', 'normal').
type('silvally', 'normal').
type('silvallybug', 'bug').
type('silvallydark', 'dark').
type('silvallydragon', 'dragon').
type('silvallyelectric', 'electric').
type('silvallyfairy', 'fairy').
type('silvallyfighting', 'fighting').
type('silvallyfire', 'fire').
type('silvallyflying', 'flying').
type('silvallyghost', 'ghost').
type('silvallygrass', 'grass').
type('silvallyground', 'ground').
type('silvallyice', 'ice').
type('silvallypoison', 'poison').
type('silvallypsychic', 'psychic').
type('silvallyrock', 'rock').
type('silvallysteel', 'steel').
type('silvallywater', 'water').
type('minior', 'rock').
type('minior', 'flying').
type('miniororange', 'rock').
type('miniororange', 'flying').
type('minioryellow', 'rock').
type('minioryellow', 'flying').
type('miniorgreen', 'rock').
type('miniorgreen', 'flying').
type('miniorblue', 'rock').
type('miniorblue', 'flying').
type('miniorindigo', 'rock').
type('miniorindigo', 'flying').
type('miniorviolet', 'rock').
type('miniorviolet', 'flying').
type('miniormeteor', 'rock').
type('miniormeteor', 'flying').
type('komala', 'normal').
type('turtonator', 'fire').
type('turtonator', 'dragon').
type('togedemaru', 'electric').
type('togedemaru', 'steel').
type('togedemarutotem', 'electric').
type('togedemarutotem', 'steel').
type('mimikyu', 'ghost').
type('mimikyu', 'fairy').
type('mimikyubusted', 'ghost').
type('mimikyubusted', 'fairy').
type('mimikyutotem', 'ghost').
type('mimikyutotem', 'fairy').
type('mimikyubustedtotem', 'ghost').
type('mimikyubustedtotem', 'fairy').
type('bruxish', 'water').
type('bruxish', 'psychic').
type('drampa', 'normal').
type('drampa', 'dragon').
type('drampamega', '???').
type('dhelmise', 'ghost').
type('dhelmise', 'grass').
type('jangmoo', 'dragon').
type('hakamoo', 'dragon').
type('hakamoo', 'fighting').
type('kommoo', 'dragon').
type('kommoo', 'fighting').
type('kommoototem', 'dragon').
type('kommoototem', 'fighting').
type('tapukoko', 'electric').
type('tapukoko', 'fairy').
type('tapulele', 'psychic').
type('tapulele', 'fairy').
type('tapubulu', 'grass').
type('tapubulu', 'fairy').
type('tapufini', 'water').
type('tapufini', 'fairy').
type('cosmog', 'psychic').
type('cosmoem', 'psychic').
type('solgaleo', 'psychic').
type('solgaleo', 'steel').
type('lunala', 'psychic').
type('lunala', 'ghost').
type('nihilego', 'rock').
type('nihilego', 'poison').
type('buzzwole', 'bug').
type('buzzwole', 'fighting').
type('pheromosa', 'bug').
type('pheromosa', 'fighting').
type('xurkitree', 'electric').
type('celesteela', 'steel').
type('celesteela', 'flying').
type('kartana', 'grass').
type('kartana', 'steel').
type('guzzlord', 'dark').
type('guzzlord', 'dragon').
type('necrozma', 'psychic').
type('necrozmaduskmane', 'psychic').
type('necrozmaduskmane', 'steel').
type('necrozmadawnwings', 'psychic').
type('necrozmadawnwings', 'ghost').
type('necrozmaultra', 'psychic').
type('necrozmaultra', 'dragon').
type('magearna', 'steel').
type('magearna', 'fairy').
type('magearnaoriginal', 'steel').
type('magearnaoriginal', 'fairy').
type('marshadow', 'fighting').
type('marshadow', 'ghost').
type('poipole', 'poison').
type('naganadel', 'poison').
type('naganadel', 'dragon').
type('stakataka', 'rock').
type('stakataka', 'steel').
type('blacephalon', 'fire').
type('blacephalon', 'ghost').
type('zeraora', 'electric').
type('meltan', 'steel').
type('melmetal', 'steel').
type('melmetalgmax', 'steel').
type('grookey', 'grass').
type('thwackey', 'grass').
type('rillaboom', 'grass').
type('rillaboomgmax', 'grass').
type('scorbunny', 'fire').
type('raboot', 'fire').
type('cinderace', 'fire').
type('cinderacegmax', 'fire').
type('sobble', 'water').
type('drizzile', 'water').
type('inteleon', 'water').
type('inteleongmax', 'water').
type('skwovet', 'normal').
type('greedent', 'normal').
type('rookidee', 'flying').
type('corvisquire', 'flying').
type('corviknight', 'flying').
type('corviknight', 'steel').
type('corviknightgmax', 'flying').
type('corviknightgmax', 'steel').
type('blipbug', 'bug').
type('dottler', 'bug').
type('dottler', 'psychic').
type('orbeetle', 'bug').
type('orbeetle', 'psychic').
type('orbeetlegmax', 'bug').
type('orbeetlegmax', 'psychic').
type('nickit', 'dark').
type('thievul', 'dark').
type('gossifleur', 'grass').
type('eldegoss', 'grass').
type('wooloo', 'normal').
type('dubwool', 'normal').
type('chewtle', 'water').
type('drednaw', 'water').
type('drednaw', 'rock').
type('drednawgmax', 'water').
type('drednawgmax', 'rock').
type('yamper', 'electric').
type('boltund', 'electric').
type('rolycoly', 'rock').
type('carkol', 'rock').
type('carkol', 'fire').
type('coalossal', 'rock').
type('coalossal', 'fire').
type('coalossalgmax', 'rock').
type('coalossalgmax', 'fire').
type('applin', 'grass').
type('applin', 'dragon').
type('flapple', 'grass').
type('flapple', 'dragon').
type('flapplegmax', 'grass').
type('flapplegmax', 'dragon').
type('appletun', 'grass').
type('appletun', 'dragon').
type('appletungmax', 'grass').
type('appletungmax', 'dragon').
type('silicobra', 'ground').
type('sandaconda', 'ground').
type('sandacondagmax', 'ground').
type('cramorant', 'flying').
type('cramorant', 'water').
type('cramorantgulping', 'flying').
type('cramorantgulping', 'water').
type('cramorantgorging', 'flying').
type('cramorantgorging', 'water').
type('arrokuda', 'water').
type('barraskewda', 'water').
type('toxel', 'electric').
type('toxel', 'poison').
type('toxtricity', 'electric').
type('toxtricity', 'poison').
type('toxtricitylowkey', 'electric').
type('toxtricitylowkey', 'poison').
type('toxtricitygmax', 'electric').
type('toxtricitygmax', 'poison').
type('toxtricitylowkeygmax', 'electric').
type('toxtricitylowkeygmax', 'poison').
type('sizzlipede', 'fire').
type('sizzlipede', 'bug').
type('centiskorch', 'fire').
type('centiskorch', 'bug').
type('centiskorchgmax', 'fire').
type('centiskorchgmax', 'bug').
type('clobbopus', 'fighting').
type('grapploct', 'fighting').
type('sinistea', 'ghost').
type('sinisteaantique', 'ghost').
type('polteageist', 'ghost').
type('polteageistantique', 'ghost').
type('hatenna', 'psychic').
type('hattrem', 'psychic').
type('hatterene', 'psychic').
type('hatterene', 'fairy').
type('hatterenegmax', 'psychic').
type('hatterenegmax', 'fairy').
type('impidimp', 'dark').
type('impidimp', 'fairy').
type('morgrem', 'dark').
type('morgrem', 'fairy').
type('grimmsnarl', 'dark').
type('grimmsnarl', 'fairy').
type('grimmsnarlgmax', 'dark').
type('grimmsnarlgmax', 'fairy').
type('obstagoon', 'dark').
type('obstagoon', 'normal').
type('perrserker', 'steel').
type('cursola', 'ghost').
type('sirfetchd', 'fighting').
type('mrrime', 'ice').
type('mrrime', 'psychic').
type('runerigus', 'ground').
type('runerigus', 'ghost').
type('milcery', 'fairy').
type('alcremie', 'fairy').
type('alcremierubycream', 'fairy').
type('alcremiematchacream', 'fairy').
type('alcremiemintcream', 'fairy').
type('alcremielemoncream', 'fairy').
type('alcremierubyswirl', 'fairy').
type('alcremiecaramelswirl', 'fairy').
type('alcremierainbowswirl', 'fairy').
type('alcremiegmax', 'fairy').
type('falinks', 'fighting').
type('falinksmega', '???').
type('pincurchin', 'electric').
type('snom', 'ice').
type('snom', 'bug').
type('frosmoth', 'ice').
type('frosmoth', 'bug').
type('stonjourner', 'rock').
type('eiscue', 'ice').
type('eiscuenoice', 'ice').
type('indeedee', 'psychic').
type('indeedee', 'normal').
type('indeedeef', 'psychic').
type('indeedeef', 'normal').
type('morpeko', 'electric').
type('morpeko', 'dark').
type('morpekohangry', 'electric').
type('morpekohangry', 'dark').
type('cufant', 'steel').
type('copperajah', 'steel').
type('copperajahgmax', 'steel').
type('dracozolt', 'electric').
type('dracozolt', 'dragon').
type('arctozolt', 'electric').
type('arctozolt', 'ice').
type('dracovish', 'water').
type('dracovish', 'dragon').
type('arctovish', 'water').
type('arctovish', 'ice').
type('duraludon', 'steel').
type('duraludon', 'dragon').
type('duraludongmax', 'steel').
type('duraludongmax', 'dragon').
type('dreepy', 'dragon').
type('dreepy', 'ghost').
type('drakloak', 'dragon').
type('drakloak', 'ghost').
type('dragapult', 'dragon').
type('dragapult', 'ghost').
type('zacian', 'fairy').
type('zaciancrowned', 'fairy').
type('zaciancrowned', 'steel').
type('zamazenta', 'fighting').
type('zamazentacrowned', 'fighting').
type('zamazentacrowned', 'steel').
type('eternatus', 'poison').
type('eternatus', 'dragon').
type('eternatuseternamax', 'poison').
type('eternatuseternamax', 'dragon').
type('kubfu', 'fighting').
type('urshifu', 'fighting').
type('urshifu', 'dark').
type('urshifurapidstrike', 'fighting').
type('urshifurapidstrike', 'water').
type('urshifugmax', 'fighting').
type('urshifugmax', 'dark').
type('urshifurapidstrikegmax', 'fighting').
type('urshifurapidstrikegmax', 'water').
type('zarude', 'dark').
type('zarude', 'grass').
type('zarudedada', 'dark').
type('zarudedada', 'grass').
type('regieleki', 'electric').
type('regidrago', 'dragon').
type('glastrier', 'ice').
type('spectrier', 'ghost').
type('calyrex', 'psychic').
type('calyrex', 'grass').
type('calyrexice', 'psychic').
type('calyrexice', 'ice').
type('calyrexshadow', 'psychic').
type('calyrexshadow', 'ghost').
type('wyrdeer', 'normal').
type('wyrdeer', 'psychic').
type('kleavor', 'bug').
type('kleavor', 'rock').
type('ursaluna', 'ground').
type('ursaluna', 'normal').
type('ursalunabloodmoon', 'ground').
type('ursalunabloodmoon', 'normal').
type('basculegion', 'water').
type('basculegion', 'ghost').
type('basculegionf', 'water').
type('basculegionf', 'ghost').
type('sneasler', 'fighting').
type('sneasler', 'poison').
type('overqwil', 'dark').
type('overqwil', 'poison').
type('enamorus', 'fairy').
type('enamorus', 'flying').
type('enamorustherian', 'fairy').
type('enamorustherian', 'flying').
type('sprigatito', 'grass').
type('floragato', 'grass').
type('meowscarada', 'grass').
type('meowscarada', 'dark').
type('fuecoco', 'fire').
type('crocalor', 'fire').
type('skeledirge', 'fire').
type('skeledirge', 'ghost').
type('quaxly', 'water').
type('quaxwell', 'water').
type('quaquaval', 'water').
type('quaquaval', 'fighting').
type('lechonk', 'normal').
type('oinkologne', 'normal').
type('oinkolognef', 'normal').
type('tarountula', 'bug').
type('spidops', 'bug').
type('nymble', 'bug').
type('lokix', 'bug').
type('lokix', 'dark').
type('pawmi', 'electric').
type('pawmo', 'electric').
type('pawmo', 'fighting').
type('pawmot', 'electric').
type('pawmot', 'fighting').
type('tandemaus', 'normal').
type('maushold', 'normal').
type('mausholdfour', 'normal').
type('fidough', 'fairy').
type('dachsbun', 'fairy').
type('smoliv', 'grass').
type('smoliv', 'normal').
type('dolliv', 'grass').
type('dolliv', 'normal').
type('arboliva', 'grass').
type('arboliva', 'normal').
type('squawkabilly', 'normal').
type('squawkabilly', 'flying').
type('squawkabillyblue', 'normal').
type('squawkabillyblue', 'flying').
type('squawkabillyyellow', 'normal').
type('squawkabillyyellow', 'flying').
type('squawkabillywhite', 'normal').
type('squawkabillywhite', 'flying').
type('nacli', 'rock').
type('naclstack', 'rock').
type('garganacl', 'rock').
type('charcadet', 'fire').
type('armarouge', 'fire').
type('armarouge', 'psychic').
type('ceruledge', 'fire').
type('ceruledge', 'ghost').
type('tadbulb', 'electric').
type('bellibolt', 'electric').
type('wattrel', 'electric').
type('wattrel', 'flying').
type('kilowattrel', 'electric').
type('kilowattrel', 'flying').
type('maschiff', 'dark').
type('mabosstiff', 'dark').
type('shroodle', 'poison').
type('shroodle', 'normal').
type('grafaiai', 'poison').
type('grafaiai', 'normal').
type('bramblin', 'grass').
type('bramblin', 'ghost').
type('brambleghast', 'grass').
type('brambleghast', 'ghost').
type('toedscool', 'ground').
type('toedscool', 'grass').
type('toedscruel', 'ground').
type('toedscruel', 'grass').
type('klawf', 'rock').
type('capsakid', 'grass').
type('scovillain', 'grass').
type('scovillain', 'fire').
type('rellor', 'bug').
type('rabsca', 'bug').
type('rabsca', 'psychic').
type('flittle', 'psychic').
type('espathra', 'psychic').
type('tinkatink', 'fairy').
type('tinkatink', 'steel').
type('tinkatuff', 'fairy').
type('tinkatuff', 'steel').
type('tinkaton', 'fairy').
type('tinkaton', 'steel').
type('wiglett', 'water').
type('wugtrio', 'water').
type('bombirdier', 'flying').
type('bombirdier', 'dark').
type('finizen', 'water').
type('palafin', 'water').
type('palafinhero', 'water').
type('varoom', 'steel').
type('varoom', 'poison').
type('revavroom', 'steel').
type('revavroom', 'poison').
type('cyclizar', 'dragon').
type('cyclizar', 'normal').
type('orthworm', 'steel').
type('glimmet', 'rock').
type('glimmet', 'poison').
type('glimmora', 'rock').
type('glimmora', 'poison').
type('greavard', 'ghost').
type('houndstone', 'ghost').
type('flamigo', 'flying').
type('flamigo', 'fighting').
type('cetoddle', 'ice').
type('cetitan', 'ice').
type('veluza', 'water').
type('veluza', 'psychic').
type('dondozo', 'water').
type('tatsugiri', 'dragon').
type('tatsugiri', 'water').
type('tatsugiridroopy', 'dragon').
type('tatsugiridroopy', 'water').
type('tatsugiristretchy', 'dragon').
type('tatsugiristretchy', 'water').
type('annihilape', 'fighting').
type('annihilape', 'ghost').
type('clodsire', 'poison').
type('clodsire', 'ground').
type('farigiraf', 'normal').
type('farigiraf', 'psychic').
type('dudunsparce', 'normal').
type('dudunsparcethreesegment', 'normal').
type('kingambit', 'dark').
type('kingambit', 'steel').
type('greattusk', 'ground').
type('greattusk', 'fighting').
type('screamtail', 'fairy').
type('screamtail', 'psychic').
type('brutebonnet', 'grass').
type('brutebonnet', 'dark').
type('fluttermane', 'ghost').
type('fluttermane', 'fairy').
type('slitherwing', 'bug').
type('slitherwing', 'fighting').
type('sandyshocks', 'electric').
type('sandyshocks', 'ground').
type('irontreads', 'ground').
type('irontreads', 'steel').
type('ironbundle', 'ice').
type('ironbundle', 'water').
type('ironhands', 'fighting').
type('ironhands', 'electric').
type('ironjugulis', 'dark').
type('ironjugulis', 'flying').
type('ironmoth', 'fire').
type('ironmoth', 'poison').
type('ironthorns', 'rock').
type('ironthorns', 'electric').
type('frigibax', 'dragon').
type('frigibax', 'ice').
type('arctibax', 'dragon').
type('arctibax', 'ice').
type('baxcalibur', 'dragon').
type('baxcalibur', 'ice').
type('gimmighoul', 'ghost').
type('gimmighoulroaming', 'ghost').
type('gholdengo', 'steel').
type('gholdengo', 'ghost').
type('wochien', 'dark').
type('wochien', 'grass').
type('chienpao', 'dark').
type('chienpao', 'ice').
type('tinglu', 'dark').
type('tinglu', 'ground').
type('chiyu', 'dark').
type('chiyu', 'fire').
type('roaringmoon', 'dragon').
type('roaringmoon', 'dark').
type('ironvaliant', 'fairy').
type('ironvaliant', 'fighting').
type('koraidon', 'fighting').
type('koraidon', 'dragon').
type('miraidon', 'electric').
type('miraidon', 'dragon').
type('walkingwake', 'water').
type('walkingwake', 'dragon').
type('ironleaves', 'grass').
type('ironleaves', 'psychic').
type('dipplin', 'grass').
type('dipplin', 'dragon').
type('poltchageist', 'grass').
type('poltchageist', 'ghost').
type('poltchageistartisan', 'grass').
type('poltchageistartisan', 'ghost').
type('sinistcha', 'grass').
type('sinistcha', 'ghost').
type('sinistchamasterpiece', 'grass').
type('sinistchamasterpiece', 'ghost').
type('okidogi', 'poison').
type('okidogi', 'fighting').
type('munkidori', 'poison').
type('munkidori', 'psychic').
type('fezandipiti', 'poison').
type('fezandipiti', 'fairy').
type('ogerpon', 'grass').
type('ogerponwellspring', 'grass').
type('ogerponwellspring', 'water').
type('ogerponhearthflame', 'grass').
type('ogerponhearthflame', 'fire').
type('ogerponcornerstone', 'grass').
type('ogerponcornerstone', 'rock').
type('ogerpontealtera', 'grass').
type('ogerponwellspringtera', 'grass').
type('ogerponwellspringtera', 'water').
type('ogerponhearthflametera', 'grass').
type('ogerponhearthflametera', 'fire').
type('ogerponcornerstonetera', 'grass').
type('ogerponcornerstonetera', 'rock').
type('archaludon', 'steel').
type('archaludon', 'dragon').
type('hydrapple', 'grass').
type('hydrapple', 'dragon').
type('gougingfire', 'fire').
type('gougingfire', 'dragon').
type('ragingbolt', 'electric').
type('ragingbolt', 'dragon').
type('ironboulder', 'rock').
type('ironboulder', 'psychic').
type('ironcrown', 'steel').
type('ironcrown', 'psychic').
type('terapagos', 'normal').
type('terapagosterastal', 'normal').
type('terapagosstellar', 'normal').
type('pecharunt', 'poison').
type('pecharunt', 'ghost').
type('missingno', 'bird').
type('missingno', 'normal').
type('ramnarok', '???').
type('ramnarokradiant', '???').
type('pokestarsmeargle', 'normal').
type('pokestarufo', 'flying').
type('pokestarufo', 'electric').
type('pokestarufo2', 'psychic').
type('pokestarufo2', 'electric').
type('pokestarbrycenman', 'dark').
type('pokestarbrycenman', 'psychic').
type('pokestarmt', 'steel').
type('pokestarmt2', 'steel').
type('pokestarmt2', 'electric').
type('pokestartransport', 'steel').
type('pokestargiant', 'normal').
type('pokestarhumanoid', 'normal').
type('pokestarmonster', 'dark').
type('pokestarf00', 'steel').
type('pokestarf00', 'normal').
type('pokestarf002', 'steel').
type('pokestarf002', 'normal').
type('pokestarspirit', 'dark').
type('pokestarspirit', 'ghost').
type('pokestarblackdoor', 'grass').
type('pokestarwhitedoor', 'fire').
type('pokestarblackbelt', 'fighting').
type('pokestarufopropu2', 'psychic').
type('pokestarufopropu2', 'electric').
pokemon_abilities('bulbasaur', ['overgrow', 'chlorophyll']).
pokemon_abilities('ivysaur', ['overgrow', 'chlorophyll']).
pokemon_abilities('venusaur', ['overgrow', 'chlorophyll']).
pokemon_abilities('venusaurmega', ['thickfat']).
pokemon_abilities('venusaurgmax', ['overgrow', 'chlorophyll']).
pokemon_abilities('charmander', ['blaze', 'solarpower']).
pokemon_abilities('charmeleon', ['blaze', 'solarpower']).
pokemon_abilities('charizard', ['blaze', 'solarpower']).
pokemon_abilities('charizardmegax', ['toughclaws']).
pokemon_abilities('charizardmegay', ['drought']).
pokemon_abilities('charizardgmax', ['blaze', 'solarpower']).
pokemon_abilities('squirtle', ['torrent', 'raindish']).
pokemon_abilities('wartortle', ['torrent', 'raindish']).
pokemon_abilities('blastoise', ['torrent', 'raindish']).
pokemon_abilities('blastoisemega', ['megalauncher']).
pokemon_abilities('blastoisegmax', ['torrent', 'raindish']).
pokemon_abilities('caterpie', ['shielddust', 'runaway']).
pokemon_abilities('metapod', ['shedskin']).
pokemon_abilities('butterfree', ['compoundeyes', 'tintedlens']).
pokemon_abilities('butterfreegmax', ['compoundeyes', 'tintedlens']).
pokemon_abilities('weedle', ['shielddust', 'runaway']).
pokemon_abilities('kakuna', ['shedskin']).
pokemon_abilities('beedrill', ['swarm', 'sniper']).
pokemon_abilities('beedrillmega', ['adaptability']).
pokemon_abilities('pidgey', ['keeneye', 'tangledfeet', 'bigpecks']).
pokemon_abilities('pidgeotto', ['keeneye', 'tangledfeet', 'bigpecks']).
pokemon_abilities('pidgeot', ['keeneye', 'tangledfeet', 'bigpecks']).
pokemon_abilities('pidgeotmega', ['noguard']).
pokemon_abilities('rattata', ['runaway', 'guts', 'hustle']).
pokemon_abilities('rattataalola', ['gluttony', 'hustle', 'thickfat']).
pokemon_abilities('raticate', ['runaway', 'guts', 'hustle']).
pokemon_abilities('raticatealola', ['gluttony', 'hustle', 'thickfat']).
pokemon_abilities('raticatealolatotem', ['thickfat']).
pokemon_abilities('spearow', ['keeneye', 'sniper']).
pokemon_abilities('fearow', ['keeneye', 'sniper']).
pokemon_abilities('ekans', ['intimidate', 'shedskin', 'unnerve']).
pokemon_abilities('arbok', ['intimidate', 'shedskin', 'unnerve']).
pokemon_abilities('pikachu', ['static', 'lightningrod']).
pokemon_abilities('pikachucosplay', ['lightningrod']).
pokemon_abilities('pikachurockstar', ['lightningrod']).
pokemon_abilities('pikachubelle', ['lightningrod']).
pokemon_abilities('pikachupopstar', ['lightningrod']).
pokemon_abilities('pikachuphd', ['lightningrod']).
pokemon_abilities('pikachulibre', ['lightningrod']).
pokemon_abilities('pikachuoriginal', ['static', 'lightningrod']).
pokemon_abilities('pikachuhoenn', ['static', 'lightningrod']).
pokemon_abilities('pikachusinnoh', ['static', 'lightningrod']).
pokemon_abilities('pikachuunova', ['static', 'lightningrod']).
pokemon_abilities('pikachukalos', ['static', 'lightningrod']).
pokemon_abilities('pikachualola', ['static', 'lightningrod']).
pokemon_abilities('pikachupartner', ['static', 'lightningrod']).
pokemon_abilities('pikachustarter', ['static', 'lightningrod']).
pokemon_abilities('pikachugmax', ['static', 'lightningrod']).
pokemon_abilities('pikachuworld', ['static', 'lightningrod']).
pokemon_abilities('raichu', ['static', 'lightningrod']).
pokemon_abilities('raichualola', ['surgesurfer']).
pokemon_abilities('sandshrew', ['sandveil', 'sandrush']).
pokemon_abilities('sandshrewalola', ['snowcloak', 'slushrush']).
pokemon_abilities('sandslash', ['sandveil', 'sandrush']).
pokemon_abilities('sandslashalola', ['snowcloak', 'slushrush']).
pokemon_abilities('nidoranf', ['poisonpoint', 'rivalry', 'hustle']).
pokemon_abilities('nidorina', ['poisonpoint', 'rivalry', 'hustle']).
pokemon_abilities('nidoqueen', ['poisonpoint', 'rivalry', 'sheerforce']).
pokemon_abilities('nidoranm', ['poisonpoint', 'rivalry', 'hustle']).
pokemon_abilities('nidorino', ['poisonpoint', 'rivalry', 'hustle']).
pokemon_abilities('nidoking', ['poisonpoint', 'rivalry', 'sheerforce']).
pokemon_abilities('clefairy', ['cutecharm', 'magicguard', 'friendguard']).
pokemon_abilities('clefable', ['cutecharm', 'magicguard', 'unaware']).
pokemon_abilities('clefablemega', ['']).
pokemon_abilities('vulpix', ['flashfire', 'drought']).
pokemon_abilities('vulpixalola', ['snowcloak', 'snowwarning']).
pokemon_abilities('ninetales', ['flashfire', 'drought']).
pokemon_abilities('ninetalesalola', ['snowcloak', 'snowwarning']).
pokemon_abilities('jigglypuff', ['cutecharm', 'competitive', 'friendguard']).
pokemon_abilities('wigglytuff', ['cutecharm', 'competitive', 'frisk']).
pokemon_abilities('zubat', ['innerfocus', 'infiltrator']).
pokemon_abilities('golbat', ['innerfocus', 'infiltrator']).
pokemon_abilities('oddish', ['chlorophyll', 'runaway']).
pokemon_abilities('gloom', ['chlorophyll', 'stench']).
pokemon_abilities('vileplume', ['chlorophyll', 'effectspore']).
pokemon_abilities('paras', ['effectspore', 'dryskin', 'damp']).
pokemon_abilities('parasect', ['effectspore', 'dryskin', 'damp']).
pokemon_abilities('venonat', ['compoundeyes', 'tintedlens', 'runaway']).
pokemon_abilities('venomoth', ['shielddust', 'tintedlens', 'wonderskin']).
pokemon_abilities('diglett', ['sandveil', 'arenatrap', 'sandforce']).
pokemon_abilities('diglettalola', ['sandveil', 'tanglinghair', 'sandforce']).
pokemon_abilities('dugtrio', ['sandveil', 'arenatrap', 'sandforce']).
pokemon_abilities('dugtrioalola', ['sandveil', 'tanglinghair', 'sandforce']).
pokemon_abilities('meowth', ['pickup', 'technician', 'unnerve']).
pokemon_abilities('meowthalola', ['pickup', 'technician', 'rattled']).
pokemon_abilities('meowthgalar', ['pickup', 'toughclaws', 'unnerve']).
pokemon_abilities('meowthgmax', ['pickup', 'technician', 'unnerve']).
pokemon_abilities('persian', ['limber', 'technician', 'unnerve']).
pokemon_abilities('persianalola', ['furcoat', 'technician', 'rattled']).
pokemon_abilities('psyduck', ['damp', 'cloudnine', 'swiftswim']).
pokemon_abilities('golduck', ['damp', 'cloudnine', 'swiftswim']).
pokemon_abilities('mankey', ['vitalspirit', 'angerpoint', 'defiant']).
pokemon_abilities('primeape', ['vitalspirit', 'angerpoint', 'defiant']).
pokemon_abilities('growlithe', ['intimidate', 'flashfire', 'justified']).
pokemon_abilities('growlithehisui', ['intimidate', 'flashfire', 'rockhead']).
pokemon_abilities('arcanine', ['intimidate', 'flashfire', 'justified']).
pokemon_abilities('arcaninehisui', ['intimidate', 'flashfire', 'rockhead']).
pokemon_abilities('poliwag', ['waterabsorb', 'damp', 'swiftswim']).
pokemon_abilities('poliwhirl', ['waterabsorb', 'damp', 'swiftswim']).
pokemon_abilities('poliwrath', ['waterabsorb', 'damp', 'swiftswim']).
pokemon_abilities('abra', ['synchronize', 'innerfocus', 'magicguard']).
pokemon_abilities('kadabra', ['synchronize', 'innerfocus', 'magicguard']).
pokemon_abilities('alakazam', ['synchronize', 'innerfocus', 'magicguard']).
pokemon_abilities('alakazammega', ['trace']).
pokemon_abilities('machop', ['guts', 'noguard', 'steadfast']).
pokemon_abilities('machoke', ['guts', 'noguard', 'steadfast']).
pokemon_abilities('machamp', ['guts', 'noguard', 'steadfast']).
pokemon_abilities('machampgmax', ['guts', 'noguard', 'steadfast']).
pokemon_abilities('bellsprout', ['chlorophyll', 'gluttony']).
pokemon_abilities('weepinbell', ['chlorophyll', 'gluttony']).
pokemon_abilities('victreebel', ['chlorophyll', 'gluttony']).
pokemon_abilities('victreebelmega', ['']).
pokemon_abilities('tentacool', ['clearbody', 'liquidooze', 'raindish']).
pokemon_abilities('tentacruel', ['clearbody', 'liquidooze', 'raindish']).
pokemon_abilities('geodude', ['rockhead', 'sturdy', 'sandveil']).
pokemon_abilities('geodudealola', ['magnetpull', 'sturdy', 'galvanize']).
pokemon_abilities('graveler', ['rockhead', 'sturdy', 'sandveil']).
pokemon_abilities('graveleralola', ['magnetpull', 'sturdy', 'galvanize']).
pokemon_abilities('golem', ['rockhead', 'sturdy', 'sandveil']).
pokemon_abilities('golemalola', ['magnetpull', 'sturdy', 'galvanize']).
pokemon_abilities('ponyta', ['runaway', 'flashfire', 'flamebody']).
pokemon_abilities('ponytagalar', ['runaway', 'pastelveil', 'anticipation']).
pokemon_abilities('rapidash', ['runaway', 'flashfire', 'flamebody']).
pokemon_abilities('rapidashgalar', ['runaway', 'pastelveil', 'anticipation']).
pokemon_abilities('slowpoke', ['oblivious', 'owntempo', 'regenerator']).
pokemon_abilities('slowpokegalar', ['gluttony', 'owntempo', 'regenerator']).
pokemon_abilities('slowbro', ['oblivious', 'owntempo', 'regenerator']).
pokemon_abilities('slowbromega', ['shellarmor']).
pokemon_abilities('slowbrogalar', ['quickdraw', 'owntempo', 'regenerator']).
pokemon_abilities('magnemite', ['magnetpull', 'sturdy', 'analytic']).
pokemon_abilities('magneton', ['magnetpull', 'sturdy', 'analytic']).
pokemon_abilities('farfetchd', ['keeneye', 'innerfocus', 'defiant']).
pokemon_abilities('farfetchdgalar', ['steadfast', 'scrappy']).
pokemon_abilities('doduo', ['runaway', 'earlybird', 'tangledfeet']).
pokemon_abilities('dodrio', ['runaway', 'earlybird', 'tangledfeet']).
pokemon_abilities('seel', ['thickfat', 'hydration', 'icebody']).
pokemon_abilities('dewgong', ['thickfat', 'hydration', 'icebody']).
pokemon_abilities('grimer', ['stench', 'stickyhold', 'poisontouch']).
pokemon_abilities('grimeralola', ['poisontouch', 'gluttony', 'powerofalchemy']).
pokemon_abilities('muk', ['stench', 'stickyhold', 'poisontouch']).
pokemon_abilities('mukalola', ['poisontouch', 'gluttony', 'powerofalchemy']).
pokemon_abilities('shellder', ['shellarmor', 'skilllink', 'overcoat']).
pokemon_abilities('cloyster', ['shellarmor', 'skilllink', 'overcoat']).
pokemon_abilities('gastly', ['levitate']).
pokemon_abilities('haunter', ['levitate']).
pokemon_abilities('gengar', ['cursedbody']).
pokemon_abilities('gengarmega', ['shadowtag']).
pokemon_abilities('gengargmax', ['cursedbody']).
pokemon_abilities('onix', ['rockhead', 'sturdy', 'weakarmor']).
pokemon_abilities('drowzee', ['insomnia', 'forewarn', 'innerfocus']).
pokemon_abilities('hypno', ['insomnia', 'forewarn', 'innerfocus']).
pokemon_abilities('krabby', ['hypercutter', 'shellarmor', 'sheerforce']).
pokemon_abilities('kingler', ['hypercutter', 'shellarmor', 'sheerforce']).
pokemon_abilities('kinglergmax', ['hypercutter', 'shellarmor', 'sheerforce']).
pokemon_abilities('voltorb', ['soundproof', 'static', 'aftermath']).
pokemon_abilities('voltorbhisui', ['soundproof', 'static', 'aftermath']).
pokemon_abilities('electrode', ['soundproof', 'static', 'aftermath']).
pokemon_abilities('electrodehisui', ['soundproof', 'static', 'aftermath']).
pokemon_abilities('exeggcute', ['chlorophyll', 'harvest']).
pokemon_abilities('exeggutor', ['chlorophyll', 'harvest']).
pokemon_abilities('exeggutoralola', ['frisk', 'harvest']).
pokemon_abilities('cubone', ['rockhead', 'lightningrod', 'battlearmor']).
pokemon_abilities('marowak', ['rockhead', 'lightningrod', 'battlearmor']).
pokemon_abilities('marowakalola', ['cursedbody', 'lightningrod', 'rockhead']).
pokemon_abilities('marowakalolatotem', ['rockhead']).
pokemon_abilities('hitmonlee', ['limber', 'reckless', 'unburden']).
pokemon_abilities('hitmonchan', ['keeneye', 'ironfist', 'innerfocus']).
pokemon_abilities('lickitung', ['owntempo', 'oblivious', 'cloudnine']).
pokemon_abilities('koffing', ['levitate', 'neutralizinggas', 'stench']).
pokemon_abilities('weezing', ['levitate', 'neutralizinggas', 'stench']).
pokemon_abilities('weezinggalar', ['levitate', 'neutralizinggas', 'mistysurge']).
pokemon_abilities('rhyhorn', ['lightningrod', 'rockhead', 'reckless']).
pokemon_abilities('rhydon', ['lightningrod', 'rockhead', 'reckless']).
pokemon_abilities('chansey', ['naturalcure', 'serenegrace', 'healer']).
pokemon_abilities('tangela', ['chlorophyll', 'leafguard', 'regenerator']).
pokemon_abilities('kangaskhan', ['earlybird', 'scrappy', 'innerfocus']).
pokemon_abilities('kangaskhanmega', ['parentalbond']).
pokemon_abilities('horsea', ['swiftswim', 'sniper', 'damp']).
pokemon_abilities('seadra', ['poisonpoint', 'sniper', 'damp']).
pokemon_abilities('goldeen', ['swiftswim', 'waterveil', 'lightningrod']).
pokemon_abilities('seaking', ['swiftswim', 'waterveil', 'lightningrod']).
pokemon_abilities('staryu', ['illuminate', 'naturalcure', 'analytic']).
pokemon_abilities('starmie', ['illuminate', 'naturalcure', 'analytic']).
pokemon_abilities('starmiemega', ['']).
pokemon_abilities('mrmime', ['soundproof', 'filter', 'technician']).
pokemon_abilities('mrmimegalar', ['vitalspirit', 'screencleaner', 'icebody']).
pokemon_abilities('scyther', ['swarm', 'technician', 'steadfast']).
pokemon_abilities('jynx', ['oblivious', 'forewarn', 'dryskin']).
pokemon_abilities('electabuzz', ['static', 'vitalspirit']).
pokemon_abilities('magmar', ['flamebody', 'vitalspirit']).
pokemon_abilities('pinsir', ['hypercutter', 'moldbreaker', 'moxie']).
pokemon_abilities('pinsirmega', ['aerilate']).
pokemon_abilities('tauros', ['intimidate', 'angerpoint', 'sheerforce']).
pokemon_abilities('taurospaldeacombat', ['intimidate', 'angerpoint', 'cudchew']).
pokemon_abilities('taurospaldeablaze', ['intimidate', 'angerpoint', 'cudchew']).
pokemon_abilities('taurospaldeaaqua', ['intimidate', 'angerpoint', 'cudchew']).
pokemon_abilities('magikarp', ['swiftswim', 'rattled']).
pokemon_abilities('gyarados', ['intimidate', 'moxie']).
pokemon_abilities('gyaradosmega', ['moldbreaker']).
pokemon_abilities('lapras', ['waterabsorb', 'shellarmor', 'hydration']).
pokemon_abilities('laprasgmax', ['waterabsorb', 'shellarmor', 'hydration']).
pokemon_abilities('ditto', ['limber', 'imposter']).
pokemon_abilities('eevee', ['runaway', 'adaptability', 'anticipation']).
pokemon_abilities('eeveestarter', ['runaway', 'adaptability', 'anticipation']).
pokemon_abilities('eeveegmax', ['runaway', 'adaptability', 'anticipation']).
pokemon_abilities('vaporeon', ['waterabsorb', 'hydration']).
pokemon_abilities('jolteon', ['voltabsorb', 'quickfeet']).
pokemon_abilities('flareon', ['flashfire', 'guts']).
pokemon_abilities('porygon', ['trace', 'download', 'analytic']).
pokemon_abilities('omanyte', ['swiftswim', 'shellarmor', 'weakarmor']).
pokemon_abilities('omastar', ['swiftswim', 'shellarmor', 'weakarmor']).
pokemon_abilities('kabuto', ['swiftswim', 'battlearmor', 'weakarmor']).
pokemon_abilities('kabutops', ['swiftswim', 'battlearmor', 'weakarmor']).
pokemon_abilities('aerodactyl', ['rockhead', 'pressure', 'unnerve']).
pokemon_abilities('aerodactylmega', ['toughclaws']).
pokemon_abilities('snorlax', ['immunity', 'thickfat', 'gluttony']).
pokemon_abilities('snorlaxgmax', ['immunity', 'thickfat', 'gluttony']).
pokemon_abilities('articuno', ['pressure', 'snowcloak']).
pokemon_abilities('articunogalar', ['competitive']).
pokemon_abilities('zapdos', ['pressure', 'static']).
pokemon_abilities('zapdosgalar', ['defiant']).
pokemon_abilities('moltres', ['pressure', 'flamebody']).
pokemon_abilities('moltresgalar', ['berserk']).
pokemon_abilities('dratini', ['shedskin', 'marvelscale']).
pokemon_abilities('dragonair', ['shedskin', 'marvelscale']).
pokemon_abilities('dragonite', ['innerfocus', 'multiscale']).
pokemon_abilities('dragonitemega', ['']).
pokemon_abilities('mewtwo', ['pressure', 'unnerve']).
pokemon_abilities('mewtwomegax', ['steadfast']).
pokemon_abilities('mewtwomegay', ['insomnia']).
pokemon_abilities('mew', ['synchronize']).
pokemon_abilities('chikorita', ['overgrow', 'leafguard']).
pokemon_abilities('bayleef', ['overgrow', 'leafguard']).
pokemon_abilities('meganium', ['overgrow', 'leafguard']).
pokemon_abilities('meganiummega', ['']).
pokemon_abilities('cyndaquil', ['blaze', 'flashfire']).
pokemon_abilities('quilava', ['blaze', 'flashfire']).
pokemon_abilities('typhlosion', ['blaze', 'flashfire']).
pokemon_abilities('typhlosionhisui', ['blaze', 'frisk']).
pokemon_abilities('totodile', ['torrent', 'sheerforce']).
pokemon_abilities('croconaw', ['torrent', 'sheerforce']).
pokemon_abilities('feraligatr', ['torrent', 'sheerforce']).
pokemon_abilities('feraligatrmega', ['']).
pokemon_abilities('sentret', ['runaway', 'keeneye', 'frisk']).
pokemon_abilities('furret', ['runaway', 'keeneye', 'frisk']).
pokemon_abilities('hoothoot', ['insomnia', 'keeneye', 'tintedlens']).
pokemon_abilities('noctowl', ['insomnia', 'keeneye', 'tintedlens']).
pokemon_abilities('ledyba', ['swarm', 'earlybird', 'rattled']).
pokemon_abilities('ledian', ['swarm', 'earlybird', 'ironfist']).
pokemon_abilities('spinarak', ['swarm', 'insomnia', 'sniper']).
pokemon_abilities('ariados', ['swarm', 'insomnia', 'sniper']).
pokemon_abilities('crobat', ['innerfocus', 'infiltrator']).
pokemon_abilities('chinchou', ['voltabsorb', 'illuminate', 'waterabsorb']).
pokemon_abilities('lanturn', ['voltabsorb', 'illuminate', 'waterabsorb']).
pokemon_abilities('pichu', ['static', 'lightningrod']).
pokemon_abilities('pichuspikyeared', ['static']).
pokemon_abilities('cleffa', ['cutecharm', 'magicguard', 'friendguard']).
pokemon_abilities('igglybuff', ['cutecharm', 'competitive', 'friendguard']).
pokemon_abilities('togepi', ['hustle', 'serenegrace', 'superluck']).
pokemon_abilities('togetic', ['hustle', 'serenegrace', 'superluck']).
pokemon_abilities('natu', ['synchronize', 'earlybird', 'magicbounce']).
pokemon_abilities('xatu', ['synchronize', 'earlybird', 'magicbounce']).
pokemon_abilities('mareep', ['static', 'plus']).
pokemon_abilities('flaaffy', ['static', 'plus']).
pokemon_abilities('ampharos', ['static', 'plus']).
pokemon_abilities('ampharosmega', ['moldbreaker']).
pokemon_abilities('bellossom', ['chlorophyll', 'healer']).
pokemon_abilities('marill', ['thickfat', 'hugepower', 'sapsipper']).
pokemon_abilities('azumarill', ['thickfat', 'hugepower', 'sapsipper']).
pokemon_abilities('sudowoodo', ['sturdy', 'rockhead', 'rattled']).
pokemon_abilities('politoed', ['waterabsorb', 'damp', 'drizzle']).
pokemon_abilities('hoppip', ['chlorophyll', 'leafguard', 'infiltrator']).
pokemon_abilities('skiploom', ['chlorophyll', 'leafguard', 'infiltrator']).
pokemon_abilities('jumpluff', ['chlorophyll', 'leafguard', 'infiltrator']).
pokemon_abilities('aipom', ['runaway', 'pickup', 'skilllink']).
pokemon_abilities('sunkern', ['chlorophyll', 'solarpower', 'earlybird']).
pokemon_abilities('sunflora', ['chlorophyll', 'solarpower', 'earlybird']).
pokemon_abilities('yanma', ['speedboost', 'compoundeyes', 'frisk']).
pokemon_abilities('wooper', ['damp', 'waterabsorb', 'unaware']).
pokemon_abilities('wooperpaldea', ['poisonpoint', 'waterabsorb', 'unaware']).
pokemon_abilities('quagsire', ['damp', 'waterabsorb', 'unaware']).
pokemon_abilities('espeon', ['synchronize', 'magicbounce']).
pokemon_abilities('umbreon', ['synchronize', 'innerfocus']).
pokemon_abilities('murkrow', ['insomnia', 'superluck', 'prankster']).
pokemon_abilities('slowking', ['oblivious', 'owntempo', 'regenerator']).
pokemon_abilities('slowkinggalar', ['curiousmedicine', 'owntempo', 'regenerator']).
pokemon_abilities('misdreavus', ['levitate']).
pokemon_abilities('unown', ['levitate']).
pokemon_abilities('wobbuffet', ['shadowtag', 'telepathy']).
pokemon_abilities('girafarig', ['innerfocus', 'earlybird', 'sapsipper']).
pokemon_abilities('pineco', ['sturdy', 'overcoat']).
pokemon_abilities('forretress', ['sturdy', 'overcoat']).
pokemon_abilities('dunsparce', ['serenegrace', 'runaway', 'rattled']).
pokemon_abilities('gligar', ['hypercutter', 'sandveil', 'immunity']).
pokemon_abilities('steelix', ['rockhead', 'sturdy', 'sheerforce']).
pokemon_abilities('steelixmega', ['sandforce']).
pokemon_abilities('snubbull', ['intimidate', 'runaway', 'rattled']).
pokemon_abilities('granbull', ['intimidate', 'quickfeet', 'rattled']).
pokemon_abilities('qwilfish', ['poisonpoint', 'swiftswim', 'intimidate']).
pokemon_abilities('qwilfishhisui', ['poisonpoint', 'swiftswim', 'intimidate']).
pokemon_abilities('scizor', ['swarm', 'technician', 'lightmetal']).
pokemon_abilities('scizormega', ['technician']).
pokemon_abilities('shuckle', ['sturdy', 'gluttony', 'contrary']).
pokemon_abilities('heracross', ['swarm', 'guts', 'moxie']).
pokemon_abilities('heracrossmega', ['skilllink']).
pokemon_abilities('sneasel', ['innerfocus', 'keeneye', 'pickpocket']).
pokemon_abilities('sneaselhisui', ['innerfocus', 'keeneye', 'pickpocket']).
pokemon_abilities('teddiursa', ['pickup', 'quickfeet', 'honeygather']).
pokemon_abilities('ursaring', ['guts', 'quickfeet', 'unnerve']).
pokemon_abilities('slugma', ['magmaarmor', 'flamebody', 'weakarmor']).
pokemon_abilities('magcargo', ['magmaarmor', 'flamebody', 'weakarmor']).
pokemon_abilities('swinub', ['oblivious', 'snowcloak', 'thickfat']).
pokemon_abilities('piloswine', ['oblivious', 'snowcloak', 'thickfat']).
pokemon_abilities('corsola', ['hustle', 'naturalcure', 'regenerator']).
pokemon_abilities('corsolagalar', ['weakarmor', 'cursedbody']).
pokemon_abilities('remoraid', ['hustle', 'sniper', 'moody']).
pokemon_abilities('octillery', ['suctioncups', 'sniper', 'moody']).
pokemon_abilities('delibird', ['vitalspirit', 'hustle', 'insomnia']).
pokemon_abilities('mantine', ['swiftswim', 'waterabsorb', 'waterveil']).
pokemon_abilities('skarmory', ['keeneye', 'sturdy', 'weakarmor']).
pokemon_abilities('skarmorymega', ['']).
pokemon_abilities('houndour', ['earlybird', 'flashfire', 'unnerve']).
pokemon_abilities('houndoom', ['earlybird', 'flashfire', 'unnerve']).
pokemon_abilities('houndoommega', ['solarpower']).
pokemon_abilities('kingdra', ['swiftswim', 'sniper', 'damp']).
pokemon_abilities('phanpy', ['pickup', 'sandveil']).
pokemon_abilities('donphan', ['sturdy', 'sandveil']).
pokemon_abilities('porygon2', ['trace', 'download', 'analytic']).
pokemon_abilities('stantler', ['intimidate', 'frisk', 'sapsipper']).
pokemon_abilities('smeargle', ['owntempo', 'technician', 'moody']).
pokemon_abilities('tyrogue', ['guts', 'steadfast', 'vitalspirit']).
pokemon_abilities('hitmontop', ['intimidate', 'technician', 'steadfast']).
pokemon_abilities('smoochum', ['oblivious', 'forewarn', 'hydration']).
pokemon_abilities('elekid', ['static', 'vitalspirit']).
pokemon_abilities('magby', ['flamebody', 'vitalspirit']).
pokemon_abilities('miltank', ['thickfat', 'scrappy', 'sapsipper']).
pokemon_abilities('blissey', ['naturalcure', 'serenegrace', 'healer']).
pokemon_abilities('raikou', ['pressure', 'innerfocus']).
pokemon_abilities('entei', ['pressure', 'innerfocus']).
pokemon_abilities('suicune', ['pressure', 'innerfocus']).
pokemon_abilities('larvitar', ['guts', 'sandveil']).
pokemon_abilities('pupitar', ['shedskin']).
pokemon_abilities('tyranitar', ['sandstream', 'unnerve']).
pokemon_abilities('tyranitarmega', ['sandstream']).
pokemon_abilities('lugia', ['pressure', 'multiscale']).
pokemon_abilities('hooh', ['pressure', 'regenerator']).
pokemon_abilities('celebi', ['naturalcure']).
pokemon_abilities('treecko', ['overgrow', 'unburden']).
pokemon_abilities('grovyle', ['overgrow', 'unburden']).
pokemon_abilities('sceptile', ['overgrow', 'unburden']).
pokemon_abilities('sceptilemega', ['lightningrod']).
pokemon_abilities('torchic', ['blaze', 'speedboost']).
pokemon_abilities('combusken', ['blaze', 'speedboost']).
pokemon_abilities('blaziken', ['blaze', 'speedboost']).
pokemon_abilities('blazikenmega', ['speedboost']).
pokemon_abilities('mudkip', ['torrent', 'damp']).
pokemon_abilities('marshtomp', ['torrent', 'damp']).
pokemon_abilities('swampert', ['torrent', 'damp']).
pokemon_abilities('swampertmega', ['swiftswim']).
pokemon_abilities('poochyena', ['runaway', 'quickfeet', 'rattled']).
pokemon_abilities('mightyena', ['intimidate', 'quickfeet', 'moxie']).
pokemon_abilities('zigzagoon', ['pickup', 'gluttony', 'quickfeet']).
pokemon_abilities('zigzagoongalar', ['pickup', 'gluttony', 'quickfeet']).
pokemon_abilities('linoone', ['pickup', 'gluttony', 'quickfeet']).
pokemon_abilities('linoonegalar', ['pickup', 'gluttony', 'quickfeet']).
pokemon_abilities('wurmple', ['shielddust', 'runaway']).
pokemon_abilities('silcoon', ['shedskin']).
pokemon_abilities('beautifly', ['swarm', 'rivalry']).
pokemon_abilities('cascoon', ['shedskin']).
pokemon_abilities('dustox', ['shielddust', 'compoundeyes']).
pokemon_abilities('lotad', ['swiftswim', 'raindish', 'owntempo']).
pokemon_abilities('lombre', ['swiftswim', 'raindish', 'owntempo']).
pokemon_abilities('ludicolo', ['swiftswim', 'raindish', 'owntempo']).
pokemon_abilities('seedot', ['chlorophyll', 'earlybird', 'pickpocket']).
pokemon_abilities('nuzleaf', ['chlorophyll', 'earlybird', 'pickpocket']).
pokemon_abilities('shiftry', ['chlorophyll', 'windrider', 'pickpocket']).
pokemon_abilities('taillow', ['guts', 'scrappy']).
pokemon_abilities('swellow', ['guts', 'scrappy']).
pokemon_abilities('wingull', ['keeneye', 'hydration', 'raindish']).
pokemon_abilities('pelipper', ['keeneye', 'drizzle', 'raindish']).
pokemon_abilities('ralts', ['synchronize', 'trace', 'telepathy']).
pokemon_abilities('kirlia', ['synchronize', 'trace', 'telepathy']).
pokemon_abilities('gardevoir', ['synchronize', 'trace', 'telepathy']).
pokemon_abilities('gardevoirmega', ['pixilate']).
pokemon_abilities('surskit', ['swiftswim', 'raindish']).
pokemon_abilities('masquerain', ['intimidate', 'unnerve']).
pokemon_abilities('shroomish', ['effectspore', 'poisonheal', 'quickfeet']).
pokemon_abilities('breloom', ['effectspore', 'poisonheal', 'technician']).
pokemon_abilities('slakoth', ['truant']).
pokemon_abilities('vigoroth', ['vitalspirit']).
pokemon_abilities('slaking', ['truant']).
pokemon_abilities('nincada', ['compoundeyes', 'runaway']).
pokemon_abilities('ninjask', ['speedboost', 'infiltrator']).
pokemon_abilities('shedinja', ['wonderguard']).
pokemon_abilities('whismur', ['soundproof', 'rattled']).
pokemon_abilities('loudred', ['soundproof', 'scrappy']).
pokemon_abilities('exploud', ['soundproof', 'scrappy']).
pokemon_abilities('makuhita', ['thickfat', 'guts', 'sheerforce']).
pokemon_abilities('hariyama', ['thickfat', 'guts', 'sheerforce']).
pokemon_abilities('azurill', ['thickfat', 'hugepower', 'sapsipper']).
pokemon_abilities('nosepass', ['sturdy', 'magnetpull', 'sandforce']).
pokemon_abilities('skitty', ['cutecharm', 'normalize', 'wonderskin']).
pokemon_abilities('delcatty', ['cutecharm', 'normalize', 'wonderskin']).
pokemon_abilities('sableye', ['keeneye', 'stall', 'prankster']).
pokemon_abilities('sableyemega', ['magicbounce']).
pokemon_abilities('mawile', ['hypercutter', 'intimidate', 'sheerforce']).
pokemon_abilities('mawilemega', ['hugepower']).
pokemon_abilities('aron', ['sturdy', 'rockhead', 'heavymetal']).
pokemon_abilities('lairon', ['sturdy', 'rockhead', 'heavymetal']).
pokemon_abilities('aggron', ['sturdy', 'rockhead', 'heavymetal']).
pokemon_abilities('aggronmega', ['filter']).
pokemon_abilities('meditite', ['purepower', 'telepathy']).
pokemon_abilities('medicham', ['purepower', 'telepathy']).
pokemon_abilities('medichammega', ['purepower']).
pokemon_abilities('electrike', ['static', 'lightningrod', 'minus']).
pokemon_abilities('manectric', ['static', 'lightningrod', 'minus']).
pokemon_abilities('manectricmega', ['intimidate']).
pokemon_abilities('plusle', ['plus', 'lightningrod']).
pokemon_abilities('minun', ['minus', 'voltabsorb']).
pokemon_abilities('volbeat', ['illuminate', 'swarm', 'prankster']).
pokemon_abilities('illumise', ['oblivious', 'tintedlens', 'prankster']).
pokemon_abilities('roselia', ['naturalcure', 'poisonpoint', 'leafguard']).
pokemon_abilities('gulpin', ['liquidooze', 'stickyhold', 'gluttony']).
pokemon_abilities('swalot', ['liquidooze', 'stickyhold', 'gluttony']).
pokemon_abilities('carvanha', ['roughskin', 'speedboost']).
pokemon_abilities('sharpedo', ['roughskin', 'speedboost']).
pokemon_abilities('sharpedomega', ['strongjaw']).
pokemon_abilities('wailmer', ['waterveil', 'oblivious', 'pressure']).
pokemon_abilities('wailord', ['waterveil', 'oblivious', 'pressure']).
pokemon_abilities('numel', ['oblivious', 'simple', 'owntempo']).
pokemon_abilities('camerupt', ['magmaarmor', 'solidrock', 'angerpoint']).
pokemon_abilities('cameruptmega', ['sheerforce']).
pokemon_abilities('torkoal', ['whitesmoke', 'drought', 'shellarmor']).
pokemon_abilities('spoink', ['thickfat', 'owntempo', 'gluttony']).
pokemon_abilities('grumpig', ['thickfat', 'owntempo', 'gluttony']).
pokemon_abilities('spinda', ['owntempo', 'tangledfeet', 'contrary']).
pokemon_abilities('trapinch', ['hypercutter', 'arenatrap', 'sheerforce']).
pokemon_abilities('vibrava', ['levitate']).
pokemon_abilities('flygon', ['levitate']).
pokemon_abilities('cacnea', ['sandveil', 'waterabsorb']).
pokemon_abilities('cacturne', ['sandveil', 'waterabsorb']).
pokemon_abilities('swablu', ['naturalcure', 'cloudnine']).
pokemon_abilities('altaria', ['naturalcure', 'cloudnine']).
pokemon_abilities('altariamega', ['pixilate']).
pokemon_abilities('zangoose', ['immunity', 'toxicboost']).
pokemon_abilities('seviper', ['shedskin', 'infiltrator']).
pokemon_abilities('lunatone', ['levitate']).
pokemon_abilities('solrock', ['levitate']).
pokemon_abilities('barboach', ['oblivious', 'anticipation', 'hydration']).
pokemon_abilities('whiscash', ['oblivious', 'anticipation', 'hydration']).
pokemon_abilities('corphish', ['hypercutter', 'shellarmor', 'adaptability']).
pokemon_abilities('crawdaunt', ['hypercutter', 'shellarmor', 'adaptability']).
pokemon_abilities('baltoy', ['levitate']).
pokemon_abilities('claydol', ['levitate']).
pokemon_abilities('lileep', ['suctioncups', 'stormdrain']).
pokemon_abilities('cradily', ['suctioncups', 'stormdrain']).
pokemon_abilities('anorith', ['battlearmor', 'swiftswim']).
pokemon_abilities('armaldo', ['battlearmor', 'swiftswim']).
pokemon_abilities('feebas', ['swiftswim', 'oblivious', 'adaptability']).
pokemon_abilities('milotic', ['marvelscale', 'competitive', 'cutecharm']).
pokemon_abilities('castform', ['forecast']).
pokemon_abilities('castformsunny', ['forecast']).
pokemon_abilities('castformrainy', ['forecast']).
pokemon_abilities('castformsnowy', ['forecast']).
pokemon_abilities('kecleon', ['colorchange', 'protean']).
pokemon_abilities('shuppet', ['insomnia', 'frisk', 'cursedbody']).
pokemon_abilities('banette', ['insomnia', 'frisk', 'cursedbody']).
pokemon_abilities('banettemega', ['prankster']).
pokemon_abilities('duskull', ['levitate', 'frisk']).
pokemon_abilities('dusclops', ['pressure', 'frisk']).
pokemon_abilities('tropius', ['chlorophyll', 'solarpower', 'harvest']).
pokemon_abilities('chimecho', ['levitate']).
pokemon_abilities('absol', ['pressure', 'superluck', 'justified']).
pokemon_abilities('absolmega', ['magicbounce']).
pokemon_abilities('wynaut', ['shadowtag', 'telepathy']).
pokemon_abilities('snorunt', ['innerfocus', 'icebody', 'moody']).
pokemon_abilities('glalie', ['innerfocus', 'icebody', 'moody']).
pokemon_abilities('glaliemega', ['refrigerate']).
pokemon_abilities('spheal', ['thickfat', 'icebody', 'oblivious']).
pokemon_abilities('sealeo', ['thickfat', 'icebody', 'oblivious']).
pokemon_abilities('walrein', ['thickfat', 'icebody', 'oblivious']).
pokemon_abilities('clamperl', ['shellarmor', 'rattled']).
pokemon_abilities('huntail', ['swiftswim', 'waterveil']).
pokemon_abilities('gorebyss', ['swiftswim', 'hydration']).
pokemon_abilities('relicanth', ['swiftswim', 'rockhead', 'sturdy']).
pokemon_abilities('luvdisc', ['swiftswim', 'hydration']).
pokemon_abilities('bagon', ['rockhead', 'sheerforce']).
pokemon_abilities('shelgon', ['rockhead', 'overcoat']).
pokemon_abilities('salamence', ['intimidate', 'moxie']).
pokemon_abilities('salamencemega', ['aerilate']).
pokemon_abilities('beldum', ['clearbody', 'lightmetal']).
pokemon_abilities('metang', ['clearbody', 'lightmetal']).
pokemon_abilities('metagross', ['clearbody', 'lightmetal']).
pokemon_abilities('metagrossmega', ['toughclaws']).
pokemon_abilities('regirock', ['clearbody', 'sturdy']).
pokemon_abilities('regice', ['clearbody', 'icebody']).
pokemon_abilities('registeel', ['clearbody', 'lightmetal']).
pokemon_abilities('latias', ['levitate']).
pokemon_abilities('latiasmega', ['levitate']).
pokemon_abilities('latios', ['levitate']).
pokemon_abilities('latiosmega', ['levitate']).
pokemon_abilities('kyogre', ['drizzle']).
pokemon_abilities('kyogreprimal', ['primordialsea']).
pokemon_abilities('groudon', ['drought']).
pokemon_abilities('groudonprimal', ['desolateland']).
pokemon_abilities('rayquaza', ['airlock']).
pokemon_abilities('rayquazamega', ['deltastream']).
pokemon_abilities('jirachi', ['serenegrace']).
pokemon_abilities('deoxys', ['pressure']).
pokemon_abilities('deoxysattack', ['pressure']).
pokemon_abilities('deoxysdefense', ['pressure']).
pokemon_abilities('deoxysspeed', ['pressure']).
pokemon_abilities('turtwig', ['overgrow', 'shellarmor']).
pokemon_abilities('grotle', ['overgrow', 'shellarmor']).
pokemon_abilities('torterra', ['overgrow', 'shellarmor']).
pokemon_abilities('chimchar', ['blaze', 'ironfist']).
pokemon_abilities('monferno', ['blaze', 'ironfist']).
pokemon_abilities('infernape', ['blaze', 'ironfist']).
pokemon_abilities('piplup', ['torrent', 'competitive']).
pokemon_abilities('prinplup', ['torrent', 'competitive']).
pokemon_abilities('empoleon', ['torrent', 'competitive']).
pokemon_abilities('starly', ['keeneye', 'reckless']).
pokemon_abilities('staravia', ['intimidate', 'reckless']).
pokemon_abilities('staraptor', ['intimidate', 'reckless']).
pokemon_abilities('bidoof', ['simple', 'unaware', 'moody']).
pokemon_abilities('bibarel', ['simple', 'unaware', 'moody']).
pokemon_abilities('kricketot', ['shedskin', 'runaway']).
pokemon_abilities('kricketune', ['swarm', 'technician']).
pokemon_abilities('shinx', ['rivalry', 'intimidate', 'guts']).
pokemon_abilities('luxio', ['rivalry', 'intimidate', 'guts']).
pokemon_abilities('luxray', ['rivalry', 'intimidate', 'guts']).
pokemon_abilities('budew', ['naturalcure', 'poisonpoint', 'leafguard']).
pokemon_abilities('roserade', ['naturalcure', 'poisonpoint', 'technician']).
pokemon_abilities('cranidos', ['moldbreaker', 'sheerforce']).
pokemon_abilities('rampardos', ['moldbreaker', 'sheerforce']).
pokemon_abilities('shieldon', ['sturdy', 'soundproof']).
pokemon_abilities('bastiodon', ['sturdy', 'soundproof']).
pokemon_abilities('burmy', ['shedskin', 'overcoat']).
pokemon_abilities('burmysandy', ['shedskin', 'overcoat']).
pokemon_abilities('burmytrash', ['shedskin', 'overcoat']).
pokemon_abilities('wormadam', ['anticipation', 'overcoat']).
pokemon_abilities('wormadamsandy', ['anticipation', 'overcoat']).
pokemon_abilities('wormadamtrash', ['anticipation', 'overcoat']).
pokemon_abilities('mothim', ['swarm', 'tintedlens']).
pokemon_abilities('combee', ['honeygather', 'hustle']).
pokemon_abilities('vespiquen', ['pressure', 'unnerve']).
pokemon_abilities('pachirisu', ['runaway', 'pickup', 'voltabsorb']).
pokemon_abilities('buizel', ['swiftswim', 'waterveil']).
pokemon_abilities('floatzel', ['swiftswim', 'waterveil']).
pokemon_abilities('cherubi', ['chlorophyll']).
pokemon_abilities('cherrim', ['flowergift']).
pokemon_abilities('cherrimsunshine', ['flowergift']).
pokemon_abilities('shellos', ['stickyhold', 'stormdrain', 'sandforce']).
pokemon_abilities('shelloseast', ['stickyhold', 'stormdrain', 'sandforce']).
pokemon_abilities('gastrodon', ['stickyhold', 'stormdrain', 'sandforce']).
pokemon_abilities('gastrodoneast', ['stickyhold', 'stormdrain', 'sandforce']).
pokemon_abilities('ambipom', ['technician', 'pickup', 'skilllink']).
pokemon_abilities('drifloon', ['aftermath', 'unburden', 'flareboost']).
pokemon_abilities('drifblim', ['aftermath', 'unburden', 'flareboost']).
pokemon_abilities('buneary', ['runaway', 'klutz', 'limber']).
pokemon_abilities('lopunny', ['cutecharm', 'klutz', 'limber']).
pokemon_abilities('lopunnymega', ['scrappy']).
pokemon_abilities('mismagius', ['levitate']).
pokemon_abilities('honchkrow', ['insomnia', 'superluck', 'moxie']).
pokemon_abilities('glameow', ['limber', 'owntempo', 'keeneye']).
pokemon_abilities('purugly', ['thickfat', 'owntempo', 'defiant']).
pokemon_abilities('chingling', ['levitate']).
pokemon_abilities('stunky', ['stench', 'aftermath', 'keeneye']).
pokemon_abilities('skuntank', ['stench', 'aftermath', 'keeneye']).
pokemon_abilities('bronzor', ['levitate', 'heatproof', 'heavymetal']).
pokemon_abilities('bronzong', ['levitate', 'heatproof', 'heavymetal']).
pokemon_abilities('bonsly', ['sturdy', 'rockhead', 'rattled']).
pokemon_abilities('mimejr', ['soundproof', 'filter', 'technician']).
pokemon_abilities('happiny', ['naturalcure', 'serenegrace', 'friendguard']).
pokemon_abilities('chatot', ['keeneye', 'tangledfeet', 'bigpecks']).
pokemon_abilities('spiritomb', ['pressure', 'infiltrator']).
pokemon_abilities('gible', ['sandveil', 'roughskin']).
pokemon_abilities('gabite', ['sandveil', 'roughskin']).
pokemon_abilities('garchomp', ['sandveil', 'roughskin']).
pokemon_abilities('garchompmega', ['sandforce']).
pokemon_abilities('munchlax', ['pickup', 'thickfat', 'gluttony']).
pokemon_abilities('riolu', ['steadfast', 'innerfocus', 'prankster']).
pokemon_abilities('lucario', ['steadfast', 'innerfocus', 'justified']).
pokemon_abilities('lucariomega', ['adaptability']).
pokemon_abilities('hippopotas', ['sandstream', 'sandforce']).
pokemon_abilities('hippowdon', ['sandstream', 'sandforce']).
pokemon_abilities('skorupi', ['battlearmor', 'sniper', 'keeneye']).
pokemon_abilities('drapion', ['battlearmor', 'sniper', 'keeneye']).
pokemon_abilities('croagunk', ['anticipation', 'dryskin', 'poisontouch']).
pokemon_abilities('toxicroak', ['anticipation', 'dryskin', 'poisontouch']).
pokemon_abilities('carnivine', ['levitate']).
pokemon_abilities('finneon', ['swiftswim', 'stormdrain', 'waterveil']).
pokemon_abilities('lumineon', ['swiftswim', 'stormdrain', 'waterveil']).
pokemon_abilities('mantyke', ['swiftswim', 'waterabsorb', 'waterveil']).
pokemon_abilities('snover', ['snowwarning', 'soundproof']).
pokemon_abilities('abomasnow', ['snowwarning', 'soundproof']).
pokemon_abilities('abomasnowmega', ['snowwarning']).
pokemon_abilities('weavile', ['pressure', 'pickpocket']).
pokemon_abilities('magnezone', ['magnetpull', 'sturdy', 'analytic']).
pokemon_abilities('lickilicky', ['owntempo', 'oblivious', 'cloudnine']).
pokemon_abilities('rhyperior', ['lightningrod', 'solidrock', 'reckless']).
pokemon_abilities('tangrowth', ['chlorophyll', 'leafguard', 'regenerator']).
pokemon_abilities('electivire', ['motordrive', 'vitalspirit']).
pokemon_abilities('magmortar', ['flamebody', 'vitalspirit']).
pokemon_abilities('togekiss', ['hustle', 'serenegrace', 'superluck']).
pokemon_abilities('yanmega', ['speedboost', 'tintedlens', 'frisk']).
pokemon_abilities('leafeon', ['leafguard', 'chlorophyll']).
pokemon_abilities('glaceon', ['snowcloak', 'icebody']).
pokemon_abilities('gliscor', ['hypercutter', 'sandveil', 'poisonheal']).
pokemon_abilities('mamoswine', ['oblivious', 'snowcloak', 'thickfat']).
pokemon_abilities('porygonz', ['adaptability', 'download', 'analytic']).
pokemon_abilities('gallade', ['steadfast', 'sharpness', 'justified']).
pokemon_abilities('gallademega', ['innerfocus']).
pokemon_abilities('probopass', ['sturdy', 'magnetpull', 'sandforce']).
pokemon_abilities('dusknoir', ['pressure', 'frisk']).
pokemon_abilities('froslass', ['snowcloak', 'cursedbody']).
pokemon_abilities('froslassmega', ['']).
pokemon_abilities('rotom', ['levitate']).
pokemon_abilities('rotomheat', ['levitate']).
pokemon_abilities('rotomwash', ['levitate']).
pokemon_abilities('rotomfrost', ['levitate']).
pokemon_abilities('rotomfan', ['levitate']).
pokemon_abilities('rotommow', ['levitate']).
pokemon_abilities('uxie', ['levitate']).
pokemon_abilities('mesprit', ['levitate']).
pokemon_abilities('azelf', ['levitate']).
pokemon_abilities('dialga', ['pressure', 'telepathy']).
pokemon_abilities('dialgaorigin', ['pressure', 'telepathy']).
pokemon_abilities('palkia', ['pressure', 'telepathy']).
pokemon_abilities('palkiaorigin', ['pressure', 'telepathy']).
pokemon_abilities('heatran', ['flashfire', 'flamebody']).
pokemon_abilities('regigigas', ['slowstart']).
pokemon_abilities('giratina', ['pressure', 'telepathy']).
pokemon_abilities('giratinaorigin', ['levitate']).
pokemon_abilities('cresselia', ['levitate']).
pokemon_abilities('phione', ['hydration']).
pokemon_abilities('manaphy', ['hydration']).
pokemon_abilities('darkrai', ['baddreams']).
pokemon_abilities('shaymin', ['naturalcure']).
pokemon_abilities('shayminsky', ['serenegrace']).
pokemon_abilities('arceus', ['multitype']).
pokemon_abilities('arceusbug', ['multitype']).
pokemon_abilities('arceusdark', ['multitype']).
pokemon_abilities('arceusdragon', ['multitype']).
pokemon_abilities('arceuselectric', ['multitype']).
pokemon_abilities('arceusfairy', ['multitype']).
pokemon_abilities('arceusfighting', ['multitype']).
pokemon_abilities('arceusfire', ['multitype']).
pokemon_abilities('arceusflying', ['multitype']).
pokemon_abilities('arceusghost', ['multitype']).
pokemon_abilities('arceusgrass', ['multitype']).
pokemon_abilities('arceusground', ['multitype']).
pokemon_abilities('arceusice', ['multitype']).
pokemon_abilities('arceuspoison', ['multitype']).
pokemon_abilities('arceuspsychic', ['multitype']).
pokemon_abilities('arceusrock', ['multitype']).
pokemon_abilities('arceussteel', ['multitype']).
pokemon_abilities('arceuswater', ['multitype']).
pokemon_abilities('victini', ['victorystar']).
pokemon_abilities('snivy', ['overgrow', 'contrary']).
pokemon_abilities('servine', ['overgrow', 'contrary']).
pokemon_abilities('serperior', ['overgrow', 'contrary']).
pokemon_abilities('tepig', ['blaze', 'thickfat']).
pokemon_abilities('pignite', ['blaze', 'thickfat']).
pokemon_abilities('emboar', ['blaze', 'reckless']).
pokemon_abilities('emboarmega', ['']).
pokemon_abilities('oshawott', ['torrent', 'shellarmor']).
pokemon_abilities('dewott', ['torrent', 'shellarmor']).
pokemon_abilities('samurott', ['torrent', 'shellarmor']).
pokemon_abilities('samurotthisui', ['torrent', 'sharpness']).
pokemon_abilities('patrat', ['runaway', 'keeneye', 'analytic']).
pokemon_abilities('watchog', ['illuminate', 'keeneye', 'analytic']).
pokemon_abilities('lillipup', ['vitalspirit', 'pickup', 'runaway']).
pokemon_abilities('herdier', ['intimidate', 'sandrush', 'scrappy']).
pokemon_abilities('stoutland', ['intimidate', 'sandrush', 'scrappy']).
pokemon_abilities('purrloin', ['limber', 'unburden', 'prankster']).
pokemon_abilities('liepard', ['limber', 'unburden', 'prankster']).
pokemon_abilities('pansage', ['gluttony', 'overgrow']).
pokemon_abilities('simisage', ['gluttony', 'overgrow']).
pokemon_abilities('pansear', ['gluttony', 'blaze']).
pokemon_abilities('simisear', ['gluttony', 'blaze']).
pokemon_abilities('panpour', ['gluttony', 'torrent']).
pokemon_abilities('simipour', ['gluttony', 'torrent']).
pokemon_abilities('munna', ['forewarn', 'synchronize', 'telepathy']).
pokemon_abilities('musharna', ['forewarn', 'synchronize', 'telepathy']).
pokemon_abilities('pidove', ['bigpecks', 'superluck', 'rivalry']).
pokemon_abilities('tranquill', ['bigpecks', 'superluck', 'rivalry']).
pokemon_abilities('unfezant', ['bigpecks', 'superluck', 'rivalry']).
pokemon_abilities('blitzle', ['lightningrod', 'motordrive', 'sapsipper']).
pokemon_abilities('zebstrika', ['lightningrod', 'motordrive', 'sapsipper']).
pokemon_abilities('roggenrola', ['sturdy', 'weakarmor', 'sandforce']).
pokemon_abilities('boldore', ['sturdy', 'weakarmor', 'sandforce']).
pokemon_abilities('gigalith', ['sturdy', 'sandstream', 'sandforce']).
pokemon_abilities('woobat', ['unaware', 'klutz', 'simple']).
pokemon_abilities('swoobat', ['unaware', 'klutz', 'simple']).
pokemon_abilities('drilbur', ['sandrush', 'sandforce', 'moldbreaker']).
pokemon_abilities('excadrill', ['sandrush', 'sandforce', 'moldbreaker']).
pokemon_abilities('excadrillmega', ['']).
pokemon_abilities('audino', ['healer', 'regenerator', 'klutz']).
pokemon_abilities('audinomega', ['healer']).
pokemon_abilities('timburr', ['guts', 'sheerforce', 'ironfist']).
pokemon_abilities('gurdurr', ['guts', 'sheerforce', 'ironfist']).
pokemon_abilities('conkeldurr', ['guts', 'sheerforce', 'ironfist']).
pokemon_abilities('tympole', ['swiftswim', 'hydration', 'waterabsorb']).
pokemon_abilities('palpitoad', ['swiftswim', 'hydration', 'waterabsorb']).
pokemon_abilities('seismitoad', ['swiftswim', 'poisontouch', 'waterabsorb']).
pokemon_abilities('throh', ['guts', 'innerfocus', 'moldbreaker']).
pokemon_abilities('sawk', ['sturdy', 'innerfocus', 'moldbreaker']).
pokemon_abilities('sewaddle', ['swarm', 'chlorophyll', 'overcoat']).
pokemon_abilities('swadloon', ['leafguard', 'chlorophyll', 'overcoat']).
pokemon_abilities('leavanny', ['swarm', 'chlorophyll', 'overcoat']).
pokemon_abilities('venipede', ['poisonpoint', 'swarm', 'speedboost']).
pokemon_abilities('whirlipede', ['poisonpoint', 'swarm', 'speedboost']).
pokemon_abilities('scolipede', ['poisonpoint', 'swarm', 'speedboost']).
pokemon_abilities('scolipedemega', ['']).
pokemon_abilities('cottonee', ['prankster', 'infiltrator', 'chlorophyll']).
pokemon_abilities('whimsicott', ['prankster', 'infiltrator', 'chlorophyll']).
pokemon_abilities('petilil', ['chlorophyll', 'owntempo', 'leafguard']).
pokemon_abilities('lilligant', ['chlorophyll', 'owntempo', 'leafguard']).
pokemon_abilities('lilliganthisui', ['chlorophyll', 'hustle', 'leafguard']).
pokemon_abilities('basculin', ['reckless', 'adaptability', 'moldbreaker']).
pokemon_abilities('basculinbluestriped', ['rockhead', 'adaptability', 'moldbreaker']).
pokemon_abilities('basculinwhitestriped', ['rattled', 'adaptability', 'moldbreaker']).
pokemon_abilities('sandile', ['intimidate', 'moxie', 'angerpoint']).
pokemon_abilities('krokorok', ['intimidate', 'moxie', 'angerpoint']).
pokemon_abilities('krookodile', ['intimidate', 'moxie', 'angerpoint']).
pokemon_abilities('darumaka', ['hustle', 'innerfocus']).
pokemon_abilities('darumakagalar', ['hustle', 'innerfocus']).
pokemon_abilities('darmanitan', ['sheerforce', 'zenmode']).
pokemon_abilities('darmanitanzen', ['zenmode']).
pokemon_abilities('darmanitangalar', ['gorillatactics', 'zenmode']).
pokemon_abilities('darmanitangalarzen', ['zenmode']).
pokemon_abilities('maractus', ['waterabsorb', 'chlorophyll', 'stormdrain']).
pokemon_abilities('dwebble', ['sturdy', 'shellarmor', 'weakarmor']).
pokemon_abilities('crustle', ['sturdy', 'shellarmor', 'weakarmor']).
pokemon_abilities('scraggy', ['shedskin', 'moxie', 'intimidate']).
pokemon_abilities('scrafty', ['shedskin', 'moxie', 'intimidate']).
pokemon_abilities('scraftymega', ['']).
pokemon_abilities('sigilyph', ['wonderskin', 'magicguard', 'tintedlens']).
pokemon_abilities('yamask', ['mummy']).
pokemon_abilities('yamaskgalar', ['wanderingspirit']).
pokemon_abilities('cofagrigus', ['mummy']).
pokemon_abilities('tirtouga', ['solidrock', 'sturdy', 'swiftswim']).
pokemon_abilities('carracosta', ['solidrock', 'sturdy', 'swiftswim']).
pokemon_abilities('archen', ['defeatist']).
pokemon_abilities('archeops', ['defeatist']).
pokemon_abilities('trubbish', ['stench', 'stickyhold', 'aftermath']).
pokemon_abilities('garbodor', ['stench', 'weakarmor', 'aftermath']).
pokemon_abilities('garbodorgmax', ['stench', 'weakarmor', 'aftermath']).
pokemon_abilities('zorua', ['illusion']).
pokemon_abilities('zoruahisui', ['illusion']).
pokemon_abilities('zoroark', ['illusion']).
pokemon_abilities('zoroarkhisui', ['illusion']).
pokemon_abilities('minccino', ['cutecharm', 'technician', 'skilllink']).
pokemon_abilities('cinccino', ['cutecharm', 'technician', 'skilllink']).
pokemon_abilities('gothita', ['frisk', 'competitive', 'shadowtag']).
pokemon_abilities('gothorita', ['frisk', 'competitive', 'shadowtag']).
pokemon_abilities('gothitelle', ['frisk', 'competitive', 'shadowtag']).
pokemon_abilities('solosis', ['overcoat', 'magicguard', 'regenerator']).
pokemon_abilities('duosion', ['overcoat', 'magicguard', 'regenerator']).
pokemon_abilities('reuniclus', ['overcoat', 'magicguard', 'regenerator']).
pokemon_abilities('ducklett', ['keeneye', 'bigpecks', 'hydration']).
pokemon_abilities('swanna', ['keeneye', 'bigpecks', 'hydration']).
pokemon_abilities('vanillite', ['icebody', 'snowcloak', 'weakarmor']).
pokemon_abilities('vanillish', ['icebody', 'snowcloak', 'weakarmor']).
pokemon_abilities('vanilluxe', ['icebody', 'snowwarning', 'weakarmor']).
pokemon_abilities('deerling', ['chlorophyll', 'sapsipper', 'serenegrace']).
pokemon_abilities('deerlingsummer', ['chlorophyll', 'sapsipper', 'serenegrace']).
pokemon_abilities('deerlingautumn', ['chlorophyll', 'sapsipper', 'serenegrace']).
pokemon_abilities('deerlingwinter', ['chlorophyll', 'sapsipper', 'serenegrace']).
pokemon_abilities('sawsbuck', ['chlorophyll', 'sapsipper', 'serenegrace']).
pokemon_abilities('emolga', ['static', 'motordrive']).
pokemon_abilities('karrablast', ['swarm', 'shedskin', 'noguard']).
pokemon_abilities('escavalier', ['swarm', 'shellarmor', 'overcoat']).
pokemon_abilities('foongus', ['effectspore', 'regenerator']).
pokemon_abilities('amoonguss', ['effectspore', 'regenerator']).
pokemon_abilities('frillish', ['waterabsorb', 'cursedbody', 'damp']).
pokemon_abilities('jellicent', ['waterabsorb', 'cursedbody', 'damp']).
pokemon_abilities('alomomola', ['healer', 'hydration', 'regenerator']).
pokemon_abilities('joltik', ['compoundeyes', 'unnerve', 'swarm']).
pokemon_abilities('galvantula', ['compoundeyes', 'unnerve', 'swarm']).
pokemon_abilities('ferroseed', ['ironbarbs']).
pokemon_abilities('ferrothorn', ['ironbarbs', 'anticipation']).
pokemon_abilities('klink', ['plus', 'minus', 'clearbody']).
pokemon_abilities('klang', ['plus', 'minus', 'clearbody']).
pokemon_abilities('klinklang', ['plus', 'minus', 'clearbody']).
pokemon_abilities('tynamo', ['levitate']).
pokemon_abilities('eelektrik', ['levitate']).
pokemon_abilities('eelektross', ['levitate']).
pokemon_abilities('eelektrossmega', ['']).
pokemon_abilities('elgyem', ['telepathy', 'synchronize', 'analytic']).
pokemon_abilities('beheeyem', ['telepathy', 'synchronize', 'analytic']).
pokemon_abilities('litwick', ['flashfire', 'flamebody', 'infiltrator']).
pokemon_abilities('lampent', ['flashfire', 'flamebody', 'infiltrator']).
pokemon_abilities('chandelure', ['flashfire', 'flamebody', 'infiltrator']).
pokemon_abilities('chandeluremega', ['']).
pokemon_abilities('axew', ['rivalry', 'moldbreaker', 'unnerve']).
pokemon_abilities('fraxure', ['rivalry', 'moldbreaker', 'unnerve']).
pokemon_abilities('haxorus', ['rivalry', 'moldbreaker', 'unnerve']).
pokemon_abilities('cubchoo', ['snowcloak', 'slushrush', 'rattled']).
pokemon_abilities('beartic', ['snowcloak', 'slushrush', 'swiftswim']).
pokemon_abilities('cryogonal', ['levitate']).
pokemon_abilities('shelmet', ['hydration', 'shellarmor', 'overcoat']).
pokemon_abilities('accelgor', ['hydration', 'stickyhold', 'unburden']).
pokemon_abilities('stunfisk', ['static', 'limber', 'sandveil']).
pokemon_abilities('stunfiskgalar', ['mimicry']).
pokemon_abilities('mienfoo', ['innerfocus', 'regenerator', 'reckless']).
pokemon_abilities('mienshao', ['innerfocus', 'regenerator', 'reckless']).
pokemon_abilities('druddigon', ['roughskin', 'sheerforce', 'moldbreaker']).
pokemon_abilities('golett', ['ironfist', 'klutz', 'noguard']).
pokemon_abilities('golurk', ['ironfist', 'klutz', 'noguard']).
pokemon_abilities('pawniard', ['defiant', 'innerfocus', 'pressure']).
pokemon_abilities('bisharp', ['defiant', 'innerfocus', 'pressure']).
pokemon_abilities('bouffalant', ['reckless', 'sapsipper', 'soundproof']).
pokemon_abilities('rufflet', ['keeneye', 'sheerforce', 'hustle']).
pokemon_abilities('braviary', ['keeneye', 'sheerforce', 'defiant']).
pokemon_abilities('braviaryhisui', ['keeneye', 'sheerforce', 'tintedlens']).
pokemon_abilities('vullaby', ['bigpecks', 'overcoat', 'weakarmor']).
pokemon_abilities('mandibuzz', ['bigpecks', 'overcoat', 'weakarmor']).
pokemon_abilities('heatmor', ['gluttony', 'flashfire', 'whitesmoke']).
pokemon_abilities('durant', ['swarm', 'hustle', 'truant']).
pokemon_abilities('deino', ['hustle']).
pokemon_abilities('zweilous', ['hustle']).
pokemon_abilities('hydreigon', ['levitate']).
pokemon_abilities('larvesta', ['flamebody', 'swarm']).
pokemon_abilities('volcarona', ['flamebody', 'swarm']).
pokemon_abilities('cobalion', ['justified']).
pokemon_abilities('terrakion', ['justified']).
pokemon_abilities('virizion', ['justified']).
pokemon_abilities('tornadus', ['prankster', 'defiant']).
pokemon_abilities('tornadustherian', ['regenerator']).
pokemon_abilities('thundurus', ['prankster', 'defiant']).
pokemon_abilities('thundurustherian', ['voltabsorb']).
pokemon_abilities('reshiram', ['turboblaze']).
pokemon_abilities('zekrom', ['teravolt']).
pokemon_abilities('landorus', ['sandforce', 'sheerforce']).
pokemon_abilities('landorustherian', ['intimidate']).
pokemon_abilities('kyurem', ['pressure']).
pokemon_abilities('kyuremblack', ['teravolt']).
pokemon_abilities('kyuremwhite', ['turboblaze']).
pokemon_abilities('keldeo', ['justified']).
pokemon_abilities('keldeoresolute', ['justified']).
pokemon_abilities('meloetta', ['serenegrace']).
pokemon_abilities('meloettapirouette', ['serenegrace']).
pokemon_abilities('genesect', ['download']).
pokemon_abilities('genesectdouse', ['download']).
pokemon_abilities('genesectshock', ['download']).
pokemon_abilities('genesectburn', ['download']).
pokemon_abilities('genesectchill', ['download']).
pokemon_abilities('chespin', ['overgrow', 'bulletproof']).
pokemon_abilities('quilladin', ['overgrow', 'bulletproof']).
pokemon_abilities('chesnaught', ['overgrow', 'bulletproof']).
pokemon_abilities('chesnaughtmega', ['']).
pokemon_abilities('fennekin', ['blaze', 'magician']).
pokemon_abilities('braixen', ['blaze', 'magician']).
pokemon_abilities('delphox', ['blaze', 'magician']).
pokemon_abilities('delphoxmega', ['']).
pokemon_abilities('froakie', ['torrent', 'protean']).
pokemon_abilities('frogadier', ['torrent', 'protean']).
pokemon_abilities('greninja', ['torrent', 'protean', 'battlebond']).
pokemon_abilities('greninjabond', ['battlebond']).
pokemon_abilities('greninjaash', ['battlebond']).
pokemon_abilities('greninjamega', ['']).
pokemon_abilities('bunnelby', ['pickup', 'cheekpouch', 'hugepower']).
pokemon_abilities('diggersby', ['pickup', 'cheekpouch', 'hugepower']).
pokemon_abilities('fletchling', ['bigpecks', 'galewings']).
pokemon_abilities('fletchinder', ['flamebody', 'galewings']).
pokemon_abilities('talonflame', ['flamebody', 'galewings']).
pokemon_abilities('scatterbug', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('spewpa', ['shedskin', 'friendguard']).
pokemon_abilities('vivillon', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonicysnow', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonpolar', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillontundra', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivilloncontinental', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillongarden', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonelegant', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonmodern', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonmarine', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonarchipelago', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonhighplains', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonsandstorm', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonriver', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonmonsoon', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonsavanna', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonsun', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonocean', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonjungle', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonfancy', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('vivillonpokeball', ['shielddust', 'compoundeyes', 'friendguard']).
pokemon_abilities('litleo', ['rivalry', 'unnerve', 'moxie']).
pokemon_abilities('pyroar', ['rivalry', 'unnerve', 'moxie']).
pokemon_abilities('pyroarmega', ['']).
pokemon_abilities('flabebe', ['flowerveil', 'symbiosis']).
pokemon_abilities('floette', ['flowerveil', 'symbiosis']).
pokemon_abilities('floetteeternal', ['flowerveil']).
pokemon_abilities('floettemega', ['']).
pokemon_abilities('florges', ['flowerveil', 'symbiosis']).
pokemon_abilities('skiddo', ['sapsipper', 'grasspelt']).
pokemon_abilities('gogoat', ['sapsipper', 'grasspelt']).
pokemon_abilities('pancham', ['ironfist', 'moldbreaker', 'scrappy']).
pokemon_abilities('pangoro', ['ironfist', 'moldbreaker', 'scrappy']).
pokemon_abilities('furfrou', ['furcoat']).
pokemon_abilities('espurr', ['keeneye', 'infiltrator', 'owntempo']).
pokemon_abilities('meowstic', ['keeneye', 'infiltrator', 'prankster']).
pokemon_abilities('meowsticf', ['keeneye', 'infiltrator', 'competitive']).
pokemon_abilities('honedge', ['noguard']).
pokemon_abilities('doublade', ['noguard']).
pokemon_abilities('aegislash', ['stancechange']).
pokemon_abilities('aegislashblade', ['stancechange']).
pokemon_abilities('spritzee', ['healer', 'aromaveil']).
pokemon_abilities('aromatisse', ['healer', 'aromaveil']).
pokemon_abilities('swirlix', ['sweetveil', 'unburden']).
pokemon_abilities('slurpuff', ['sweetveil', 'unburden']).
pokemon_abilities('inkay', ['contrary', 'suctioncups', 'infiltrator']).
pokemon_abilities('malamar', ['contrary', 'suctioncups', 'infiltrator']).
pokemon_abilities('malamarmega', ['']).
pokemon_abilities('binacle', ['toughclaws', 'sniper', 'pickpocket']).
pokemon_abilities('barbaracle', ['toughclaws', 'sniper', 'pickpocket']).
pokemon_abilities('barbaraclemega', ['']).
pokemon_abilities('skrelp', ['poisonpoint', 'poisontouch', 'adaptability']).
pokemon_abilities('dragalge', ['poisonpoint', 'poisontouch', 'adaptability']).
pokemon_abilities('dragalgemega', ['']).
pokemon_abilities('clauncher', ['megalauncher']).
pokemon_abilities('clawitzer', ['megalauncher']).
pokemon_abilities('helioptile', ['dryskin', 'sandveil', 'solarpower']).
pokemon_abilities('heliolisk', ['dryskin', 'sandveil', 'solarpower']).
pokemon_abilities('tyrunt', ['strongjaw', 'sturdy']).
pokemon_abilities('tyrantrum', ['strongjaw', 'rockhead']).
pokemon_abilities('amaura', ['refrigerate', 'snowwarning']).
pokemon_abilities('aurorus', ['refrigerate', 'snowwarning']).
pokemon_abilities('sylveon', ['cutecharm', 'pixilate']).
pokemon_abilities('hawlucha', ['limber', 'unburden', 'moldbreaker']).
pokemon_abilities('hawluchamega', ['']).
pokemon_abilities('dedenne', ['cheekpouch', 'pickup', 'plus']).
pokemon_abilities('carbink', ['clearbody', 'sturdy']).
pokemon_abilities('goomy', ['sapsipper', 'hydration', 'gooey']).
pokemon_abilities('sliggoo', ['sapsipper', 'hydration', 'gooey']).
pokemon_abilities('sliggoohisui', ['sapsipper', 'shellarmor', 'gooey']).
pokemon_abilities('goodra', ['sapsipper', 'hydration', 'gooey']).
pokemon_abilities('goodrahisui', ['sapsipper', 'shellarmor', 'gooey']).
pokemon_abilities('klefki', ['prankster', 'magician']).
pokemon_abilities('phantump', ['naturalcure', 'frisk', 'harvest']).
pokemon_abilities('trevenant', ['naturalcure', 'frisk', 'harvest']).
pokemon_abilities('pumpkaboo', ['pickup', 'frisk', 'insomnia']).
pokemon_abilities('pumpkaboosmall', ['pickup', 'frisk', 'insomnia']).
pokemon_abilities('pumpkaboolarge', ['pickup', 'frisk', 'insomnia']).
pokemon_abilities('pumpkaboosuper', ['pickup', 'frisk', 'insomnia']).
pokemon_abilities('gourgeist', ['pickup', 'frisk', 'insomnia']).
pokemon_abilities('gourgeistsmall', ['pickup', 'frisk', 'insomnia']).
pokemon_abilities('gourgeistlarge', ['pickup', 'frisk', 'insomnia']).
pokemon_abilities('gourgeistsuper', ['pickup', 'frisk', 'insomnia']).
pokemon_abilities('bergmite', ['owntempo', 'icebody', 'sturdy']).
pokemon_abilities('avalugg', ['owntempo', 'icebody', 'sturdy']).
pokemon_abilities('avalugghisui', ['strongjaw', 'icebody', 'sturdy']).
pokemon_abilities('noibat', ['frisk', 'infiltrator', 'telepathy']).
pokemon_abilities('noivern', ['frisk', 'infiltrator', 'telepathy']).
pokemon_abilities('xerneas', ['fairyaura']).
pokemon_abilities('xerneasneutral', ['fairyaura']).
pokemon_abilities('yveltal', ['darkaura']).
pokemon_abilities('zygarde', ['aurabreak', 'powerconstruct']).
pokemon_abilities('zygarde10', ['aurabreak', 'powerconstruct']).
pokemon_abilities('zygardecomplete', ['powerconstruct']).
pokemon_abilities('zygardemega', ['']).
pokemon_abilities('diancie', ['clearbody']).
pokemon_abilities('dianciemega', ['magicbounce']).
pokemon_abilities('hoopa', ['magician']).
pokemon_abilities('hoopaunbound', ['magician']).
pokemon_abilities('volcanion', ['waterabsorb']).
pokemon_abilities('rowlet', ['overgrow', 'longreach']).
pokemon_abilities('dartrix', ['overgrow', 'longreach']).
pokemon_abilities('decidueye', ['overgrow', 'longreach']).
pokemon_abilities('decidueyehisui', ['overgrow', 'scrappy']).
pokemon_abilities('litten', ['blaze', 'intimidate']).
pokemon_abilities('torracat', ['blaze', 'intimidate']).
pokemon_abilities('incineroar', ['blaze', 'intimidate']).
pokemon_abilities('popplio', ['torrent', 'liquidvoice']).
pokemon_abilities('brionne', ['torrent', 'liquidvoice']).
pokemon_abilities('primarina', ['torrent', 'liquidvoice']).
pokemon_abilities('pikipek', ['keeneye', 'skilllink', 'pickup']).
pokemon_abilities('trumbeak', ['keeneye', 'skilllink', 'pickup']).
pokemon_abilities('toucannon', ['keeneye', 'skilllink', 'sheerforce']).
pokemon_abilities('yungoos', ['stakeout', 'strongjaw', 'adaptability']).
pokemon_abilities('gumshoos', ['stakeout', 'strongjaw', 'adaptability']).
pokemon_abilities('gumshoostotem', ['adaptability']).
pokemon_abilities('grubbin', ['swarm']).
pokemon_abilities('charjabug', ['battery']).
pokemon_abilities('vikavolt', ['levitate']).
pokemon_abilities('vikavolttotem', ['levitate']).
pokemon_abilities('crabrawler', ['hypercutter', 'ironfist', 'angerpoint']).
pokemon_abilities('crabominable', ['hypercutter', 'ironfist', 'angerpoint']).
pokemon_abilities('oricorio', ['dancer']).
pokemon_abilities('oricoriopompom', ['dancer']).
pokemon_abilities('oricoriopau', ['dancer']).
pokemon_abilities('oricoriosensu', ['dancer']).
pokemon_abilities('cutiefly', ['honeygather', 'shielddust', 'sweetveil']).
pokemon_abilities('ribombee', ['honeygather', 'shielddust', 'sweetveil']).
pokemon_abilities('ribombeetotem', ['sweetveil']).
pokemon_abilities('rockruff', ['keeneye', 'vitalspirit', 'steadfast', 'owntempo']).
pokemon_abilities('rockruffdusk', ['owntempo']).
pokemon_abilities('lycanroc', ['keeneye', 'sandrush', 'steadfast']).
pokemon_abilities('lycanrocmidnight', ['keeneye', 'vitalspirit', 'noguard']).
pokemon_abilities('lycanrocdusk', ['toughclaws']).
pokemon_abilities('wishiwashi', ['schooling']).
pokemon_abilities('wishiwashischool', ['schooling']).
pokemon_abilities('mareanie', ['merciless', 'limber', 'regenerator']).
pokemon_abilities('toxapex', ['merciless', 'limber', 'regenerator']).
pokemon_abilities('mudbray', ['owntempo', 'stamina', 'innerfocus']).
pokemon_abilities('mudsdale', ['owntempo', 'stamina', 'innerfocus']).
pokemon_abilities('dewpider', ['waterbubble', 'waterabsorb']).
pokemon_abilities('araquanid', ['waterbubble', 'waterabsorb']).
pokemon_abilities('araquanidtotem', ['waterbubble']).
pokemon_abilities('fomantis', ['leafguard', 'contrary']).
pokemon_abilities('lurantis', ['leafguard', 'contrary']).
pokemon_abilities('lurantistotem', ['leafguard']).
pokemon_abilities('morelull', ['illuminate', 'effectspore', 'raindish']).
pokemon_abilities('shiinotic', ['illuminate', 'effectspore', 'raindish']).
pokemon_abilities('salandit', ['corrosion', 'oblivious']).
pokemon_abilities('salazzle', ['corrosion', 'oblivious']).
pokemon_abilities('salazzletotem', ['corrosion']).
pokemon_abilities('stufful', ['fluffy', 'klutz', 'cutecharm']).
pokemon_abilities('bewear', ['fluffy', 'klutz', 'unnerve']).
pokemon_abilities('bounsweet', ['leafguard', 'oblivious', 'sweetveil']).
pokemon_abilities('steenee', ['leafguard', 'oblivious', 'sweetveil']).
pokemon_abilities('tsareena', ['leafguard', 'queenlymajesty', 'sweetveil']).
pokemon_abilities('comfey', ['flowerveil', 'triage', 'naturalcure']).
pokemon_abilities('oranguru', ['innerfocus', 'telepathy', 'symbiosis']).
pokemon_abilities('passimian', ['receiver', 'defiant']).
pokemon_abilities('wimpod', ['wimpout']).
pokemon_abilities('golisopod', ['emergencyexit']).
pokemon_abilities('sandygast', ['watercompaction', 'sandveil']).
pokemon_abilities('palossand', ['watercompaction', 'sandveil']).
pokemon_abilities('pyukumuku', ['innardsout', 'unaware']).
pokemon_abilities('typenull', ['battlearmor']).
pokemon_abilities('silvally', ['rkssystem']).
pokemon_abilities('silvallybug', ['rkssystem']).
pokemon_abilities('silvallydark', ['rkssystem']).
pokemon_abilities('silvallydragon', ['rkssystem']).
pokemon_abilities('silvallyelectric', ['rkssystem']).
pokemon_abilities('silvallyfairy', ['rkssystem']).
pokemon_abilities('silvallyfighting', ['rkssystem']).
pokemon_abilities('silvallyfire', ['rkssystem']).
pokemon_abilities('silvallyflying', ['rkssystem']).
pokemon_abilities('silvallyghost', ['rkssystem']).
pokemon_abilities('silvallygrass', ['rkssystem']).
pokemon_abilities('silvallyground', ['rkssystem']).
pokemon_abilities('silvallyice', ['rkssystem']).
pokemon_abilities('silvallypoison', ['rkssystem']).
pokemon_abilities('silvallypsychic', ['rkssystem']).
pokemon_abilities('silvallyrock', ['rkssystem']).
pokemon_abilities('silvallysteel', ['rkssystem']).
pokemon_abilities('silvallywater', ['rkssystem']).
pokemon_abilities('minior', ['shieldsdown']).
pokemon_abilities('miniororange', ['shieldsdown']).
pokemon_abilities('minioryellow', ['shieldsdown']).
pokemon_abilities('miniorgreen', ['shieldsdown']).
pokemon_abilities('miniorblue', ['shieldsdown']).
pokemon_abilities('miniorindigo', ['shieldsdown']).
pokemon_abilities('miniorviolet', ['shieldsdown']).
pokemon_abilities('miniormeteor', ['shieldsdown']).
pokemon_abilities('komala', ['comatose']).
pokemon_abilities('turtonator', ['shellarmor']).
pokemon_abilities('togedemaru', ['ironbarbs', 'lightningrod', 'sturdy']).
pokemon_abilities('togedemarutotem', ['sturdy']).
pokemon_abilities('mimikyu', ['disguise']).
pokemon_abilities('mimikyubusted', ['disguise']).
pokemon_abilities('mimikyutotem', ['disguise']).
pokemon_abilities('mimikyubustedtotem', ['disguise']).
pokemon_abilities('bruxish', ['dazzling', 'strongjaw', 'wonderskin']).
pokemon_abilities('drampa', ['berserk', 'sapsipper', 'cloudnine']).
pokemon_abilities('drampamega', ['']).
pokemon_abilities('dhelmise', ['steelworker']).
pokemon_abilities('jangmoo', ['bulletproof', 'soundproof', 'overcoat']).
pokemon_abilities('hakamoo', ['bulletproof', 'soundproof', 'overcoat']).
pokemon_abilities('kommoo', ['bulletproof', 'soundproof', 'overcoat']).
pokemon_abilities('kommoototem', ['overcoat']).
pokemon_abilities('tapukoko', ['electricsurge', 'telepathy']).
pokemon_abilities('tapulele', ['psychicsurge', 'telepathy']).
pokemon_abilities('tapubulu', ['grassysurge', 'telepathy']).
pokemon_abilities('tapufini', ['mistysurge', 'telepathy']).
pokemon_abilities('cosmog', ['unaware']).
pokemon_abilities('cosmoem', ['sturdy']).
pokemon_abilities('solgaleo', ['fullmetalbody']).
pokemon_abilities('lunala', ['shadowshield']).
pokemon_abilities('nihilego', ['beastboost']).
pokemon_abilities('buzzwole', ['beastboost']).
pokemon_abilities('pheromosa', ['beastboost']).
pokemon_abilities('xurkitree', ['beastboost']).
pokemon_abilities('celesteela', ['beastboost']).
pokemon_abilities('kartana', ['beastboost']).
pokemon_abilities('guzzlord', ['beastboost']).
pokemon_abilities('necrozma', ['prismarmor']).
pokemon_abilities('necrozmaduskmane', ['prismarmor']).
pokemon_abilities('necrozmadawnwings', ['prismarmor']).
pokemon_abilities('necrozmaultra', ['neuroforce']).
pokemon_abilities('magearna', ['soul-heart']).
pokemon_abilities('magearnaoriginal', ['soul-heart']).
pokemon_abilities('marshadow', ['technician']).
pokemon_abilities('poipole', ['beastboost']).
pokemon_abilities('naganadel', ['beastboost']).
pokemon_abilities('stakataka', ['beastboost']).
pokemon_abilities('blacephalon', ['beastboost']).
pokemon_abilities('zeraora', ['voltabsorb']).
pokemon_abilities('meltan', ['magnetpull']).
pokemon_abilities('melmetal', ['ironfist']).
pokemon_abilities('melmetalgmax', ['ironfist']).
pokemon_abilities('grookey', ['overgrow', 'grassysurge']).
pokemon_abilities('thwackey', ['overgrow', 'grassysurge']).
pokemon_abilities('rillaboom', ['overgrow', 'grassysurge']).
pokemon_abilities('rillaboomgmax', ['overgrow', 'grassysurge']).
pokemon_abilities('scorbunny', ['blaze', 'libero']).
pokemon_abilities('raboot', ['blaze', 'libero']).
pokemon_abilities('cinderace', ['blaze', 'libero']).
pokemon_abilities('cinderacegmax', ['blaze', 'libero']).
pokemon_abilities('sobble', ['torrent', 'sniper']).
pokemon_abilities('drizzile', ['torrent', 'sniper']).
pokemon_abilities('inteleon', ['torrent', 'sniper']).
pokemon_abilities('inteleongmax', ['torrent', 'sniper']).
pokemon_abilities('skwovet', ['cheekpouch', 'gluttony']).
pokemon_abilities('greedent', ['cheekpouch', 'gluttony']).
pokemon_abilities('rookidee', ['keeneye', 'unnerve', 'bigpecks']).
pokemon_abilities('corvisquire', ['keeneye', 'unnerve', 'bigpecks']).
pokemon_abilities('corviknight', ['pressure', 'unnerve', 'mirrorarmor']).
pokemon_abilities('corviknightgmax', ['pressure', 'unnerve', 'mirrorarmor']).
pokemon_abilities('blipbug', ['swarm', 'compoundeyes', 'telepathy']).
pokemon_abilities('dottler', ['swarm', 'compoundeyes', 'telepathy']).
pokemon_abilities('orbeetle', ['swarm', 'frisk', 'telepathy']).
pokemon_abilities('orbeetlegmax', ['swarm', 'frisk', 'telepathy']).
pokemon_abilities('nickit', ['runaway', 'unburden', 'stakeout']).
pokemon_abilities('thievul', ['runaway', 'unburden', 'stakeout']).
pokemon_abilities('gossifleur', ['cottondown', 'regenerator', 'effectspore']).
pokemon_abilities('eldegoss', ['cottondown', 'regenerator', 'effectspore']).
pokemon_abilities('wooloo', ['fluffy', 'runaway', 'bulletproof']).
pokemon_abilities('dubwool', ['fluffy', 'steadfast', 'bulletproof']).
pokemon_abilities('chewtle', ['strongjaw', 'shellarmor', 'swiftswim']).
pokemon_abilities('drednaw', ['strongjaw', 'shellarmor', 'swiftswim']).
pokemon_abilities('drednawgmax', ['strongjaw', 'shellarmor', 'swiftswim']).
pokemon_abilities('yamper', ['ballfetch', 'rattled']).
pokemon_abilities('boltund', ['strongjaw', 'competitive']).
pokemon_abilities('rolycoly', ['steamengine', 'heatproof', 'flashfire']).
pokemon_abilities('carkol', ['steamengine', 'flamebody', 'flashfire']).
pokemon_abilities('coalossal', ['steamengine', 'flamebody', 'flashfire']).
pokemon_abilities('coalossalgmax', ['steamengine', 'flamebody', 'flashfire']).
pokemon_abilities('applin', ['ripen', 'gluttony', 'bulletproof']).
pokemon_abilities('flapple', ['ripen', 'gluttony', 'hustle']).
pokemon_abilities('flapplegmax', ['ripen', 'gluttony', 'hustle']).
pokemon_abilities('appletun', ['ripen', 'gluttony', 'thickfat']).
pokemon_abilities('appletungmax', ['ripen', 'gluttony', 'thickfat']).
pokemon_abilities('silicobra', ['sandspit', 'shedskin', 'sandveil']).
pokemon_abilities('sandaconda', ['sandspit', 'shedskin', 'sandveil']).
pokemon_abilities('sandacondagmax', ['sandspit', 'shedskin', 'sandveil']).
pokemon_abilities('cramorant', ['gulpmissile']).
pokemon_abilities('cramorantgulping', ['gulpmissile']).
pokemon_abilities('cramorantgorging', ['gulpmissile']).
pokemon_abilities('arrokuda', ['swiftswim', 'propellertail']).
pokemon_abilities('barraskewda', ['swiftswim', 'propellertail']).
pokemon_abilities('toxel', ['rattled', 'static', 'klutz']).
pokemon_abilities('toxtricity', ['punkrock', 'plus', 'technician']).
pokemon_abilities('toxtricitylowkey', ['punkrock', 'minus', 'technician']).
pokemon_abilities('toxtricitygmax', ['punkrock', 'plus', 'technician']).
pokemon_abilities('toxtricitylowkeygmax', ['punkrock', 'minus', 'technician']).
pokemon_abilities('sizzlipede', ['flashfire', 'whitesmoke', 'flamebody']).
pokemon_abilities('centiskorch', ['flashfire', 'whitesmoke', 'flamebody']).
pokemon_abilities('centiskorchgmax', ['flashfire', 'whitesmoke', 'flamebody']).
pokemon_abilities('clobbopus', ['limber', 'technician']).
pokemon_abilities('grapploct', ['limber', 'technician']).
pokemon_abilities('sinistea', ['weakarmor', 'cursedbody']).
pokemon_abilities('sinisteaantique', ['weakarmor', 'cursedbody']).
pokemon_abilities('polteageist', ['weakarmor', 'cursedbody']).
pokemon_abilities('polteageistantique', ['weakarmor', 'cursedbody']).
pokemon_abilities('hatenna', ['healer', 'anticipation', 'magicbounce']).
pokemon_abilities('hattrem', ['healer', 'anticipation', 'magicbounce']).
pokemon_abilities('hatterene', ['healer', 'anticipation', 'magicbounce']).
pokemon_abilities('hatterenegmax', ['healer', 'anticipation', 'magicbounce']).
pokemon_abilities('impidimp', ['prankster', 'frisk', 'pickpocket']).
pokemon_abilities('morgrem', ['prankster', 'frisk', 'pickpocket']).
pokemon_abilities('grimmsnarl', ['prankster', 'frisk', 'pickpocket']).
pokemon_abilities('grimmsnarlgmax', ['prankster', 'frisk', 'pickpocket']).
pokemon_abilities('obstagoon', ['reckless', 'guts', 'defiant']).
pokemon_abilities('perrserker', ['battlearmor', 'toughclaws', 'steelyspirit']).
pokemon_abilities('cursola', ['weakarmor', 'perishbody']).
pokemon_abilities('sirfetchd', ['steadfast', 'scrappy']).
pokemon_abilities('mrrime', ['tangledfeet', 'screencleaner', 'icebody']).
pokemon_abilities('runerigus', ['wanderingspirit']).
pokemon_abilities('milcery', ['sweetveil', 'aromaveil']).
pokemon_abilities('alcremie', ['sweetveil', 'aromaveil']).
pokemon_abilities('alcremierubycream', ['sweetveil', 'aromaveil']).
pokemon_abilities('alcremiematchacream', ['sweetveil', 'aromaveil']).
pokemon_abilities('alcremiemintcream', ['sweetveil', 'aromaveil']).
pokemon_abilities('alcremielemoncream', ['sweetveil', 'aromaveil']).
pokemon_abilities('alcremierubyswirl', ['sweetveil', 'aromaveil']).
pokemon_abilities('alcremiecaramelswirl', ['sweetveil', 'aromaveil']).
pokemon_abilities('alcremierainbowswirl', ['sweetveil', 'aromaveil']).
pokemon_abilities('alcremiegmax', ['sweetveil', 'aromaveil']).
pokemon_abilities('falinks', ['battlearmor', 'defiant']).
pokemon_abilities('falinksmega', ['']).
pokemon_abilities('pincurchin', ['lightningrod', 'electricsurge']).
pokemon_abilities('snom', ['shielddust', 'icescales']).
pokemon_abilities('frosmoth', ['shielddust', 'icescales']).
pokemon_abilities('stonjourner', ['powerspot']).
pokemon_abilities('eiscue', ['iceface']).
pokemon_abilities('eiscuenoice', ['iceface']).
pokemon_abilities('indeedee', ['innerfocus', 'synchronize', 'psychicsurge']).
pokemon_abilities('indeedeef', ['owntempo', 'synchronize', 'psychicsurge']).
pokemon_abilities('morpeko', ['hungerswitch']).
pokemon_abilities('morpekohangry', ['hungerswitch']).
pokemon_abilities('cufant', ['sheerforce', 'heavymetal']).
pokemon_abilities('copperajah', ['sheerforce', 'heavymetal']).
pokemon_abilities('copperajahgmax', ['sheerforce', 'heavymetal']).
pokemon_abilities('dracozolt', ['voltabsorb', 'hustle', 'sandrush']).
pokemon_abilities('arctozolt', ['voltabsorb', 'static', 'slushrush']).
pokemon_abilities('dracovish', ['waterabsorb', 'strongjaw', 'sandrush']).
pokemon_abilities('arctovish', ['waterabsorb', 'icebody', 'slushrush']).
pokemon_abilities('duraludon', ['lightmetal', 'heavymetal', 'stalwart']).
pokemon_abilities('duraludongmax', ['lightmetal', 'heavymetal', 'stalwart']).
pokemon_abilities('dreepy', ['clearbody', 'infiltrator', 'cursedbody']).
pokemon_abilities('drakloak', ['clearbody', 'infiltrator', 'cursedbody']).
pokemon_abilities('dragapult', ['clearbody', 'infiltrator', 'cursedbody']).
pokemon_abilities('zacian', ['intrepidsword']).
pokemon_abilities('zaciancrowned', ['intrepidsword']).
pokemon_abilities('zamazenta', ['dauntlessshield']).
pokemon_abilities('zamazentacrowned', ['dauntlessshield']).
pokemon_abilities('eternatus', ['pressure']).
pokemon_abilities('eternatuseternamax', ['pressure']).
pokemon_abilities('kubfu', ['innerfocus']).
pokemon_abilities('urshifu', ['unseenfist']).
pokemon_abilities('urshifurapidstrike', ['unseenfist']).
pokemon_abilities('urshifugmax', ['unseenfist']).
pokemon_abilities('urshifurapidstrikegmax', ['unseenfist']).
pokemon_abilities('zarude', ['leafguard']).
pokemon_abilities('zarudedada', ['leafguard']).
pokemon_abilities('regieleki', ['transistor']).
pokemon_abilities('regidrago', ['dragon\'smaw']).
pokemon_abilities('glastrier', ['chillingneigh']).
pokemon_abilities('spectrier', ['grimneigh']).
pokemon_abilities('calyrex', ['unnerve']).
pokemon_abilities('calyrexice', ['asone(glastrier)']).
pokemon_abilities('calyrexshadow', ['asone(spectrier)']).
pokemon_abilities('wyrdeer', ['intimidate', 'frisk', 'sapsipper']).
pokemon_abilities('kleavor', ['swarm', 'sheerforce', 'sharpness']).
pokemon_abilities('ursaluna', ['guts', 'bulletproof', 'unnerve']).
pokemon_abilities('ursalunabloodmoon', ['mind\'seye']).
pokemon_abilities('basculegion', ['swiftswim', 'adaptability', 'moldbreaker']).
pokemon_abilities('basculegionf', ['swiftswim', 'adaptability', 'moldbreaker']).
pokemon_abilities('sneasler', ['pressure', 'unburden', 'poisontouch']).
pokemon_abilities('overqwil', ['poisonpoint', 'swiftswim', 'intimidate']).
pokemon_abilities('enamorus', ['cutecharm', 'contrary']).
pokemon_abilities('enamorustherian', ['overcoat']).
pokemon_abilities('sprigatito', ['overgrow', 'protean']).
pokemon_abilities('floragato', ['overgrow', 'protean']).
pokemon_abilities('meowscarada', ['overgrow', 'protean']).
pokemon_abilities('fuecoco', ['blaze', 'unaware']).
pokemon_abilities('crocalor', ['blaze', 'unaware']).
pokemon_abilities('skeledirge', ['blaze', 'unaware']).
pokemon_abilities('quaxly', ['torrent', 'moxie']).
pokemon_abilities('quaxwell', ['torrent', 'moxie']).
pokemon_abilities('quaquaval', ['torrent', 'moxie']).
pokemon_abilities('lechonk', ['aromaveil', 'gluttony', 'thickfat']).
pokemon_abilities('oinkologne', ['lingeringaroma', 'gluttony', 'thickfat']).
pokemon_abilities('oinkolognef', ['aromaveil', 'gluttony', 'thickfat']).
pokemon_abilities('tarountula', ['insomnia', 'stakeout']).
pokemon_abilities('spidops', ['insomnia', 'stakeout']).
pokemon_abilities('nymble', ['swarm', 'tintedlens']).
pokemon_abilities('lokix', ['swarm', 'tintedlens']).
pokemon_abilities('pawmi', ['static', 'naturalcure', 'ironfist']).
pokemon_abilities('pawmo', ['voltabsorb', 'naturalcure', 'ironfist']).
pokemon_abilities('pawmot', ['voltabsorb', 'naturalcure', 'ironfist']).
pokemon_abilities('tandemaus', ['runaway', 'pickup', 'owntempo']).
pokemon_abilities('maushold', ['friendguard', 'cheekpouch', 'technician']).
pokemon_abilities('mausholdfour', ['friendguard', 'cheekpouch', 'technician']).
pokemon_abilities('fidough', ['owntempo', 'klutz']).
pokemon_abilities('dachsbun', ['well-bakedbody', 'aromaveil']).
pokemon_abilities('smoliv', ['earlybird', 'harvest']).
pokemon_abilities('dolliv', ['earlybird', 'harvest']).
pokemon_abilities('arboliva', ['seedsower', 'harvest']).
pokemon_abilities('squawkabilly', ['intimidate', 'hustle', 'guts']).
pokemon_abilities('squawkabillyblue', ['intimidate', 'hustle', 'guts']).
pokemon_abilities('squawkabillyyellow', ['intimidate', 'hustle', 'sheerforce']).
pokemon_abilities('squawkabillywhite', ['intimidate', 'hustle', 'sheerforce']).
pokemon_abilities('nacli', ['purifyingsalt', 'sturdy', 'clearbody']).
pokemon_abilities('naclstack', ['purifyingsalt', 'sturdy', 'clearbody']).
pokemon_abilities('garganacl', ['purifyingsalt', 'sturdy', 'clearbody']).
pokemon_abilities('charcadet', ['flashfire', 'flamebody']).
pokemon_abilities('armarouge', ['flashfire', 'weakarmor']).
pokemon_abilities('ceruledge', ['flashfire', 'weakarmor']).
pokemon_abilities('tadbulb', ['owntempo', 'static', 'damp']).
pokemon_abilities('bellibolt', ['electromorphosis', 'static', 'damp']).
pokemon_abilities('wattrel', ['windpower', 'voltabsorb', 'competitive']).
pokemon_abilities('kilowattrel', ['windpower', 'voltabsorb', 'competitive']).
pokemon_abilities('maschiff', ['intimidate', 'runaway', 'stakeout']).
pokemon_abilities('mabosstiff', ['intimidate', 'guarddog', 'stakeout']).
pokemon_abilities('shroodle', ['unburden', 'pickpocket', 'prankster']).
pokemon_abilities('grafaiai', ['unburden', 'poisontouch', 'prankster']).
pokemon_abilities('bramblin', ['windrider', 'infiltrator']).
pokemon_abilities('brambleghast', ['windrider', 'infiltrator']).
pokemon_abilities('toedscool', ['myceliummight']).
pokemon_abilities('toedscruel', ['myceliummight']).
pokemon_abilities('klawf', ['angershell', 'shellarmor', 'regenerator']).
pokemon_abilities('capsakid', ['chlorophyll', 'insomnia', 'klutz']).
pokemon_abilities('scovillain', ['chlorophyll', 'insomnia', 'moody']).
pokemon_abilities('rellor', ['compoundeyes', 'shedskin']).
pokemon_abilities('rabsca', ['synchronize', 'telepathy']).
pokemon_abilities('flittle', ['anticipation', 'frisk', 'speedboost']).
pokemon_abilities('espathra', ['opportunist', 'frisk', 'speedboost']).
pokemon_abilities('tinkatink', ['moldbreaker', 'owntempo', 'pickpocket']).
pokemon_abilities('tinkatuff', ['moldbreaker', 'owntempo', 'pickpocket']).
pokemon_abilities('tinkaton', ['moldbreaker', 'owntempo', 'pickpocket']).
pokemon_abilities('wiglett', ['gooey', 'rattled', 'sandveil']).
pokemon_abilities('wugtrio', ['gooey', 'rattled', 'sandveil']).
pokemon_abilities('bombirdier', ['bigpecks', 'keeneye', 'rockypayload']).
pokemon_abilities('finizen', ['waterveil']).
pokemon_abilities('palafin', ['zerotohero']).
pokemon_abilities('palafinhero', ['zerotohero']).
pokemon_abilities('varoom', ['overcoat', 'slowstart']).
pokemon_abilities('revavroom', ['overcoat', 'filter']).
pokemon_abilities('cyclizar', ['shedskin', 'regenerator']).
pokemon_abilities('orthworm', ['eartheater', 'sandveil']).
pokemon_abilities('glimmet', ['toxicdebris', 'corrosion']).
pokemon_abilities('glimmora', ['toxicdebris', 'corrosion']).
pokemon_abilities('greavard', ['pickup', 'fluffy']).
pokemon_abilities('houndstone', ['sandrush', 'fluffy']).
pokemon_abilities('flamigo', ['scrappy', 'tangledfeet', 'costar']).
pokemon_abilities('cetoddle', ['thickfat', 'snowcloak', 'sheerforce']).
pokemon_abilities('cetitan', ['thickfat', 'slushrush', 'sheerforce']).
pokemon_abilities('veluza', ['moldbreaker', 'sharpness']).
pokemon_abilities('dondozo', ['unaware', 'oblivious', 'waterveil']).
pokemon_abilities('tatsugiri', ['commander', 'stormdrain']).
pokemon_abilities('tatsugiridroopy', ['commander', 'stormdrain']).
pokemon_abilities('tatsugiristretchy', ['commander', 'stormdrain']).
pokemon_abilities('annihilape', ['vitalspirit', 'innerfocus', 'defiant']).
pokemon_abilities('clodsire', ['poisonpoint', 'waterabsorb', 'unaware']).
pokemon_abilities('farigiraf', ['cudchew', 'armortail', 'sapsipper']).
pokemon_abilities('dudunsparce', ['serenegrace', 'runaway', 'rattled']).
pokemon_abilities('dudunsparcethreesegment', ['serenegrace', 'runaway', 'rattled']).
pokemon_abilities('kingambit', ['defiant', 'supremeoverlord', 'pressure']).
pokemon_abilities('greattusk', ['protosynthesis']).
pokemon_abilities('screamtail', ['protosynthesis']).
pokemon_abilities('brutebonnet', ['protosynthesis']).
pokemon_abilities('fluttermane', ['protosynthesis']).
pokemon_abilities('slitherwing', ['protosynthesis']).
pokemon_abilities('sandyshocks', ['protosynthesis']).
pokemon_abilities('irontreads', ['quarkdrive']).
pokemon_abilities('ironbundle', ['quarkdrive']).
pokemon_abilities('ironhands', ['quarkdrive']).
pokemon_abilities('ironjugulis', ['quarkdrive']).
pokemon_abilities('ironmoth', ['quarkdrive']).
pokemon_abilities('ironthorns', ['quarkdrive']).
pokemon_abilities('frigibax', ['thermalexchange', 'icebody']).
pokemon_abilities('arctibax', ['thermalexchange', 'icebody']).
pokemon_abilities('baxcalibur', ['thermalexchange', 'icebody']).
pokemon_abilities('gimmighoul', ['rattled']).
pokemon_abilities('gimmighoulroaming', ['runaway']).
pokemon_abilities('gholdengo', ['goodasgold']).
pokemon_abilities('wochien', ['tabletsofruin']).
pokemon_abilities('chienpao', ['swordofruin']).
pokemon_abilities('tinglu', ['vesselofruin']).
pokemon_abilities('chiyu', ['beadsofruin']).
pokemon_abilities('roaringmoon', ['protosynthesis']).
pokemon_abilities('ironvaliant', ['quarkdrive']).
pokemon_abilities('koraidon', ['orichalcumpulse']).
pokemon_abilities('miraidon', ['hadronengine']).
pokemon_abilities('walkingwake', ['protosynthesis']).
pokemon_abilities('ironleaves', ['quarkdrive']).
pokemon_abilities('dipplin', ['supersweetsyrup', 'gluttony', 'stickyhold']).
pokemon_abilities('poltchageist', ['hospitality', 'heatproof']).
pokemon_abilities('poltchageistartisan', ['hospitality', 'heatproof']).
pokemon_abilities('sinistcha', ['hospitality', 'heatproof']).
pokemon_abilities('sinistchamasterpiece', ['hospitality', 'heatproof']).
pokemon_abilities('okidogi', ['toxicchain', 'guarddog']).
pokemon_abilities('munkidori', ['toxicchain', 'frisk']).
pokemon_abilities('fezandipiti', ['toxicchain', 'technician']).
pokemon_abilities('ogerpon', ['defiant']).
pokemon_abilities('ogerponwellspring', ['waterabsorb']).
pokemon_abilities('ogerponhearthflame', ['moldbreaker']).
pokemon_abilities('ogerponcornerstone', ['sturdy']).
pokemon_abilities('ogerpontealtera', ['embodyaspect(teal)']).
pokemon_abilities('ogerponwellspringtera', ['embodyaspect(wellspring)']).
pokemon_abilities('ogerponhearthflametera', ['embodyaspect(hearthflame)']).
pokemon_abilities('ogerponcornerstonetera', ['embodyaspect(cornerstone)']).
pokemon_abilities('archaludon', ['stamina', 'sturdy', 'stalwart']).
pokemon_abilities('hydrapple', ['supersweetsyrup', 'regenerator', 'stickyhold']).
pokemon_abilities('gougingfire', ['protosynthesis']).
pokemon_abilities('ragingbolt', ['protosynthesis']).
pokemon_abilities('ironboulder', ['quarkdrive']).
pokemon_abilities('ironcrown', ['quarkdrive']).
pokemon_abilities('terapagos', ['terashift']).
pokemon_abilities('terapagosterastal', ['terashell']).
pokemon_abilities('terapagosstellar', ['teraformzero']).
pokemon_abilities('pecharunt', ['poisonpuppeteer']).
pokemon_abilities('missingno', ['']).
pokemon_abilities('ramnarok', ['']).
pokemon_abilities('ramnarokradiant', ['']).
pokemon_abilities('pokestarsmeargle', ['owntempo', 'technician', 'moody']).
pokemon_abilities('pokestarufo', ['levitate']).
pokemon_abilities('pokestarufo2', ['levitate']).
pokemon_abilities('pokestarbrycenman', ['levitate']).
pokemon_abilities('pokestarmt', ['analytic']).
pokemon_abilities('pokestarmt2', ['flashfire']).
pokemon_abilities('pokestartransport', ['motordrive']).
pokemon_abilities('pokestargiant', ['hugepower']).
pokemon_abilities('pokestarhumanoid', ['insomnia']).
pokemon_abilities('pokestarmonster', ['pressure']).
pokemon_abilities('pokestarf00', ['voltabsorb']).
pokemon_abilities('pokestarf002', ['reckless']).
pokemon_abilities('pokestarspirit', ['wonderguard']).
pokemon_abilities('pokestarblackdoor', ['earlybird']).
pokemon_abilities('pokestarwhitedoor', ['blaze']).
pokemon_abilities('pokestarblackbelt', ['hugepower']).
pokemon_abilities('pokestarufopropu2', ['levitate']).
