import fs from 'node:fs'
import showdown from 'pokemon-showdown'
import pokedex from '../vendor/showdown/pokedex.json' with { type: 'json' }

const { Dex } = showdown

const POKEMON_PL_FILE = "./db/dex/pokemon.pl"
const LEARNESET_PL_FILE = "./db/dex/learnsets.pl"
const MOVES_PL_FILE = "./db/dex/moves.pl"

class ModuleFile {
  constructor(path) {
    fs.rmSync(path, { force: true })
    this.stream = fs.createWriteStream(path)
    this.stream.on('error', (e) => console.error(e))
    this.writeln("% GENERATED FILE - do not modify directly\n% see create-dex.js")
  }

  writeln(string = '') {
    this.stream.write(string + '\n')
  }

  close() {
    this.stream.close()
  }
}

// Pokemon: types and stats
const pokemonStream = new ModuleFile(POKEMON_PL_FILE)
pokemonStream.writeln(`:- module(dex, [pokemon/1, type/2, pokemon_ability/2,
  pokemon_hp/2, pokemon_atk/2, pokemon_def/2, pokemon_spa/2, pokemon_spd/2, pokemon_spe/2
]).
`)

for (const id in pokedex) {
  const mon = Dex.species.get(id)
  pokemonStream.writeln(`pokemon('${mon.id}').`)
}

for (const id in pokedex) {
  const mon = Dex.species.get(id)
  pokemonStream.writeln(`pokemon_hp('${mon.id}', ${mon.baseStats.hp}).`)
}

for (const id in pokedex) {
  const mon = Dex.species.get(id)
  pokemonStream.writeln(`pokemon_atk('${mon.id}', ${mon.baseStats.atk}).`)
}

for (const id in pokedex) {
  const mon = Dex.species.get(id)
  pokemonStream.writeln(`pokemon_def('${mon.id}', ${mon.baseStats.def}).`)
}

for (const id in pokedex) {
  const mon = Dex.species.get(id)
  pokemonStream.writeln(`pokemon_spa('${mon.id}', ${mon.baseStats.spa}).`)
}

for (const id in pokedex) {
  const mon = Dex.species.get(id)
  pokemonStream.writeln(`pokemon_spd('${mon.id}', ${mon.baseStats.spd}).`)
}

for (const id in pokedex) {
  const mon = Dex.species.get(id)
  pokemonStream.writeln(`pokemon_spe('${mon.id}', ${mon.baseStats.spe}).`)
}

for (const id in pokedex) {
  const mon = Dex.species.get(id)
  for (const type of mon.types) {
    pokemonStream.writeln(`type('${mon.id}', '${type.toLowerCase()}').`)
  }
}

for (const id in pokedex) {
  const mon = Dex.species.get(id)
  const abilities = Object.values(mon.abilities)
  for (const ability of abilities) {
    const normalizedAbility = ability.toLowerCase().replace("'", "\\'").replace(' ', '')
    pokemonStream.writeln(`pokemon_ability('${mon.id}', '${normalizedAbility}').`)
  }
}
pokemonStream.close()

// Learnsets
const learnsetsStream = new ModuleFile(LEARNESET_PL_FILE)
learnsetsStream.writeln(":- module(learnsets, [learns/2]).\n")
for (const id in pokedex) {
  const mon = Dex.species.get(id)
  const moves = Dex.species.getMovePool(mon, true)
  for (const move of moves) {
    if (move !== 'hiddenpower') learnsetsStream.writeln(`learns('${mon.id}', '${move}').`)
  }
}
learnsetsStream.close()

// Moves
const movesStream = new ModuleFile(MOVES_PL_FILE)

movesStream.writeln(":- module(moves, [move/1, move_type/2, move_power/2, move_accuracy/2, move_category/2, move_boost/3, move_target/2, move_priority/2]).\n")

// Excluding hidden power for now because it's not legal and adds a lot of noise
const moves = Dex.moves.all().filter(move => move.id !== 'hiddenpower')
function forEachMove(predicateFunc) {
  moves.forEach(move => { writePredicate(movesStream, predicateFunc(move)) })
  movesStream.writeln()
}

forEachMove((move) => ['move', move.id])

forEachMove((move) => ['move_type', move.id, move.type])

forEachMove((move) => ['move_power', move.id, move.basePower])

forEachMove((move) => {
  const acc = move.accuracy === true || move.accuracy
  return ['move_accuracy', move.id, acc]
})

forEachMove((move) => ( ['move_category', move.id, move.category]))

for (const move of moves) {
  for (const stat in move.boosts) {
    const id = move.id
    const boost = move.boosts[stat]
    movesStream.writeln(`move_boost('${id}', ${stat}, ${boost}).`)
  }
}
movesStream.writeln()

forEachMove((move) => ['move_target', move.id, move.target])
forEachMove((move) => ['move_priority', move.id, move.priority])

movesStream.close()

function writePredicate(stream, predicate) {
  if (!predicate || predicate.length < 1) return
  const [name, ...args] = predicate
  const argList = args.map(arg => {
    if (typeof arg == 'number' || typeof arg == 'boolean') {
      return arg.toString()
    } else {
      return `'${arg.toString().toLowerCase()}'`
    }
  })
  const argString = argList.join(', ')
  stream.writeln(`${name}(${argString}).`)
}

