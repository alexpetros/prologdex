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


// Pokemon and their types
const pokemonStream = new ModuleFile(POKEMON_PL_FILE)
pokemonStream.writeln(":- module(dex, [pokemon/1, type/2]).\n")
for (const id in pokedex) {
  const mon = Dex.species.get(id)
  pokemonStream.writeln(`pokemon('${mon.id}').`)
}
for (const id in pokedex) {
  const mon = Dex.species.get(id)
  for (const type of mon.types) {
    pokemonStream.writeln(`type('${mon.id}', '${type.toLowerCase()}').`)
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
    learnsetsStream.writeln(`learns('${mon.id}', '${move}').`)
  }
}
learnsetsStream.close()

// Moves
const movesStream = new ModuleFile(MOVES_PL_FILE)
movesStream.writeln(":- module(moves, [move/1, move_type/2, move_power/2, move_accuracy/2]).\n")
const moves = Dex.moves.all()
for (const move of moves) {
  const id = move.id
  movesStream.writeln(`move('${id}').`)
}
movesStream.writeln()
for (const move of moves) {
  const id = move.id
  const type = move.type.toLowerCase()
  movesStream.writeln(`move_type('${id}', '${type}').`)
}
movesStream.writeln()
for (const move of moves) {
  const id = move.id
  const power = move.basePower
  movesStream.writeln(`move_power('${id}', ${power}).`)
}
movesStream.writeln()
for (const move of moves) {
  const id = move.id
  const accuracy = move.accuracy === true ? 'true' : move.accuracy

  movesStream.writeln(`move_accuracy('${id}', ${accuracy}).`)
}
movesStream.close()
