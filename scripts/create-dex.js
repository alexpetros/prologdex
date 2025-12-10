import fs from 'node:fs'
import showdown from 'pokemon-showdown'
import pokedex from '../vendor/showdown/pokedex.json' with { type: 'json' }

const { Dex } = showdown

const POKEMON_PL_FILE = "./db/dex/pokemon.pl"
const LEARNESET_PL_FILE = "./db/dex/learnsets.pl"

fs.rmSync(POKEMON_PL_FILE, { force: true })
fs.rmSync(LEARNESET_PL_FILE, { force: true })

class File {
  constructor(path) {
    this.stream = fs.createWriteStream(path)
    this.stream.on('error', (e) => console.error(e))
  }

  writeln(string) {
    this.stream.write(string + '\n')
  }

  close() {
    this.stream.close()
  }
}


// Pomeon and their types
const pokemonStream = new File(POKEMON_PL_FILE)
pokemonStream.writeln("% GENERATED FILE - do not modify directly\n% see create-dex.js")
pokemonStream.writeln(":- module(dex, [pokemon/1, type/2]).\n")

for (const id in pokedex) {
  const mon = Dex.species.get(id)
  pokemonStream.writeln(`pokemon('${mon.id}').`)
  // const hasMega = mon.otherFormes.some(name => name.endsWith("-Mega"))
  // if (hasMega) log(`pokemon(${mon.id}mega)`)
}
for (const id in pokedex) {
  const mon = Dex.species.get(id)
  for (const type of mon.types) {
    pokemonStream.writeln(`type('${mon.id}', '${type.toLowerCase()}').`)
  }
}
pokemonStream.close()

// Learnsets
const learnsetsStream = new File(LEARNESET_PL_FILE)
learnsetsStream.writeln("% GENERATED FILE - do not modify directly\n% see create-dex.js")
learnsetsStream.writeln(":- module(learnsets, [learns/2]).\n")
for (const id in pokedex) {
  const mon = Dex.species.get(id)
  const moves = Dex.species.getMovePool(mon, true)
  for (const move of moves) {
    learnsetsStream.writeln(`learns('${mon.id}', '${move}').`)
  }
}
learnsetsStream.close()

// // Moves
// for (const move of Dex.moves.all()) {
//   const id = move.id
//   const type = move.type.toLowerCase()
//   console.log(move)
// }
