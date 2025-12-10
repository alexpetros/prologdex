import fs from 'node:fs'
import showdown from 'pokemon-showdown'
import pokedex from '../vendor/showdown/pokedex.json' with { type: 'json' }

const { Dex } = showdown

const POKEMON_PL_FILE = "./db/dex/pokemon.pl"
const LEARNESET_PL_FILE = "./db/dex/learnsets.pl"

fs.rmSync(POKEMON_PL_FILE, { force: true })
fs.rmSync(LEARNESET_PL_FILE, { force: true })

const pokemonStream = fs.createWriteStream(POKEMON_PL_FILE)
pokemonStream.on('error', (e) => console.error(e))
pokemonStream.write("% GENERATED FILE - do not modify directly\n")
pokemonStream.write("% see create-dex.js\n")
pokemonStream.write(":- module(dex, [pokemon/1, type/2]).\n\n")

// Mons
for (const id in pokedex) {
  const mon = Dex.species.get(id)
  pokemonStream.write(`pokemon('${mon.id}').\n`)
  // const hasMega = mon.otherFormes.some(name => name.endsWith("-Mega"))
  // if (hasMega) log(`pokemon(${mon.id}mega)`)
}

// Types
for (const id in pokedex) {
  const mon = Dex.species.get(id)
  for (const type of mon.types) {
    pokemonStream.write(`type('${mon.id}', '${type.toLowerCase()}').\n`)
  }
}
pokemonStream.write('\n')
pokemonStream.close()

// Learnset
const learnsetStream = fs.createWriteStream(LEARNESET_PL_FILE)
learnsetStream.on('error', (e) => console.error(e))
learnsetStream.write("% GENERATED FILE - do not modify directly\n")
learnsetStream.write("% see create-dex.js\n")
learnsetStream.write(":- module(learnsets, [learns/2]).\n\n")
for (const id in pokedex) {
  const mon = Dex.species.get(id)
  const moves = Dex.species.getMovePool(mon, true)
  for (const move of moves) {
    learnsetStream.write(`learns('${mon.id}', '${move}').\n`)
  }
}
learnsetStream.close()
