import fs from 'node:fs'
import showdown from 'pokemon-showdown'
import pokedex from '../vendor/showdown/pokedex.json' with { type: 'json' }

const { Dex } = showdown

// const POKEMON_PL_FILE = "./dex/pokemon.pl"
// const LEARNESET_PL_FILE = "./dex/learnsets.pl"

// fs.rmSync(POKEMON_PL_FILE, { force: true })
// fs.rmSync(LEARNESET_PL_FILE, { force: true })

const DEX_FILE = "./dex/dex.pl"
fs.rmSync(DEX_FILE, { force: true })

const pokemonStream = fs.createWriteStream(DEX_FILE)
pokemonStream.on('error', (e) => console.error(e))
pokemonStream.write(":- module(dex, [pokemon/1, learns/2, type/2]).\n\n")
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

// Learnset
// const learnsetStream = fs.createWriteStream(DEX_FILE)
// learnsetStream.on('error', (e) => console.error(e))

for (const id in pokedex) {
  const mon = Dex.species.get(id)
  const moves = Dex.species.getMovePool(mon, true)
  for (const move of moves) {
    pokemonStream.write(`learns('${mon.id}', '${move}').\n`)
  }
}
pokemonStream.close()
