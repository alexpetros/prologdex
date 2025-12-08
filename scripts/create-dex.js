import showdown from 'pokemon-showdown'
import pokedex from '../vendor/showdown/pokedex.json' with { type: 'json' }

const { Dex } = showdown
const log = console.log


log(":- module(dex, [pokemon/1, learns/2, type/2]).\n")

// Mons
for (const id in pokedex) {
  const mon = Dex.species.get(id)
  log(`pokemon('${mon.id}').`)
  // const hasMega = mon.otherFormes.some(name => name.endsWith("-Mega"))
  // if (hasMega) log(`pokemon(${mon.id}mega)`)
}
log()

// Types
for (const id in pokedex) {
  const mon = Dex.species.get(id)
  for (const type of mon.types) {
    log(`type('${mon.id}', '${type.toLowerCase()}').`)
  }
}
log()

// Learnset
for (const id in pokedex) {
  const mon = Dex.species.get(id)
  const moves = Dex.species.getMovePool(mon, true)
  for (const move of moves) {
    log(`learns('${mon.id}', '${move}').`)
  }
}

