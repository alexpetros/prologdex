#!node

import pokedex from '../vendor/showdown/pokedex.json' with { type: 'json' }
import learnsets from '../vendor/showdown/learnsets.json' with { type: 'json' }

// Learnset
for (const mon in pokedex) {
  const learnset = learnsets[mon]?.learnset
  for (const move in learnset) {
    console.log(`learns('${mon}', '${move}').`)
  }
}

// Types
for (const mon in pokedex) {
  const types = pokedex[mon]?.types
  if (!types) { continue }
  for (const type of types) {
    console.log(`type('${mon}', ${type}).`)
  }
}


