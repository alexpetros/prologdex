import fs from 'node:fs'
import showdown from 'pokemon-showdown'
import pokedex from '../vendor/showdown/pokedex.json' with { type: 'json' }

const { Dex } = showdown

const POKEMON_PL_FILE = "./db/dex/pokemon.pl"
const LEARNESET_PL_FILE = "./db/dex/learnsets.pl"
const MOVES_PL_FILE = "./db/dex/moves.pl"
const DRAFT_PL_FILE = "./db/dex/draft.pl"

class ModuleFile {
  constructor(path) {
    fs.rmSync(path, { force: true })
    this.stream = fs.createWriteStream(path)
    this.stream.on('error', (e) => console.error(e))
    this.writeln("% GENERATED FILE - do not modify directly\n% see create-dex.js")
  }

  write(string = '') {
    this.stream.write(string)
  }

  writeln(string = '') {
    this.stream.write(string + '\n')
  }

  close() {
    this.stream.close()
  }
}

function writePredicates(stream, predicates) {
  if (!predicates || predicates.length < 1) return

  let toWrite = Array.isArray(predicates[0]) ? predicates : [predicates]
  toWrite.forEach((p) => writeSinglePredicate(stream, p))
}

function writeSinglePredicate(stream, predicate) {
  const [name, ...args] = predicate
  const argList = args.map(arg => {
    if (typeof arg == 'number' || typeof arg == 'boolean') {
      return arg.toString()
    } else {
      const normalized = arg.toString().toLowerCase().replace("'", "\\'").replace(' ', '')
      return `'${normalized}'`
    }
  })
  const argString = argList.join(', ')
  stream.writeln(`${name}(${argString}).`)
}

function writeModuleDeclaration(stream, name, indicators) {
  stream.write(":- module(")
  stream.write(`${name}, [`)
  const indicatorString = indicators.join(', ')
  stream.write(indicatorString)
  stream.writeln("]).\n")
}

// Pokemon: types and stats
const pokemonStream = new ModuleFile(POKEMON_PL_FILE)
function forEachPokemon(predicateFunc) {
  for (const id in pokedex) {
    const mon = Dex.species.get(id)
    if (mon.isNonstandard == "CAP") continue // excluded create-a-pokemon mons
    writePredicates(pokemonStream, predicateFunc(mon))
  }
}

writeModuleDeclaration( pokemonStream, 'dex', [
  'pokemon/1', 'type/2', 'pokemon_ability/2', 'pokemon_hp/2', 'pokemon_atk/2', 'pokemon_def/2',
  'pokemon_spa/2', 'pokemon_spd/2', 'pokemon_spe/2'
])

forEachPokemon((mon) => ['pokemon', mon.id])
forEachPokemon((mon) => ['pokemon_hp', mon.id, mon.baseStats.hp])
forEachPokemon((mon) => ['pokemon_atk', mon.id, mon.baseStats.atk])
forEachPokemon((mon) => ['pokemon_def', mon.id, mon.baseStats.def])
forEachPokemon((mon) => ['pokemon_spa', mon.id, mon.baseStats.spa])
forEachPokemon((mon) => ['pokemon_spd', mon.id, mon.baseStats.spd])
forEachPokemon((mon) => ['pokemon_spe', mon.id, mon.baseStats.spe])
forEachPokemon((mon) => mon.types.map(type => ['type', mon.id, type]))
forEachPokemon((mon) => {
  const abilities = Object.values(mon.abilities)
  return abilities.map(ability => ['pokemon_ability', mon.id, ability])
})
pokemonStream.close()

// Learnsets
const learnsetsStream = new ModuleFile(LEARNESET_PL_FILE)
writeModuleDeclaration(learnsetsStream, 'learnsets', ['learns/2'])
for (const id in pokedex) {
  const mon = Dex.species.get(id)
  const moves = Dex.species.getMovePool(mon, true)
  moves.forEach(move => {
    if (move !== 'hiddenpower') writePredicates(learnsetsStream, ['learns', mon.id, move])
  })
}
learnsetsStream.close()

// Moves
const movesStream = new ModuleFile(MOVES_PL_FILE)
writeModuleDeclaration(movesStream, 'moves', [
  'move/1', 'move_type/2', 'move_power/2', 'move_accuracy/2', 'move_category/2', 'move_boost/3',
  'move_target/2', 'move_priority/2'
])

// Excluding hidden power for now because it's not legal and adds a lot of noise
const moves = Dex.moves.all().filter(move => move.id !== 'hiddenpower')
function forEachMove(predicateFunc) {
  moves.forEach(move => { writePredicates(movesStream, predicateFunc(move)) })
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
forEachMove((move) => {
  if (!move.boosts) return
  const stats = Object.keys(move.boosts)
  return stats.map(stat => ['move_boost', move.id, stat, move.boosts[stat]])
})
forEachMove((move) => ['move_target', move.id, move.target])
forEachMove((move) => ['move_priority', move.id, move.priority])

movesStream.close()


const REMOVAL_MOVES = [ 'rapidspin', 'defog', 'courtchange', 'tidyup' ]
const HAZARD_MOVES = ['stealthrock', 'spikes', 'toxicspikes', 'stickyweb']
const DOUBLES_MOVES = ['helpinghand', 'afteryou', 'quash', 'allyswitch', 'followme', 'ragepowder', 'aromaticmist', 'holdhands', 'spotlight', 'craftyshield', 'quickguard', 'wideguard']
const PROTECTION_MOVES = ['endure', 'detect', 'protect', 'magiccoat', 'kingsshield', 'burningbulwark', 'spikyshield', 'banefulbunker']

const draftStream = new ModuleFile(DRAFT_PL_FILE)
writeModuleDeclaration(draftStream, 'draft', [
  'protection_move/1', 'protection_move_t/2',
  'hazard_move/1', 'hazard_move_t/2',
  'removal_move/1', 'removal_move_t/2',
  'doubles_move/1', 'doubles_move_t/2',
])
function forEachMoveDraftPreds(predicateFunc) {
  moves.forEach(move => { writePredicates(draftStream, predicateFunc(move)) })
  draftStream.writeln()
}

draftStream.writeln("protection_move(Move) :- protection_move_t(Move, true).")
draftStream.writeln("hazard_move(Move) :- hazard_move_t(Move, true).")
draftStream.writeln("removal_move(Move) :- removal_move_t(Move, true).")
draftStream.writeln("doubles_move(Move) :- doubles_move_t(Move, true).")

forEachMoveDraftPreds((move) =>
  ['protection_move_t', move.id, PROTECTION_MOVES.includes(move.id)]
)
forEachMoveDraftPreds((move) =>
  ['hazard_move_t', move.id, HAZARD_MOVES.includes(move.id)]
)
forEachMoveDraftPreds((move) =>
  ['removal_move_t', move.id, REMOVAL_MOVES.includes(move.id)]
)
forEachMoveDraftPreds((move) =>
  ['doubles_move_t', move.id, DOUBLES_MOVES.includes(move.id)]
)

draftStream.close()
