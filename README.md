# Prologdex

A prolog-based Pokemon Draft database.
It has the goal of eventually being a general, wasm-based web tool.


Right now it mostly lets me query facts about my current draft league.

## Installation

1. Install the [rust toolchain](https://rust-lang.org/learn/get-started/)
2. Run `rust install scryer-prolog just`

## Usage

`just dex` starts the prolog engine.

Prolog tries to "unify" your queries by coming up with all possible matches for the given variables. Variables are denoted by names that start with uppercase letters, and facts start with lowercase ones.
The [dex.pl](./dex/dex.pl) database contains facts with basic Pokedex info.

```prolog
pokemon(zapdos).

type(zapdos, electric).
type(zapdos, flying).

learns(zapdos, defog).
learns(zapdos, hurricane).
% and so on
```

> [!WARNING]
> The movesets are not quite correct yet. The JSON that I used from Pokemon Showdown doesn't include egg moves (and a few other things).

You can ask who all the pokemon on Morry's team are with the following query:

```prolog
% Get morry's team
morry(Pokemon).
```

The interactive shell will then present you with one possible match:

```prolog
?- morry(Pokemon).
   Pokemon = mawilemega
```

From here, you can control what to do based on hitting certain keys:
* `a` - show all the solutions
* `n` - show the next solution
* `.` - stop showing solutions
* `h` - show the help message

So if you press `a`, you'll see (at the time of this writing):

```prolog
?- morry(Pokemon).
   Pokemon = mawilemega
;  Pokemon = walkingwake
;  Pokemon = zapdos
;  Pokemon = ursaluna.
```

A couple other queries I've been using:

```prolog
% Which undrafted steel types learn Volt Switch?
undrafted(Mon), type(Mon, steel), learns(Mon, voltswitch).

% Which draft mons have removal?
drafted(Mon), learns_removal(Mon).

% The point value of Morry's draft
total_points(morry, Points).

% The remaining points in Morry's draft
total_points(morry, Points), Remaining is 90 - Points.

% The remaining points in everyone's drafts
total_points(Player, Points), Remaining is 90 - Points.
```

The structure is still messy, but most of the season 6 draft facts, such as the draft board and people's teams, can be found in [s6.pl](./dex/s6.pl).

## Other resources

* https://github.com/mthom/scryer-prolog/
* https://www.scryer.pl/
* https://dex.pokemonshowdown.com
