:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(http/http_open)).

replay_origin --> "https://replay.pokemonshowdown.com/".

% Whitespace and non-whitespace sequences
ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].
non_ws([C|Cs]) --> [C], { \+ char_type(C, whitespace) }, non_ws(Cs).
non_ws([]) --> [].

replay(Id) --> replay_origin, non_ws(Id), ws.
replays([replay(Id)|Rs]) --> replay(Id), replays(Rs).
replays([]) --> [].
replay_url(replay(Id)) --> replay_origin, Id.

fetch_and_write_log(Url) :-
  http_open(Url, Stream, []),
  phrase(format_("./logs/first.log", []), Fp),
  phrase_from_stream(seq(Ls), Stream),
  phrase_to_file(Ls, Fp).

list_replay_urls([R|Rs]) --> replay_url(R), "\n", list_replay_urls(Rs).
list_replay_urls([]) --> [].

get_replays(Fp, Ls) :-
  phrase_from_file(replays(Rs), Fp),
  phrase(list_replay_urls(Rs), Ls).

% Read the URLs from the file and print out all the log URLs
make_logs(Fp) :-
  get_replays(Fp, [H|_]),
  fetch_and_write_log(H)
  .

run :- make_logs("./logs/urls").
