:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(http/http_open)).

% Whitespace and non-whitespace sequences
ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].
non_ws([C|Cs]) --> [C], { \+ char_type(C, whitespace) }, non_ws(Cs).
non_ws([]) --> [].

replay_origin --> "https://replay.pokemonshowdown.com/".

replay(Id) --> replay_origin, non_ws(Id), ws.
replays([replay(Id)|Rs]) --> replay(Id), replays(Rs).
replays([]) --> [].

replay_url(replay(Id)) --> replay_origin, Id.
log_url(replay(Id)) --> replay_origin, Id, ".log".
log_fp(replay(Id)) --> "./logs/", Id, ".log".

fetch_and_write_log(Replay) :-
  phrase(log_url(Replay), Url),
  http_open(Url, Stream, []),
  phrase(log_fp(Replay), Fp),
  phrase_from_stream(seq(Ls), Stream),
  phrase_to_file(Ls, Fp).

list_replay_urls([R|Rs]) --> replay_url(R), "\n", list_replay_urls(Rs).
list_replay_urls([]) --> [].

get_replays(Fp, Rs) :- phrase_from_file(replays(Rs), Fp).

% Read the URLs from the file and print out all the log URLs
make_logs(Fp) :-
  get_replays(Fp, Rs),
  maplist(fetch_and_write_log, Rs).

run :- make_logs("./logs/urls").
