:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(http/http_open)).

% Download the all the logs and write them to the logs directory
run :- make_logs("./logs/urls", "./logs").

% Read the URLs from the file and print out all the log URLs
make_logs(ReplayUrlsFp, OutputDir) :-
  phrase_from_file(replays(Rs), ReplayUrlsFp),
  maplist(fetch_and_write_log(OutputDir), Rs).

fetch_log(Replay, Log) :-
  phrase(log_url(Replay), Url),
  http_open(Url, Stream, []),
  phrase_from_stream(seq(Log), Stream).

fetch_and_write_log(OutputDir, Replay) :-
  phrase(log_fp(Replay, OutputDir), LogFp),
  fetch_log(Replay, Log),
  phrase_to_file(Log, LogFp).

replay_origin --> "https://replay.pokemonshowdown.com/".

% Describes a file of replay URLs with arbitrary whitespace between them
replay(Id) --> replay_origin, non_ws(Id), ws.
replays([replay(Id)|Rs]) --> replay(Id), replays(Rs).
replays([]) --> [].

% Terminal descriptions of the replay
replay_url(replay(Id)) --> replay_origin, Id.
log_url(replay(Id)) --> replay_origin, Id, ".log".
log_fp(replay(Id), OutputDir) --> OutputDir, "/", Id, ".log".

% Turn a list of replays into newline-delimeted URLs
% You can change `replay_url` to `log_url` to get the .log URL
list_replay_urls([R|Rs]) --> replay_url(R), "\n", list_replay_urls(Rs).
list_replay_urls([]) --> [].

% Whitespace and non-whitespace sequences
ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].
non_ws([C|Cs]) --> [C], { \+ char_type(C, whitespace) }, non_ws(Cs).
non_ws([]) --> [].

