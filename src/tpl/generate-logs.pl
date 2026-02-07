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

% URLs are a scheme ("https://") followed non-whitespace chars (the path)
url_scheme("https://") --> "https://".
url_scheme("http://") --> "http://".
url(S, P) --> url_scheme(S), non_ws(P), ws.

% A list of whitespace-delimited URLs
urls([url(S, P)|Us]) --> url(S, P), urls(Us).
urls([]) --> [].

% Append .log to all the URLs
log_url(S, P) --> { phrase(format_("~s~s.log", [S, P]), Ls) }, [Ls].
log_urls([url(S, P) | Us]) --> log_url(S, P), log_urls(Us).
log_urls([]) --> [].

fetch_and_write_log(Url) :-
  http_open(Url, Stream, []),
  phrase(format_("./logs/first.log", []), Fp),
  phrase_from_stream(seq(Ls), Stream),
  phrase_to_file(Ls, Fp).

% Read the URLs from the file and print out all the log URLs
make_logs(Fp) :-
  phrase_from_file(urls(U), Fp),
  phrase(log_urls(U), [H | _]),
  fetch_and_write_log(H)
  .

run :- make_logs("./logs/urls").
