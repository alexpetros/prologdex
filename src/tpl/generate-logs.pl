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


% Convert the list of the URLs into newline-delimeted URLs with ".log"
log_url(url(S,P)) --> format_("~s~s.log", [S, P]).
log_urls([url(S, P) | Us]) --> log_url(url(S,P)), "\n", log_urls(Us).
log_urls([]) --> [].

% Read the URLs from the file and print out all the log URLs
make_logs(Fp) :-
  phrase_from_file(urls(U), Fp),
  phrase_to_stream(log_urls(U), user_output).

run :- make_logs("./logs/urls").

% phrase_from_file(url(U), "./logs/urls"),
% http_open("https://www.example.com", S, [])
