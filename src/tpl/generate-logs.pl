:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(http/http_open)).

ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].

eos([], []).
line([])     --> ( "\n" | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).
lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).


url_scheme("https://") --> "https://".
url_scheme("http://") --> "http://".

url(S, U) --> url_scheme(S), line(U), ws.
urls([url(S, U)|Us]) --> url(S, U), urls(Us).
urls([]) --> [].

log_urls([]) --> [].
log_urls([url(S, U) | Us]) -->
  format_("~s~s.log~n", [S, U]),
  log_urls(Us).

make_logs(Fp) :-
  phrase_from_file(urls(U), Fp),
  phrase_to_stream(log_urls(U), user_output).

run :- make_logs("./logs/urls").

% phrase_from_file(url(U), "./logs/urls"),
% http_open("https://www.example.com", S, [])
