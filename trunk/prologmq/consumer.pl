:- module(consumer, [consume/2]).

:- use_module(library('http/http_client')).

consume(Term, Server) :-
    http_get(Server, Reply, []),
    term_to_atom(Term, Reply).
