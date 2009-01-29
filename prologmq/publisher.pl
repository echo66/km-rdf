:- module(publisher, [publish/2]).

:- use_module(library(http/http_client)).

publish(Term, Server) :-
    term_to_atom(Term, TermAtom),
    http_post(Server, form_data([term=TermAtom]), _Reply, []).

