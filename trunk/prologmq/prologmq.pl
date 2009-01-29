:- module(prologmq, [server/0, server/2]).

:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_client')).
:- use_module(library('http/http_mime_plugin')).

:- dynamic queue/1.

server(Port, Options) :-
        http_server(reply,[ port(Port),timeout(20)| Options]).

reply(Request) :-
    member(path('/'), Request),
    member(method(post), Request), !,
    http_read_data(Request,Data,[]),
    member((term=AtomTerm), Data),
    term_to_atom(Term, AtomTerm),
    assert(queue(Term)),
    format('Content-type: text/plain~n~n', []),
    format('Term added to queue~n',[]).

reply(Request) :-
    member(path('/'), Request),
    member(method(get), Request), !,
    queue(Term),
    term_to_atom(Term, AtomTerm),
    format('Content-type: text/plain~n~n', []),
    format('~w~n',[AtomTerm]),
    retract(queue(Term)).


server :- server(3333,[]).    
