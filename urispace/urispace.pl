:- module(urispace,[init/0]).


:- use_module(library('http/thread_httpd')).
:- use_module(library('semweb/rdf_db')).
:- use_module(log).
:- use_module(config).
:- use_module(mapping).

server(Port, Options) :-
        http_server(reply,[ port(Port),timeout(20)| Options]).



/**
  * Closes the servlet
  */
reply(Request) :-
        log:log(Request),
        member(path('/quit'), Request), !,
        format('Connection: close~n', []),
        format('Content-type: text/html~n~n', []),
        format('Bye Bye~n').


/**
 * Sends back 303 to RDF document describing the resource
 */
reply(Request) :-
	member(path(Path),Request),
	member(accept(AcceptHeader),Request),
	log:log('Accept header: ~w ',[AcceptHeader]),
	accept_rdf(AcceptHeader),
	concat_atom(PatternX,'/',Path),
	delete(PatternX,'',Pattern),
	mapping:see_other_rdf(requested_pattern(Pattern),redirect_pattern(RedirectP)),
	!,
	concat_atom(RedirectP,Redirect),
	log:log('Sending a 303 towards ~w',Redirect),
	throw(http_reply(see_other(Redirect),[])).

accept_rdf('application/rdf+xml').
accept_rdf('text/xml').
accept_rdf(AcceptHeader) :-
	sub_atom(AcceptHeader,_,_,_,'application/rdf+xml').
accept_rdf(AcceptHeader) :-
        sub_atom(AcceptHeader,_,_,_,'text/xml').

/**
 * Sends back towards the default representation of the resource
 * (usually html)
 */
reply(Request) :-
        member(path(Path),Request),
        concat_atom(PatternX,'/',Path),
        delete(PatternX,'',Pattern),
        mapping:see_other(requested_pattern(Pattern),redirect_pattern(RedirectP)),
        !,
        concat_atom(RedirectP,Redirect),
        log:log('Sending a 303 towards ~w',Redirect),
        throw(http_reply(see_other(Redirect),[])).

init :- 
        config:port(P),
        server(P,[]),
        nl,
        writeln(' - Server launched'),nl.

:-
 nl,writeln('----------------'),nl,
 writeln(' - Launch the server by running ?-init.'),
 writeln(' - To change the port, change config.pl'),nl,writeln('----------------'),nl.


