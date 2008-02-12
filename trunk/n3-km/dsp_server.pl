#!/usr/local/bin/pl -g serql_welcome -L128m -G128m -T128m -s

:- load_files([ 'SeRQL/load'
	      ],
	      [ silent(true)
	      ]).

:- use_module(n3_entailment).

server :-
	serql_server([]).

server(Port) :-
	serql_server([port(Port)]).

%:- server.

:- use_module('dsp-builtins/aspl_builtins').

:- n3_load('dsp-n3/decode.n3').


think :-
        findall(rdf(S,P,O),n3_entailment:rdf(S,P,O),Bag),
        rdf_write_xml(user_output,Bag).


