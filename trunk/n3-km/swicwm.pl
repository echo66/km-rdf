#!/usr/local/bin/pl -q -s 

:- use_module(n3_entailment).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(rdf)).


think(File) :-
	n3_entailment:n3_load(File),
	findall(rdf(S,P,O),n3_entailment:rdf(S,P,O),Bag),
	rdf_write_xml(user_output,Bag).

:- 
	current_prolog_flag(argv,L),
	last(L,File),
	think(File),
	halt.


