:- module(rdf_dump,[rdf_dump/1,rdf_dump/0]).

/**
 * A module providing a way to dump all available 
 * RDF information, through p2r_entailement
 */

:- use_module(library(rdf_write)).
:- use_module(p2r_entailment).

rdf_dump :-
	rdf_dump('rdf_dump.rdf').

rdf_dump(File) :-
	open(File,write,Id,[]),
	bagof(rdf(A,B,C),p2r_entailment:rdf(A,B,C),Bag),
	rdf_write_xml(Id,Bag),close(Id).

