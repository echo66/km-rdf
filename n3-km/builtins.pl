:- module(builtins,[builtin/2,convert/4]).


/**
 * This modules provides anchors for builtin predicates.
 * By registering a new builtin(Predicate,CorrespondingPrologPredicate)
 * clause, proving Predicate using the n3_entailment
 * module will call CorrespondingPrologPredicate.
 *
 * The file examples/list.n3 gives an illustration of that, 
 * and uses the list:member/2 SWI predicate.
 *
 * Copyright Yves Raimond (c) 2007
 * Centre for Digital Music, Queen Mary, University of London
 */


:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_meta builtin(r,?).


/**
 * Switch between a RDF representation and a Prolog one,
 * that might actually be used to call a Prolog predicate
 */
convert(rdf(S,P,O),rdf(SS,P,OO),Args,Bindings) :-
	convert_node(S,SS,SL,SB),
	convert_node(O,OO,OL,OB),
	append(SL,OL,Args),
	append(SB,OB,Bindings).
convert_node(S,S,[SL],[]) :-
	rdfs_list_to_prolog_list(S,SL),!.
	%findall(t(C,B),(member(M,SL),convert(M,C,B)),T),
	%findall(C,member(t(C,_),T),SL),
	%findall(B,member(t(_,B),T),T2),
	%flatten(T2,Bindings),!.
convert_node(S,T,[T],[match(S,T)]) :-
	rdf_is_bnode(S),!.
convert_node(literal(type(_,Lit)),literal(type(_,Lit)),[SL],[]) :-
	atom_to_term(Lit,SL,[]),!.
convert_node(literal(lang(_,Lit)),literal(lang(_,Lit)),[SL],[]) :-
	atom_to_term(Lit,SL,[]),!.
convert_node(literal(Lit),literal(Lit),[SL],[]) :-
	atom_to_term(Lit,SL,[]),!.
convert_node(S,[S],[]).

/**
 *
 * Declaration of builtin predicates
 *
 */

builtin('http://purl.org/ontology/swi/member',list:member).



