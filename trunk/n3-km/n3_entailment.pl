:- module(n3_entailment,[n3_load/1]).

/**
 * This module provides entailment 
 * rules based on a set of N3 formulae.
 *
 * The N3 formulae are stored within the SWI
 * quad store, using the representation tricks
 * mentioned in n3_dcg.pl
 *
 * The top-level predicate is n3_entailment:rdf/3
 *
 * This is designed to be used as an entailment 
 * module in SeRQL:
 * http://www.swi-prolog.org/packages/SeRQL/
 *
 * This is rather small and understandable, but hacky
 * at some points.
 * It mainly supports nesting of rules and builtin
 * predicates (see builtins.pl). A set of examples
 * is available in examples/.
 *
 * A small CWM-like (although this module covers probably
 * 1/1000 of the CWM features) script is available in
 * swicwm.pl - on the command-line, this is roughly the equivalent
 * of 
 * $ cwm $1 --think --rdf --data
 *
 * The biggest limitation is perhaps the lack of recursive rule
 * handling (see examples/same.n3), which results
 * in a stack overflow. Will I have to implement
 * a XSB-style tabling system?
 *
 * Copyright Yves Raimond (c) 2007
 * Centre for Digital Music, Queen Mary, University of London
 */


:- use_module(n3_dcg).
:- use_module(library('semweb/rdf_db'),
              [ rdf_global_id/2,
                rdf_reachable/3,
                rdf_has/3,
                rdf_subject/1,
                rdf_equal/2
              ]).
:- use_module(namespaces).
:- use_module(builtins).


term_expansion((rdf(S0, P0, O0) :- Body),
               (rdf(S,  P,  O)  :- Body)) :-
        rdf_global_id(S0, S),
        rdf_global_id(P0, P),
        rdf_global_id(O0, O).

%should be elsewhere
n3_load(File) :-
	tokenise(File,Tokens),
	phrase(n3_dcg:document('',Doc),Tokens),
	forall(member(rdf(A,B,C,D),Doc),rdf_db:rdf_assert(A,B,C,D)).

/**
 * Top-level predicate, encapsulating the N3 entailment
 */
rdf(S,P,O) :-
	rdf_with_formulae(S,P,O),
	\+in_formulae(S,P,O).
in_formulae(S,P,O) :-
	universal(S);universal(P);universal(O). %loose...
in_formulae(_,P,_) :-
	rdf_db:rdf_global_id(log:implies,D),
	P=D.

/**
 * If you want to get back the RDF representation
 * of the formulae (see rdf_dcg.pl) along with 
 * the results of the 
 * entailment, this one's for you
 */
rdf_with_formulae(S,P,O) :-
	copy_term(rdf(S,P,O),rdf(SQ,PQ,OQ)),
	prove_triple(rdf(SQ,PQ,OQ),rdf(SS,PP,OO),Bindings),
	%writeln(Bindings),
	check(Bindings),
	replace(rdf(SS,PP,OO),Bindings,rdf(S,P,O)),
	nonvar(S),nonvar(P),nonvar(O).

/**
 * Builtin support
 */
prove_triple(rdf(S,P,O),rdf(SS,PP,OO),B) :-
	builtin(P,PlPred), %!
	convert(rdf(S,P,O),rdf(SS,PP,OO),Args,B),
	catch(apply(PlPred,Args),_,fail).
/**
 * Standard entailment
 */
prove_triple(rdf(S,P,O),rdf(SS,PP,OO),Bindings) :-
	match(rdf(S,P,O),rdf(SS,PP,OO),B1,Context),
	prove(Context,B2),
	flatten([B1,B2],Bindings).

/**
 * Binding triples that could be equivalent in
 * a given context Context,
 * according to the substitutions mentioned in B
 */
match(rdf(S,P,O),rdf(SS,PP,OO),B,Context) :-
	rdf_db:rdf(SS,PP,OO,Context), %slow
	match_node(S,SS,B1),
	match_node(P,PP,B2),
	match_node(O,OO,B3),
	flatten([B1,B2,B3],B).


match_node(N,N,[]).
match_node(N,NN,[match(N,NN)]) :-
	rdf_db:rdf_is_bnode(NN),
	N\=NN.
match_node(N,NN,[match(N,NN)]) :-
	rdf_db:rdf_is_bnode(N),
	N\=NN.


/**
 * Proves a context (eg. proves the body when wanting
 * to prove something in the head of a rule)
 */
%prove(Context,_) :- rdf_db:rdf(Context,log:implies,_),!,fail.
prove(Context,Bindings) :-
	rdf_db:rdf(BodyC,log:implies,Context),!,
	prove_body(BodyC,Bindings).
prove(_,[]).
prove_body(Context,Bindings) :-
	findall(Bindings,
		(
			rdf_db:rdf(S,P,O,Context),
			prove_triple(rdf(S,P,O),_,Bindings)
		),
		Bss
	),
	flatten(Bss,Bindings).


/**
 * This should be better written.
 * This is an achor, to check wether we have some inconsistencies
 * in the binding. I'll recode it when I find an example
 * which needs it.
 */
%check(Bindings) :-
%	member(match(_,G),Bindings),G\=literal(_),
%	rdf_db:rdf(_,_,_,G),!,
%	fail.
%check(Bindings) :-
%	member(match(V,BNode),Bindings),
%	\+var(V),
%	existential(BNode),!,
%	fail.
check(_).	


/**
 * Does the substitutions, according to 
 * a set of bindings and an original triples.
 *
 * No support for transitive substitution, so far
 */
replace(rdf(S,P,O),Bindings,rdf(SS,PP,OO)) :-
	%writeln(binding(rdf(S,P,O),rdf(SS,PP,OO))),
	replace_n(S,Bindings,SS),
	replace_n(P,Bindings,PP),
	replace_n(O,Bindings,OO).
replace_n(S,Bindings,SS) :-
	member(match(SS,S),Bindings).
replace_n(SS,Bindings,S) :-
	member(match(SS,S),Bindings).
replace_n(S,_,S).


/**
 * Check the quantification of a variable
 */
existential(Node) :-
	quantification(Node,Q),
	Q=existential.
universal(Node) :-
	quantification(Node,Q),
	Q=universal.
quantification(Node,universal) :-
	rdf_db:rdf_is_bnode(Node),
	atom_concat(_,'_uqvar',Node),!.
quantification(Node,existential) :-
	rdf_db:rdf_is_bnode(Node).

