:- module(n3_entailment,[n3_load/1,compile/0]).

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
 * TODO:
 *    - The biggest limitation is perhaps the lack of recursive rule
 * 	handling (see examples/same.n3), which results
 *	 in a stack overflow. Will I have to implement
 * 	a XSB-style tabling system?
 *    - The second annoying thing is that, when deriving all possible
 *      entailments, all the registered builtins are triggered (is 
 *      that a bug or a feature?)
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
:- use_module(library('semweb/rdfs'),
	      [ rdfs_list_to_prolog_list/2 ]).
:- use_module(namespaces).
:- use_module(builtins).
:- use_module('persistency/persist').

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

:- dynamic rdf_e/3.


/**
 * Top-level predicate
 * This predicate hides entailment
 * triggered by N3 formulae, without
 * giving access to the formulae themselves
 * (therefore hiding their "RDF" representation).
 */
rdf(S,P,O) :-
	rdf_s(S,P,O),
	\+in_formulae(rdf(S,P,O)).
	%\+list(S),\+list(P),\+list(O).

/**
 * Second-level predicate
 * This also holds "RDF" representation
 * of formulae, and handles 
 * owl:sameAs
 */
% special handling of owl:sameAs
rdf_s(S,P,O) :-
	sameAs(S,SS),
	rdf_e(SS,P,O),
	P\='http://www.w3.org/2002/07/owl#sameAs'.
rdf_s(S,P,O) :-
	sameAs(O,OO),
	rdf_e(S,P,OO),
	P\='http://www.w3.org/2002/07/owl#sameAs'.
rdf_s(S,P,O) :-
	rdf_e(S,P,O).

/**
 * And third level predicate. Just do the
 * N3/builtin entailment
 */
rdf_e(S,P,O) :-
	rdf_b(S,P,O).



/**
 * Finally... Fourth level predicate, handling
 * calls to the backing rdf store, and builtins
 */
:- dynamic rdf_b/3.
rdf_b(S,P,O) :-
	rdf_l(S,P,O).
rdf_b(S,P,O) :-
	\+list(S),\+list(O),
	rdf_db:rdf(S,P,O),
	mem_load(S),mem_load(O).

/**
 * Compiling predicates.
 * This translates available in-store N3 rules
 * to nice and fast Prolog code (hopefully :) )
 *
 * Two steps:
 *   1- Compile builtins (translate a rdf_e/3 query to 
 *   calling an arbitrary Prolog predicate, as defined in 
 *   builtins.pl
 *   2- Compile formulae
 */
compile :- compile_builtins,compile_rules.
compile_builtins :-
	forall(builtin(P,PlPred),
		(
			
			format(user_error, 'DEBUG: Asserting ~w :- ~w\n',[rdf_b(S,P,O),(convert(S,O,Args,B),merge_bindings(B),catch(apply(PlPred,Args),_,fail))]),
			%assert(':-'(rdf_b(S,P,O),(tabled(P),check(rdf(S,P,O)),!,format(user_error,'DEBUG: Retrieving ~w\n',[rdf(S,P,O)])))), %only for det predicates
			assert(':-'(rdf_b(S,P,O),(\+tabled(P),!,rdf_b2(S,P,O)))),
			assert(':-'(rdf_b(S,P,O),(tabled(P),\+check(rdf(S,P,O)),copy_term((S,O),(S2,O2)),format(user_error,'DEBUG: Evaluating ~w\n',[rdf(S,P,O)]),rdf_b2(S2,P,O2),!,persist(rdf(S2,P,O2),rdf(S,P,O))))),
			assert(':-'(rdf_b2(S,P,O),(Args=[S,O],format(user_error,'DEBUG: ~w\n',[apply(PlPred,Args)]),catch(apply(PlPred,Args),_,fail))))
		)
	).
compile_rules :-
	forall(
		implies(Body,Head),
		(
			n3_pl(Head,PredListH,Bindings1),
			n3_pl(Body,PredListB,Bindings2),
			append(Bindings1,Bindings2,Bindings),
			merge_bindings(Bindings),
			list_to_conj(PredListB,PlB),
			forall(member(rdf_e(S,P,O),PredListH),
				(
					format(user_error,'DEBUG: Asserting ~w :- ~w\n',[rdf_e(S,P,O),(PlB)]),
					assert(':-'(rdf_e(S,P,O),(PlB,check(S,P,O))))
				))
		)
	).

/**
 * Just a dummy predicate checking wether all three given
 * variables are instantiated
 */
check(S,P,O) :-
	nonvar(S),nonvar(P),nonvar(O).

/**
 * Is the given triple part of a N3 forumlae?
 *
 * TODO: to enhance this... this is the crappiest
 * test ever
 */
in_formulae(rdf(S,P,O)) :-
	%(rdf_db:rdf(S,P,O,G),\+((existential(S),existential(P),existential(O))),rdf_db:rdf_is_bnode(G),!);
	(universal(S),!);(universal(P),!);(universal(O),!);
	P='http://www.w3.org/2000/10/swap/log#implies'. %loose
in_formulae(_) :- fail.

/**
 * Get back two named graph identifiers linked
 * together through log:implies
 */
implies(Body,Head) :-
	rdf_db:rdf(Body,log:implies,Head).

/**
 * Get back two named considered as equal (owl:sameAs)
 */
sameAs(A,B) :-
	\+list(A),\+list(B),
	rdf_db:rdf(A,owl:sameAs,B).
sameAs(A,B) :-
	\+list(A),\+list(B),
	rdf_db:rdf(B,owl:sameAs,A).

/**
 * Convert a N3 graph to a bag of Prolog terms (RDF lists are converted to Prolog lists)
 */
n3_pl(Head,PredList,Bindings) :-
	findall(rdf_e(S,P,O),rdf_db:rdf(S,P,O,Head),Triples),
	n3_pl2(Triples,PredList,Bindings).
n3_pl2([],[],[]).
n3_pl2([rdf_e(_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',_)|T],T2,B) :-
	!,n3_pl2(T,T2,B).
n3_pl2([rdf_e(_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',_)|T],T2,B) :-
        !,n3_pl2(T,T2,B).
n3_pl2([rdf_e(_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#List')|T],T2,B) :-
        !,n3_pl2(T,T2,B).
n3_pl2([rdf_e(S,P,O)|T],[rdf_e(SS,PP,OO)|T2],Bindings) :-
	convert_n(S,SS,B1),
	convert_n(P,PP,B2),
	convert_n(O,OO,B3),
	flatten([B1,B2,B3],BH),
	n3_pl2(T,T2,BT),
	append(BH,BT,Bindings).

convert_all([],[],[]).
convert_all([H|T],[H2|T2],Bindings) :-
        convert_n(H,H2,BH),
        convert_all(T,T2,BT),
        append(BH,BT,Bindings).
convert_n(S,SL,Bindings) :-
        atomic(S),
        rdfs_list_to_prolog_list(S,SLT),!,
        convert_all(SLT,SL,Bindings).
convert_n(S,T,[binding(S,T)]) :-
        universal(S),!.
%convert_n(S,T,[binding(S,T)]) :-
%	existential(S),!.
convert_n(S,S,[]).

/**
 * Given a list of terms bindings(Node,Term), make
 * sure all pairs of Terms are equal
 */
merge_bindings([]).
merge_bindings([binding(Node,Term)|T]) :-
	associate(binding(Node,Term),T),
	merge_bindings(T).

associate(_,[]).
associate(binding(Node,Term),[binding(Node,Term)|T]) :-
	!,
	associate(binding(Node,Term),T).
associate(binding(Node,Term),[_|T]) :-
	associate(binding(Node,Term),T).



/**
 * Cheap utils
 */

list_to_conj([H],H) :- !.
list_to_conj([H,T],(H,T)) :- !.
list_to_conj([H|T],(H,T2)) :-
	list_to_conj(T,T2).


pl_list_to_rdf_list([H],[rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H3),rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil')|Triples],H2) :- is_list(H),pl_list_to_rdf_list(H,Triples,H3),!.
pl_list_to_rdf_list([H],[rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H),rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil')],H2) :- !.
pl_list_to_rdf_list([H|T],[rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H3),rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',T2)|Triples],H2) :-
	is_list(H),!,
        pl_list_to_rdf_list(H,Triples1,H3),
        pl_list_to_rdf_list(T,Triples2,T2),
        append(Triples1,Triples2,Triples).
pl_list_to_rdf_list([H|T],[rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H),rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',T2)|Triples],H2) :-
	pl_list_to_rdf_list(T,Triples,T2).


/**
 * A couple of list-related builtins 
 */
rdf_l(S,P,O) :-
        S\=[_|_],P\=[_|_],O\=[_|_],
        rdf_db:rdf(S,P,O).
rdf_l(_,P,_) :- (P\=='http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',P\=='http://www.w3.org/1999/02/22-rdf-syntax-ns#first'),!,fail.
rdf_l(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',L) :-
	S = [L|_].
rdf_l(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil') :- S=[_].
rdf_l(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',L) :-
	\+L=='http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',
	S = [_|L].
	
list(L) :- nonvar(L),L=[_|_].


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
        Node\=[_|_],
	rdf_db:rdf_is_bnode(Node),
        atom_concat(_,'_uqvar',Node),!.
quantification(Node,existential) :-
	Node\=[_|_],
        rdf_db:rdf_is_bnode(Node).


/**
 * We'll know register the rdf/3 predicate to 
 * be used as an entailment module within the
 * SeRQL SWI Semantic Web server.
 */


                 /*******************************
                 *             REGISTER         *
                 *******************************/

:- multifile
        serql:entailment/2.

serql:entailment(n3, n3_entailment).


