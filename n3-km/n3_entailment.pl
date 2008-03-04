:- module(n3_entailment,[init_persistency/0,n3_load/1,compile/0]).

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
		rdf_bnode/1,
		rdf_is_bnode/1,
                rdf_subject/1,
                rdf_equal/2
              ]).
:- use_module(library('semweb/rdfs'),
	      [ rdfs_list_to_prolog_list/2 ]).
:- use_module(namespaces).
:- use_module(builtins).
:- use_module('persistency/persist').
:- use_module('persistency/tmp').

term_expansion((rdf(S0, P0, O0) :- Body),
               (rdf(S,  P,  O)  :- Body)) :-
        rdf_global_id(S0, S),
        rdf_global_id(P0, P),
        rdf_global_id(O0, O).

%should be elsewhere
n3_load(File) :-
	tokenise(File,Tokens),
	absolute_file_name(File,Absolute),
	format(atom(Base),'file://~w',[Absolute]), % That's probably not right
	phrase(n3_dcg:document(Base,Doc),Tokens),
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
	copy_term([S,O],[S2,O2]),atomic(P), rdf_l(S2,P,O2),
	S2=S,O2=O.
	%(list_id(S2,S)->true;S=S2),
	%(list_id(O2,O)->true;O=O2).
rdf(S,P,O) :-
	format(user_error,'DEBUG: Top level - ~w\n',[rdf(S,P,O)]),
	copy_term([S,O],[S2,O2]),
	((rdf_is_bnode(S2),list_id(S3,S2))->true;S3=S2),((rdf_is_bnode(O2),list_id(O3,O2))->true;O3=O2),
	rdf_s(S3,P,O3),
	%\+in_formulae(rdf(S2,P,O2)),
	%free_variables(rdf(S3,P,O3),Vars),bnodes(Vars),
	format(user_error,'DEBUG: Matching triple at rdf/3 level - ~w\n',[rdf(S3,P,O3)]),
	(list_id(S3,S)->true;S=S3),
	(list_id(O3,O)->true;O=O3).
	%\+list(S),\+list(P),\+list(O).
rdf(S,P,O) :-
	list(Node,List),
	%\+(in_formulae(rdf(List,_,_));in_formulae(rdf(_,_,List))),
	pl_list_to_rdf_list(List,Triples,Node),
	(pl_rdf_l(List,Triples) -> true; (free_variables(Triples,Vars),bnodes(Vars),assert(pl_rdf_l(List,Triples)))),
	member(rdf(S,P,O),Triples).
bnodes([]).
bnodes([H|T]) :-
	rdf_bnode(H),
	bnodes(T).

:- dynamic pl_rdf_l/2.


/**
 * Second-level predicate
 * This also holds "RDF" representation
 * of formulae, and handles 
 * owl:sameAs
 */
% special handling of owl:sameAs
rdf_s(S,P,O) :-
	format(user_error,'DEBUG: SameAs handling - rdf_s/3\n',[]),
	%nonvar(S),
	sameAs(S,SS),
	rdf_e(SS,P,O),
	P\='http://www.w3.org/2002/07/owl#sameAs'.
rdf_s(S,P,O) :-
	%nonvar(O),
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
	format(user_error,'DEBUG: Entailment rules - ~w/3\n',[rdf_e(S,P,O)]),
	rdf_b(S,P,O).



/**
 * Finally... Fourth level predicate, handling
 * calls to the backing rdf store, and builtins
 */
:- dynamic rdf_b/3.
rdf_b(S,P,O) :-
	format(user_error,'DEBUG: Builtin handling - ~w\n',[rdf_b(S,P,O)]),
%	rdf_l(S,P,O).
%rdf_b(S,P,O) :-
	%\+list(S),\+list(O),
	rdf_core(S,P,O,_),
	mem_load(S),mem_load(O).


/**
 * Lowest level RDF access - just adds a list interpretation layer on top
 * of rdf_db:rdf/3
 */
rdf_core(S,P,O,persist) :-
	format(user_error,'DEBUG: Core rdf query - ~w\n',[rdf_core(S,P,O)]),
	rdf_tmp(S,P,O).
rdf_core(S,P,O,G) :-
	copy_term([S,O],[S2,O2]),
	((list(S2))->true;S4=S2),((list(O2))->true;O4=O2),
	rdf_db:rdf(S4,P,O4,G),\+rdf_is_bnode(G),
	P\='http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',P\='http://www.w3.org/1999/02/22-rdf-syntax-ns#first',O4\='http://www.w3.org/1999/02/22-rdf-syntax-ns#List',P\='http://www.w3.org/2000/10/swap/log#implies',
	get_list(S4,S),
	get_list(O4,O3),
	(O3=[]->O='http://www.w3.org/1999/02/22-rdf-syntax-ns#nil';O=O3).

list(L) :- nonvar(L),L=[_|_].


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
			assert(':-'(rdf_b(S,P,O),(\+tabled(P),((atomic(S),list(S,SS))->true;S=SS),((atomic(O),list(O,OO))->true;O=OO),rdf_b2(SS,P,OO)))),
			assert(':-'(rdf_b(S,P,O),(tabled(P),((atomic(S),list(S,SS))->true;S=SS),((atomic(O),list(O,OO))->true;O=OO),\+rdf_core(SS,P,OO,persist),format(user_error,'DEBUG: Evaluating ~w\n',[rdf(SS,P,OO)]),!,rdf_b2(SS,P,OO),persist(rdf(SS,P,OO))))),
			assert(':-'(rdf_b2(S,P,O),(format(user_error,'DEBUG: ~w\n',[apply(PlPred,[S,O])]),catch(apply(PlPred,[S,O]),_,fail))))
		)
	).
compile_rules :-
	forall(
		implies(Body,Head),
		(
			n3_pl(Head,PredListH,Bindings1,BNodes1),
			n3_pl(Body,PredListB,Bindings2,BNodes2),
			append(BNodes1,BNodes2,BNodes),
			append(Bindings1,Bindings2,Bindings),
			merge_bindings(Bindings),
			list_to_conj(PredListB,PlB),
			forall(member(rdf_e(S,P,O),PredListH),
				(
					format(user_error,'DEBUG: Asserting ~w :- ~w\n',[rdf_e(S,P,O),(PlB)]),
					assert(':-'(rdf_e(S,P,O),(PlB,handle_bnodes(BNodes,PredListH,PlB),writeln(rdf_e(S,P,O)),check(S,P,O))))
				))
		)
	).

:- dynamic bas/3. %FIXME - that's wrong
handle_bnodes(BN,PlH,PlB) :-
	format(user_error,'DEBUG: ~w\n',[handle_bnodes(BN,PlH,PlB)]),
	bas(_,PlH,PlB),!.
handle_bnodes(BNodes,PlH,PlB) :-
	check_bnodes(BNodes),
	free_variables(PlH,Bs),
	bnodes(BNodes,Bs),
	forall(member(bnode(B),BNodes),(\+((bas(B,_,PlB2),PlB\=PlB2)),assert(bas(B,PlH,PlB)))).

check_bnodes([]).
check_bnodes([bnode(H)|T]) :-
	var(H),
	%(rdf_is_bnode(H),\+list(H,_))),
	check_bnodes(T).

bnodes(_,[]).
bnodes(BNodes,[H|T]) :-
	s_member(bnode(H),BNodes),
	rdf_bnode(H),
	bnodes(BNodes,T).

s_member(_,[]) :- !, fail.
s_member(A,[H|_]) :-
	A==H,!.
s_member(A,[_|T]) :-
	s_member(A,T).


%handle_bnodes(rdf(S,P,O),PlH,PlB) :-
%	member(rdf_e(S,P,O),PlH),
%	bas(PlH,PlB),
%	ground(rdf(S,P,O)),!.
%handle_bnodes(rdf(S,P,O),PlH,PlB) :-
%	member(rdf_e(S,P,O),PlH),
%	bas(PlH,PlB),!,
%	free_variables(rdf(S,P,O),Bs),
%	retractall(bas(PlH,PlB)),
%	bnodes(Bs),
%	assert(bas(PlH,PlB)).
%handle_bnodes(rdf(S,P,O),PlH,PlB) :-
%	member(rdf_e(S,P,O),PlH),
%	free_variables(rdf(S,P,O),Bs),
%	bnodes(Bs),
%	 assert(bas(PlH,PlB)).

%handle_bnodes(rdf(S,P,O),PlH,PlB) :-
%	format(user_error,'DEBUG: Handling blank nodes for ~w\n',[PlB]),
%	ground(PlB),
%	bas(PlH,PlB),
%	retractall(bas(PlH,PlB)),
%	free_variables(rdf(S,P,O),BNodes),
%	bnodes(BNodes),
%	assert(bas(PlH,PlB)),
%	!.
%handle_bnodes(rdf(S,P,O),PlH,PlB) :-
%	free_variables(rdf(S,P,O),BNodes), 
%	bnodes(BNodes),
%	assert(bas(PlH,PlB)),!.

%handle_bnodes(_).
%handle_bnodes(rdf(S,P,O),PlB) :-
%	format(user_error,'DEBUG: Handling blank nodes for ~w\n',[PlB]),
%	assoc(S,PlB),assoc(P,PlB),assoc(O,PlB).
%assoc(N,PlB) :-
%	rdf_is_bnode(N),
%	\+bas(N,_),!,
%	assert(bas(N,PlB)).
%assoc(N,PlB) :-
%	rdf_is_bnode(N),
%	bas(N,Pl2),PlB\=Pl2,!,fail.
%assoc(N,PlB) :-
%	var(N),
%	bas(N,PlB),!.
%assoc(N,PlB) :-
%	var(N),
%	rdf_bnode(N),
%	assert(bas(N,PlB)),!.
%assoc(N,_) :- \+var(N).

/**
 * Just a dummy predicate checking wether all three given
 * variables are instantiated
 */
check(_,_,_).
%check(S,P,O) :-
%	check(S),check(P),check(O).
%check(N) :-
%	nonvar(N),!.
%check(N) :- rdf_bnode(N).



/**
 * Is the given triple part of a N3 formulae?
 *
 * TODO: to enhance this... this is the crappiest
 * test ever
 */
in_formulae(rdf(S,P,O)) :-
	%(rdf_db:rdf(S,P,O,G),\+((existential(S),existential(P),existential(O))),rdf_db:rdf_is_bnode(G),!);
	%(universal(S),!);(universal(P),!);(universal(O),!);
	(rdf_core(S,P,O,G),rdf_is_bnode(G));
	(atomic(P),P='http://www.w3.org/2000/10/swap/log#implies'). %loose
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
	%\+list(A),\+list(B),
	rdf_e(A,'http://www.w3.org/2002/07/owl#sameAs',B).
sameAs(A,B) :-
	%\+list(A),\+list(B),
	rdf_e(B,'http://www.w3.org/2002/07/owl#sameAs',A).

/**
 * Convert a N3 graph to a bag of Prolog terms (RDF lists are converted to Prolog lists)
 */
n3_pl(Head,PredList,Bindings,BNodes) :-
	findall(rdf_e(S,P,O),rdf_db:rdf(S,P,O,Head),Triples),
	n3_pl2(Triples,PredList,Bindings,BNodes).
n3_pl2([],[],[],[]).
n3_pl2([rdf_e(_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',_)|T],T2,B,BN) :-
	!,n3_pl2(T,T2,B,BN).
n3_pl2([rdf_e(_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',_)|T],T2,B,BN) :-
        !,n3_pl2(T,T2,B,BN).
n3_pl2([rdf_e(_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#List')|T],T2,B,BN) :-
        !,n3_pl2(T,T2,B,BN).
n3_pl2([rdf_e(S,P,O)|T],[rdf_e(SS,PP,OO)|T2],Bindings,BNodes) :-
	convert_n(S,SS,B1,BN1),
	convert_n(P,PP,B2,BN2),
	convert_n(O,OO,B3,BN3),
	flatten([BN1,BN2,BN3],BNodesH),
	flatten([B1,B2,B3],BH),
	n3_pl2(T,T2,BT,BNodesT),
	append(BNodesH,BNodesT,BNodes),
	append(BH,BT,Bindings).

convert_all([],[],[],[]).
convert_all([H|T],[H2|T2],Bindings,BNodes) :-
        convert_n(H,H2,BH,BNodesH),
        convert_all(T,T2,BT,BNodesT),
	append(BNodesH,BNodesT,BNodes),
        append(BH,BT,Bindings).
convert_n(S,SL,Bindings,BNodes) :-
        atomic(S),
        rdfs_list_to_prolog_list(S,SLT),!,
        convert_all(SLT,SL,Bindings,BNodes).
convert_n(S,T,[binding(S,T)],[]) :-
        (universal(S)),!.
convert_n(S,T,[binding(S,T)],[bnode(T)]) :-
	existential(S),!.
convert_n(S,S,[],[]).

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
%%rdf_l(S,P,O) :-
%%        S\=[_|_],P\=[_|_],O\=[_|_],
%%        rdf_db:rdf(S,P,O).
%%rdf_l(_,P,_) :- (P\=='http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',P\=='http://www.w3.org/1999/02/22-rdf-syntax-ns#first'),!,fail.
rdf_l(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',L) :-
	(S = [L|_]; list(S,[L|_])).
rdf_l(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',L) :-
	%\+L=='http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',
	(S\=[_];var(L)),\+list(S,[_]),
	(S = [_|L]; list(S,[_|L])),!.
rdf_l(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil') :-
        (S=[_]; list(S,[_])).


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


