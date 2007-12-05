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

:- dynamic rdf/3.

rdf(S,P,O) :- 
	rdf_db:rdf(S,P,O,G),
	\+in_formulae(rdf(S,P,O,G)).

compile :- compile_builtins,compile_rules.
compile_builtins.
%compile_builtins :-
%	forall(builtin(P,PlPred),
%		(
%			format('~w :- ~w\n',[rdf(S,P,O),(to_args(S,O,Args),catch(apply(PlPred,Args)))]),
%			assert(':-'(rdf(S,P,O),(to_args(S,O,Args),catch(apply(PlPred,Args)))))
%		)
%	).
compile_rules :-
	forall(
		implies(Body,Head),
		(
			n3_pl(Head,PredListH,Bindings1),
			n3_pl(Body,PredListB,Bindings2),
			writeln(Bindings1),writeln(Bindings2),
			append(Bindings1,Bindings2,Bindings),
			merge_bindings(Bindings),
			list_to_conj(PredListB,PlB),
			forall(member(rdf(S,P,O),PredListH),
				(
					format('~w :- ~w\n',[rdf(S,P,O),(PlB)]),
					assert(':-'(rdf(S,P,O),(PlB,check(S,P,O))))
				))
		)
	).

check(S,P,O) :-
	nonvar(S),nonvar(P),nonvar(O).

in_formulae(rdf(_,P,_,G)) :-
	rdf_db:rdf(G,_,_,_);
	rdf_db:rdf(_,_,G,_);
	P='http://www.w3.org/2000/10/swap/log#implies'. %loose
in_formulae(_) :- fail.

implies(Body,Head) :-
	rdf_db:rdf(Body,log:implies,Head).


n3_pl(Head,PredList,Bindings) :-
	findall(rdf(S,P,O),rdf_db:rdf(S,P,O,Head),Triples),
	n3_pl2(Triples,PredList,Bindings).
n3_pl2([],[],[]).
n3_pl2([rdf(S,P,O)|T],[rdf(SS,PP,OO)|T2],Bindings) :-
	rdf_to_pl_n(S,SS,B1),
	rdf_to_pl_n(P,PP,B2),
	rdf_to_pl_n(O,OO,B3),
	flatten([B1,B2,B3],BH),
	n3_pl2(T,T2,BT),
	append(BH,BT,Bindings).

rdf_to_pl_n(N,T,[binding(N,T)]) :-
	universal(N),!.
rdf_to_pl_n(N,N,[]).
%list


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


list_to_conj([H],H) :- !.
list_to_conj([H,T],(H,T)) :- !.
list_to_conj([H|T],(H,T2)) :-
	list_to_conj(T,T2).

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

