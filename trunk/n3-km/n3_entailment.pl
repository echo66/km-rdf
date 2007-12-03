:- module(n3_entailment,[]).


:- use_module(n3_dcg).
:- use_module(library('semweb/rdf_db'),
              [ rdf_global_id/2,
                rdf_reachable/3,
                rdf_has/3,
                rdf_subject/1,
                rdf_equal/2
              ]).
:- use_module(namespaces).

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


rdf(S,P,O) :-
	rdf_with_formulae(S,P,O),
	\+in_formulae(S,P,O).
in_formulae(S,P,O) :-
	universal(S);universal(P);universal(O). %loose...
in_formulae(_,P,_) :-
	rdf_db:rdf_global_id(log:implies,D),
	P=D.

rdf_with_formulae(S,P,O) :-
	copy_term(rdf(S,P,O),rdf(SQ,PQ,OQ)),
	prove_triple(rdf(SQ,PQ,OQ),rdf(SS,PP,OO),Bindings),
	%writeln(Bindings),
	check(Bindings),
	replace(rdf(SS,PP,OO),Bindings,rdf(S,P,O)),
	nonvar(S),nonvar(P),nonvar(O).


prove_triple(rdf(S,P,O),rdf(SS,PP,OO),Bindings) :-
	match(rdf(S,P,O),rdf(SS,PP,OO),B1,Context),
	prove(Context,B2),
	flatten([B1,B2],Bindings).

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


check(Bindings) :-
	member(match(_,G),Bindings),G\=literal(_),
	rdf_db:rdf(_,_,_,G),!,
	fail.
check(Bindings) :-
	member(match(V,BNode),Bindings),
	\+var(V),
	existential(BNode),!,
	fail.
check(_).	


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

