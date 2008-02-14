:- module(persist,[commit/0,check_tmp/1,mem_load/1,bin_db/1,assert_tabled/1,just_tabled/1,cached/1,tabled/1,persist/1,s_table/1]).


:- use_module(library('semweb/rdf_db')).
:- use_module('../swiaudiodata/audiodata').

:- rdf_register_ns(t,'http://purl.org/ontology/tabling/').



bin_db(bindb).

:- 
	bin_db(DB),
	(exists_directory(DB) -> true; make_directory(DB)).

:- dynamic rdf_tmp/3.
check_tmp(rdf(S,P,O)) :-
	%rdf_db:rdf(S,P,O),!.
	rdf_tmp(S,P,O),!.

tabled(Predicate) :-
	rdf_db:rdf(Predicate,rdf:type,t:'TabledPredicate');
	rdf_db:rdf(Predicate,rdf:type,t:'CachedPredicate').
just_tabled(Predicate) :-
	rdf_db:rdf(Predicate,rdf:type,t:'TabledPredicate').
cached(Predicate) :-
	rdf_db:rdf(Predicate,rdf:type,t:'CachedPredicate').
:- rdf_meta assert_tabled(r).
assert_tabled(Predicate) :-
	rdf_db:rdf_assert(Predicate,rdf:type,t:'TabledPredicate').

persist(rdf(S,P,O)) :-
	format(user_error,'DEBUG: Persisting ~w (local cache)\n',[rdf(S,P,O)]),
	%persist_l(S),persist_l(O),
	assert(rdf_tmp(S,P,O)).

persist_l([]).
persist_l([H|T]) :-
	persist_l(H),
	persist_l(T).
persist_l(H) :- persist_node(H).
persist_l(_).
clean_l([]).
clean_l([H|T]) :-
        clean_l(H),
        clean_l(T).
clean_l(H) :- clean_node(H).
clean_l(_).


:- multifile list/2.

commit :- 
	forall(rdf_tmp(S,P,O),
		(
			(cached(P)->(((\+((rdf_tmp(_,P2,S),just_tabled(P2))),!,persist_l(S));clean_l(S)),persist_l(O));true),
			((pl_list_to_rdf_list(S,Triples1,SS),n3_entailment:list(SS,S),!);(S=SS,Triples1=[])),
			((pl_list_to_rdf_list(O,Triples2,OO),n3_entailment:list(OO,O),!);(O=OO,Triples2=[])),
			append(Triples1,Triples2,Triples),
			free_variables([Triples,SS,OO],Vars),bnode_list(Vars),
			!,rdf_assert(SS,P,OO),
			assert_all(Triples)
		)),
	retractall(rdf_tmp(_,_,_)).

bnode_list([]).
bnode_list([H|T]) :-
	rdf_bnode(H),
	bnode_list(T).

assert_all([]).
assert_all([rdf(S,P,O)|T]) :-
	rdf_assert(S,P,O),
	assert_all(T).

pl_list_to_rdf_list([H],[rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H3),rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil')|Triples],H2) :- is_list(H),pl_list_to_rdf_list(H,Triples,H3),!.
pl_list_to_rdf_list([H],[rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H),rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil')],H2) :-  !.
pl_list_to_rdf_list([H|T],[rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H3),rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',T2)|Triples],H2) :-
        is_list(H),!,
        pl_list_to_rdf_list(H,Triples1,H3),
        pl_list_to_rdf_list(T,Triples2,T2),
        append(Triples1,Triples2,Triples).
pl_list_to_rdf_list([H|T],[rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H),rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',T2)|Triples],H2) :-
	pl_list_to_rdf_list(T,Triples,T2).

%handle_list(A,A) :- \+is_list(A),!,persist_node(A).
%handle_list([],'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil').
%handle_list([H|T],Id) :-
%	rdf_bnode(Id),
%	rdf_assert(Id,rdf:type,rdf:'List',persist),
%	rdf_assert(Id,rdf:first,H,persist),
%	persist_node(H),
%	handle_list(T,Id2),
%	rdf_assert(Id,rdf:rest,Id2,persist).

persist_node(N) :-
	atomic(N),
	atom_concat('__data',_,N),
	bin_db(DB),
	format(atom(File),'~w/~w',[DB,N]),
	format(user_error,'DEBUG: Dumping ~w in ~w\n',[N,File]),
	!,
	data_out(N,File),clean_node(N).
persist_node(_).

clean_node(N) :-
        atomic(N),
        atom_concat('__data',_,N),
        format(user_error,'DEBUG: Spring cleaning of ~w\n',[N]),
        !,
        clean_data(N).
clean_node(_).

mem_load(N) :-
	atomic(N),
	atom_concat('__data_',_Id,N),
	bin_db(DB),\+active_id(N),!,
	((reserve_id(N),!);true),
	format(atom(File),'~w/~w',[DB,N]),
	format(user_error,'DEBUG: Loading node ~w from ~w\n',[N,File]),
	data_in(File,N),
	on_backtracking(N).
mem_load([]) :- !.
mem_load([H|T]) :-
	mem_load(H),!,
	mem_load(T).
mem_load(_).

on_backtracking(_).
on_backtracking(N) :- clean_data(N),!,fail.	


/**
 * In session tabling - should not really
 * be used, except for debugging purposes
 *
 * Only for deterministic predicates
 */
:- dynamic cache/1.
:- module_transparent s_table/1.
s_table(Goal) :-
	persist:cache(Goal),!.
s_table(Goal) :-
	Goal,
	assert(persist:cache(Goal)).


