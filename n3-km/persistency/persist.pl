:- module(persist,[bin_db/1,assert_tabled/1,tabled/1,check/1,persist/2,s_table/1]).


:- use_module(library('semweb/rdf_db')).
:- use_module('../swiaudiodata/audiodata').

:- rdf_register_ns(t,'http://purl.org/ontology/tabling/').


bin_db(bin_db).

:- 
	bin_db(DB),
	(exists_directory(DB) -> true; make_directory(DB)).


tabled(Predicate) :-
	rdf_db:rdf(Predicate,rdf:type,t:'TabledPredicate').
:- rdf_meta assert_tabled(r).
assert_tabled(Predicate) :-
	rdf_db:rdf_assert(Predicate,rdf:type,t:'TabledPredicate').

check(rdf(S,P,O)) :-
	rdf_db:rdf(S,P,O).
persist(rdf(S,P,O),rdf(SS,P,OO)) :-
	handle_list(S,SS),
	handle_list(O,OO),
	rdf_db:rdf_assert(SS,P,OO,persist).

handle_list(A,A) :- \+is_list(A),!,persist_node(A).
handle_list([],'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil').
handle_list([H|T],Id) :-
	rdf_bnode(Id),
	rdf_assert(Id,rdf:type,rdf:'List',persist),
	rdf_assert(Id,rdf:first,H,persist),
	persist_node(H),
	handle_list(T,Id2),
	rdf_assert(Id,rdf:rest,Id2,persist).

persist_node(N) :-
	atomic(N),
	atom_concat('__data',_,N),
	bin_db(DB),
	format(atom(File),'~w/~w',[DB,N]),
	writeln(data_out(N,File)),!,
	data_out(N,File).
persist_node(_).


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


