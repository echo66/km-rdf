:- module(persist,[init_persistency/0,commit/0,bin_db/1,assert_tabled/1,tabled/1,persist/1]).


:- use_module(library('semweb/rdf_db')).
:- use_module('../swiaudiodata/audiodata').
:- use_module(rdf_e).
:- use_module(namespaces).

:- begin_tests(persist).
test(commit_triple) :-
	assert(rdf_e:cache(a,b,c)),
	commit,!,
	rdf_db:rdf(a,b,c),!,
	rdf_db:rdf_reset_db. % Ouch
:- end_tests(persist).

bin_db(bindb).

:- 
	bin_db(DB),
	(exists_directory(DB) -> true; make_directory(DB)).


tabled(Predicate) :-
	rdf_db:rdf(Predicate,rdf:type,t:'TabledPredicate').
:- rdf_meta assert_tabled(r).
assert_tabled(Predicate) :-
	rdf_db:rdf_assert(Predicate,rdf:type,t:'TabledPredicate').


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
	forall(cache(S,P,O),
		(
			persist(rdf(S,P,O)),
			((pl_list_to_rdf_list(S,Triples1,SS),!);(S=SS,Triples1=[])),
			((pl_list_to_rdf_list(O,Triples2,OO),!);(O=OO,Triples2=[])),
			append(Triples1,Triples2,Triples),
			free_variables([Triples,SS,OO],Vars),bnode_list(Vars),
			!,rdf_assert(SS,P,OO,persist),
			assert_all(Triples)
		)),
	retractall(rdf_tmp(_,_,_)).

bnode_list([]).
bnode_list([H|T]) :-
	rdf_bnode(H),
	bnode_list(T).

assert_all([]).
assert_all([rdf(S,P,O)|T]) :-
	rdf_assert(S,P,O,persist),
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

persist(rdf(S,_,O)) :-
	persist_node(S),persist_node(O).

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


on_backtracking(_).
on_backtracking(N) :- clean_data(N),!,fail.	

init_persistency :-
	forall((rdf_db:rdf(_,_,C),atomic(C),atom_concat('__data_',_,C)),reserve(C)),
	forall((rdf_db:rdf(A,_,_),atomic(A),atom_concat('__data_',_,A)),reserve(A)),
	!.
init_persistency.

reserve(N) :-
	format(user_error,'DEBUG: Reserving data ID ~w\n',[N]),
	reserve_id(N).

