:- module(rdf_e,[rdf_e/3]).

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
:- use_module(builtins).

:- dynamic rdf_e/3.


:- begin_tests(rdf_e).
test(list_to_query) :-
	rdf_query(N,[a,b,c],(rdf(N, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', a), rdf(N, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', G498), rdf(G498, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', b), rdf(G498, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', G512), rdf(G512, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', c), rdf(G512, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'), true)),!.
test(query_list) :-
	rdf_db:rdf_assert(l1,isa,list),
	rdf_db:rdf_assert(l1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',a),
	rdf_db:rdf_assert(l1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',l2),
	rdf_db:rdf_assert(l2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',b),
	rdf_db:rdf_assert(l2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),!,
	rdf_e([a,b],isa,list),!,
	rdf_db:rdf_retractall(l1,isa,list),
	rdf_db:rdf_retractall(l1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',a),
	rdf_db:rdf_retractall(l1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',l2),
        rdf_db:rdf_retractall(l2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',b),
        rdf_db:rdf_retractall(l2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil').
:- end_tests(rdf_e).


rdf_e(S,P,O) :-
	rdf_query(SS,S,Q1),
	rdf_query(OO,O,Q2),
	rdf_db:(Q1,Q2,rdf(SS,P,OO,G)),G\=_:_,
	\+rdf_db:rdf(G,_,_),\+rdf_db:rdf(_,_,G),P\='http://www.w3.org/2000/10/swap/log#implies'.
rdf_e(S,P,O) :-
        builtin(P,Pred),
        wrap_list(S,SS),
	wrap_list(O,OO),
	call(Pred,SS,OO).



rdf_query(N,N,true) :- atomic(N);var(N);N=@=literal(_). % is a RDF node?
rdf_query('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[],true) :-!.
rdf_query(LH,[H|T],(rdf(LH,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H),rdf(LH,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',LT),QT)) :-
	rdf_query(LT,T,QT).

wrap_list(S,SS) :-
	atomic(S),
	rdfs_list_to_prolog_list(S,SS),!.
wrap_list(S,S).


