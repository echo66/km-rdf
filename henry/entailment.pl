:- module(entailment,[rdf/3]).

:- use_module(rdf_e).

:- begin_tests(entailment).
test(list_construction,[]) :-
	rdf_l(A,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',a),
	rdf_l(A,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',B),
	rdf_l(B,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',b),
	rdf_l(B,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',C),
	rdf_l(C,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',c),
	rdf_l(C,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),!,
	A = [a,b,c],
	B = [b,c],
	C = [c].
test(list_in1,[]) :-
	rdf_l(A,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',a),
        rdf_l(A,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',B),
        rdf_l(B,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',b),
        rdf_l(B,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',C),
        rdf_l(C,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',c),
        rdf_l(C,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),
	rdf_l(D,'http://www.w3.org/2000/10/swap/list#in',A),!,
	memberchk(D,[a,b,c]).
test(list_in2,[blocked('The first member/2 creates an infinity of lists => infinite loop. This test should succeed as list_in1.')]) :-
	rdf_l(D,'http://www.w3.org/2000/10/swap/list#in',A),
	rdf_l(A,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',a),
        rdf_l(A,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',B),
        rdf_l(B,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',b),
        rdf_l(B,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',C),
        rdf_l(C,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',c),
        rdf_l(C,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),!,
        memberchk(D,[a,b,c]).
:- end_tests(entailment).


rdf(S,P,O) :-
	ground(P),
	rdf_l(S,P,O).
rdf(S,P,O) :-
	rdf_e(S,P,O).


rdf_l([_],'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil').
rdf_l([H|_],'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H).
rdf_l([_|T],'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',T) :- T\=='http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'.
rdf_l(M,'http://www.w3.org/2000/10/swap/list#in',L) :- member(M,L).


