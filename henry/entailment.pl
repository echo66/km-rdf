:- module(entailment,[rdf/3]).

:- use_module(rdf_e).
:- use_module(utils).
:- use_module(library('semweb/rdf_db'),[rdf_bnode/1]).


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
	rdf_e(D,'http://www.w3.org/2000/10/swap/list#in',A),!,
	memberchk(D,[a,b,c]).
test(list_in2,[]) :-
	rdf(E,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',literal(a)),
	rdf(E,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),
	rdf(A,'http://www.w3.org/2000/10/swap/list#in',E),!,
	E = [literal(a)],
	A = literal(a).
test(list_in2,[blocked('The first member/2 creates an infinity of lists => infinite loop. This test should succeed as list_in1.')]) :-
	rdf_l(D,'http://www.w3.org/2000/10/swap/list#in',A),
	rdf_l(A,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',a),
        rdf_l(A,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',B),
        rdf_l(B,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',b),
        rdf_l(B,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',C),
        rdf_l(C,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',c),
        rdf_l(C,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),!,
        memberchk(D,[a,b,c]).
test(describe_list,[]) :-
	findall(Triple,describe_list([[a],[b]],list,Triple),Triples), % Should perhaps test with rdf_l, but bnodes make it complicated
	Triples = [
		rdf(list,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',B)
	,	rdf(B,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',a)
	,	rdf(B,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil')
	,	rdf(list,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',C)
	,       rdf(C,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil')
	,	rdf(C,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',D)
	,	rdf(D,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',b)
	,	rdf(D,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil')
	].
:- end_tests(entailment).


rdf(S,P,O) :-
	copy_term((S,P,O),(SS,PP,OO)),
	entail(rdf(SS,PP,OO),rdf(SSS,PPP,OOO)),
	(is_list(SSS) -> rdf_bnode(SSSS) ; SSS=SSSS),
	(is_list(OOO) -> rdf_bnode(OOOO) ; OOO=OOOO),
	(
		((is_list(SSS);is_list(OOO)),S=SSS,P=PPP,O=OOO);
		(S=SSSS,P=PPP,O=OOOO);
		(is_list(SSS),describe_list(SSS,SSSS,rdf(S,P,O)));
		(is_list(OOO),describe_list(OOO,OOOO,rdf(S,P,O)))
	).

describe_list([H|_],S,rdf(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H)) :-
	\+is_list(H).
describe_list([_],S,rdf(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil')).
describe_list([H|_],S,Triple) :-
	is_list(H),
	rdf_bnode(SS),
	RDF = rdf(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',SS),
	(Triple = RDF; describe_list(H,SS,Triple)).
describe_list([_|T],S,Triple) :-
	T\=[],
	rdf_bnode(SS),
	RDF = rdf(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',SS),
	(Triple = RDF; describe_list(T,SS,Triple)).
	
	


entail(rdf(S,P,O),rdf(SS,P,OO)) :-
	rdf_2(S,P,O),
	% some filtering, to not make SeRQL crash with lists-as-nodes and bnodes-as-terms
	clean(S,SS),
	clean(O,OO).

clean(Bnode,NN) :-
       ground(Bnode), Bnode=individual(B,Vars),
	concat_atom([B|Vars],'_',NN),!.
clean(N,N).


rdf_2(S,P,O) :-
	ground(P),
	rdf_l(S,P,O).
rdf_2(S,P,O) :-
	rdf_e(S,P,O).


rdf_l([_],'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil').
rdf_l([H|_],'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H).
rdf_l([_|T],'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',T) :- T\=='http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'.
%rdf_l(M,'http://www.w3.org/2000/10/swap/list#in',L) :- \+var(L),member(M,L).



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

