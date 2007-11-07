#!/usr/local/bin/pl -s

:- use_module(ctr).
:- use_module(rdf_at).
:- use_module(ops).
:- use_module(namespaces).
:- use_module(sw_tr).
:- use_module(meta).
:- use_module(tabling).


:- dynamic success/1.
success(0).



success :-
	success(N),
	M is N + 1,
	retractall(success(_)),
	assert(success(M)).

test(' - 1st test: rollback on insert (sequential conjunction): ',(
	(trans \+((assert_t(a,b,c,d) >> assert_t(e,r,t,y) >> fail)) >> \+triple(a,b,c,d) and \+triple(e,r,t,y))
	)).

%this one seems to fail non-deterministically
test(' - 2nd test: rollback on concurrent conjunction: ',(
	\+( trans assert_t(a,b,c,d) # assert_t(e,r,t,y) # fail) >> \+triple(a,b,c,d) and \+triple(e,r,t,y)
	)).


test(' - 3rd test: mix between concurrent conjunctions and sequential conjunctions, followed by a bulk delete: ',(
	(trans ((assert_t(a,b,c,d) >> assert_t(e,r,t,y))#(assert_t(q,w,e,r) >> assert_t(e,r,t,y))) >> triple(a,b,c,d) and triple(e,r,t,y) and triple(q,w,e,r) and triple(e,r,t,y) >> <= triple(_,_,_,_) >> \+triple(_,_,_,_))
	)).


test(' - 4th test: bulk insert, check, and bulk delete: ',(
	(trans (member(A,[a,b,c]) => triple(A,z,z,z)) >> triple(a,z,z,z) and triple(b,z,z,z) and triple(c,z,z,z) >> <= triple(_,_,_,_) >> \+triple(_,_,_,_))
	)).


test(' - 5th test: tabling a determinist predicate: ',(
	(trans assert(mode sum(in,in,out) is det) and assert(sum(A,B,C) :- C is A+B) >> table sum(1,2,D) >> lookup(sum(1,2,D)))
	)).

test(' - 6th test: sequence of concurrent conjunctions: ',(
	(\+ (trans ((assert_t(a,b,c,d)#true) >> (assert_t(e,r,t,y)#true) >> fail))))).

run :-
        forall(test(Description,Test),
                ((writeln(Description), writeln(Test), call(Test) ->
                        (writeln('Succeeded...'),success);writeln('Failed...')),nl)
                        ),
        findall(T,test(_,T),B),length(B,N),
	success(K),nl,format('Passed ~w tests successfully...',[K]),nl,
        (K=N->writeln('All tests passed successfully...');writeln('Something went wrong...')).


