:- module(rdf_at,[
		triple/4
	,	assert_t/4
	,	delete_t/4
	,	commit/0
	,	db_time/2
	,	undo/0
]).

/**
 * Defines atomic transactions on top of
 * the SWI semweb rdf store
 */

:- use_module(library('semweb/rdf_db')).


:- dynamic inserted/4.
:- dynamic deleted/4.


%init literal search
:- use_module(library('semweb/rdf_litindex')).

%db lookup
:- rdf_meta(triple(r,r,o,r)).
triple(S,P,O,Source) :-
	(inserted(S,P,O,Source);rdf(S,P,O,Source)),
	\+deleted(S,P,O,Source).
triple(S,P,O) :-
	(inserted(S,P,O,Source);rdf(S,P,O,Source)),
	\+deleted(S,P,O,Source).

%assert rdf quads - not strict assert
:-rdf_meta(assert_t(r,r,o,r)).
assert_t(S,P,O,Source) :- triple(S,P,O,Source).
assert_t(S,P,O,Source) :-
	\+(inserted(S,P,O,Source);rdf(S,P,O,Source)),
	assert(inserted(S,P,O,Source)),increment_time(assert_t(S,P,O,Source)),
	undo_assert1(S,P,O,Source).
undo_assert1(_,_,_,_).
undo_assert1(S,P,O,Source) :-
	retract(inserted(S,P,O,Source)),decrement_time,!,fail.
assert_t(S,P,O,Source) :-
	deleted(S,P,O,Source),
	retract(deleted(S,P,O,Source)),increment_time(assert_t(S,P,O,Source)),
	undo_assert2(S,P,O,Source).
undo_assert2(_,_,_,_).
undo_assert2(S,P,O,Source) :-
	asserta(deleted(S,P,O,Source)),decrement_time,!,fail.

%delete rdf quads
:- rdf_meta(delete_t(r,r,o,r)).
delete_t(S,P,O,Source) :-
	(inserted(S,P,O,Source);rdf(S,P,O,Source)),
	\+deleted(S,P,O,Source),
	assert(deleted(S,P,O,Source)),increment_time(delete_t(S,P,O,Source)),
	undo_del(S,P,O,Source).
undo_del(_,_,_,_).
undo_del(S,P,O,Source) :-
	retract(deleted(S,P,O,Source)),decrement_time,!,fail.


%commit all ``pending'' changes, for optimisation purpose via SWI semweb RDF store
commit :-
	forall((inserted(S,P,O,Source),\+deleted(S,P,O,Source)),(rdf_assert(S,P,O,Source),retract(inserted(S,P,O,Source)))),
	forall(deleted(S,P,O,Source),(rdf_retractall(S,P,O,Source),retract(deleted(S,P,O,Source)),(inserted(S,P,O,Source)->retract(inserted(S,P,O,Source));true))).



%history of rdf store states
:- dynamic db_time/2.
db_time(0,[]). %at time 0, db is empty
increment_time(AtomicTr) :-
	db_time(N,L),
	M is N+1,
	NL = [AtomicTr|L],
	retract(db_time(N,L)),
	assert(db_time(M,NL)).
decrement_time :-
	db_time(N,L),
	M is N-1,
	L = [_|NL],
	retract(db_time(N,L)),
	assert(db_time(M,NL)).


%undo last change
undo :-
	db_time(N,[delete_t(S,P,O,Source)|T]),!,
	(deleted(S,P,O,Source) -> retract(deleted(S,P,O,Source)) ; rdf_assert(S,P,O,Source)),
	retract(db_time(N,[delete_t(S,P,O,Source)|T])),
	M is N - 1,
	assert(db_time(M,T)).
undo :-
	db_time(N,[assert_t(S,P,O,Source)|T]),!,
	(inserted(S,P,O,Source) -> retract(inserted(S,P,O,Source)) ; rdf_retractall(S,P,O,Source)),
	retract(db_time(N,[assert_t(S,P,O,Source)|T])),
	M is N - 1,
	assert(db_time(M,T)).

