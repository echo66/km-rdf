:- module(ctr,[
		'>>'/2
	,	'and'/2
	,	'or'/2
	,	'#'/2
	,	'o'/1
	,	'=>'/2
	,	'=>>'/2
	,	'=#>'/2
	,	'=&>'/2
	,	'<='/1
	,	'rs'/2
	,	'rc'/2
	,	s/0
	,	trans/1
	,	send/2
	,	peek/2
	,	receive/2
]).


:- module_transparent([
		'>>'/2
	,	'and'/2
	,	'or'/2
	,	'#'/2
	,	'o'/1
	,	'=>'/2
	,	'=#>'/2
	,	'=&>'/2
	,	'=>>'/2
	,	'<='/1
	,	'rs'/2
        ,       'rc'/2
	,	'trans'/1
	]).

/**
 * (Concurrent) transaction logic intepreter, using as atomic
 * transactions the one defined in rdf_at, operating over
 * the SWI rdf store (semweb library)
 *
 * Yves Raimond, C4DM, Queen Mary University of London, 2007
 */


:- use_module(ops).
:- use_module(rdf_at).

:- set_prolog_flag(history, 50).


%enable namespace expansion
:- rdf_meta('>>'(t,t)).
:- rdf_meta('and'(t,t)).
:- rdf_meta('or'(t,t)).
:- rdf_meta('#'(t,t)).
:- rdf_meta(o(t)).
:- rdf_meta('=>'(t,t)).
:- rdf_meta('<='(t)).
:- rdf_meta('=>>'(t,t)).
:- rdf_meta('=#>'(t,t)).
:- rdf_meta('=&>'(t,t)).
:- rdf_meta('rs'(t,t)).
:- rdf_meta('rc'(t,t)).

:- use_module(meta).
:- use_module(library('semweb/rdf_db')).

% handle thread clean up - should be used as trans a # b, for example.
(trans A) :-
	A -> (writeln(allok),allok,commit);(writeln(failing),allfail).


A >> B :- 
	A,B.

A and B :- (A, B) *-> true;(B, A). %should be A,B -> true;B,A ? perhaps with a soft cut?

A or B :- A; B.

%max_threads(1).
%A # B :-
%	concconj_to_list((A#B),List),
%	max_threads(Max),
%	cut_list(Max,List,Lists),writeln(Lists),
%	(member(L,Lists) =>> (ctr:execute_list_concurrently(L))).
:- dynamic status_queue/1.
:- dynamic feedback_queue/1.
:- dynamic waiting_thread/1.

reset_queues :-
	retractall(status_queue(Status)),
	retractall(feedback_queue(Feedback)),
	message_queue_create(Status),
	assert(status_queue(Status)),
	message_queue_create(Feedback),
	assert(feedback_queue(Feedback)).
:- reset_queues.

% only ok for deterministic predicates, for now (i would have to play with thread_exit to handle non determinism).
A # B :- 
	(concconj_to_list((A#B),List),
	execute_list_concurrently(List)).

execute_list_concurrently(List) :-
	status_queue(Status),
	feedback_queue(Feedback),
	length(List,N),
	forall(
		member(G,List),
			(thread_create((
				(catch(G,Exc,(writeln(Exc),thread_send_message(Status,failed),fail)),writeln('I did succeed'),thread_send_message(Status,ok),thread_get_message(Feedback,Msg),((Msg=allfail,fail);(Msg=allok)),!);(thread_send_message(Status,failed),fail)
			),Id,[]),
			assert(waiting_thread(Id)),
			format('- Created thread ~w for ~w\n',[Id,G])
			)
		),
	writeln(' - Checking if current concurrent conjunction succeeds'),
	receive_ok_failed(Status,N,F,Ok),writeln(N),writeln(Ok),writeln(F),
	(Ok < N -> 
		( writeln(' - At least one failure'),
		  !,fail
		  );
		( writeln(' - Concurrent Conjunction succeeded'),
		  true)
	).

allok :- 
	findall(Id,ctr:waiting_thread(Id),ThreadBag),
	length(ThreadBag,N),
	feedback_queue(Feedback),
	send_n_message(N,Feedback,allok),
	joinall.

allfail :- 
	findall(Id,ctr:waiting_thread(Id),ThreadBag),
	length(ThreadBag,N),
	feedback_queue(Feedback),
	send_n_message(N,Feedback,allfail),
	joinall,
	fail.

joinall :-
	forall(
		waiting_thread(Id),
			( thread_join(Id,_), format(' - Cleaned thread ~w\n',Id),retractall(waiting_thread(Id)))
		),
	reset_queues.


o A :- A. %does not work for now - i need to know how to stop every other threads

%bulk insert
Goal => triple(A,B,C,D) :- 
	findall(assert_t(A,B,C,D),Goal,Asserts),
	ctr:construct_seq(Asserts,Sequence),
	Sequence.

%bulk delete
<= triple(A,B,C,D) :-
	findall(delete_t(A,B,C,D),triple(A,B,C,D),Deletes),
	ctr:construct_seq(Deletes,Sequence),
	Sequence.

%forall (create and execute sequential conjunction)
Cond =>> Goal :-
	findall(Goal,Cond,Goals),
	ctr:construct_seq(Goals,GoalSeq),
	GoalSeq.

%forall (create and execute concurrent conjunction)
Cond =#> Goal :-
	findall(Goal,Cond,Goals),
	ctr:construct_cseq(Goals,GoalCSeq),
	trans(GoalCSeq).

%for all (create and execute std conjunction)
Cond =&> Goal :-
	findall(Goal,Cond,Goals),
	ctr:construct_conj(Goals,GoalConj),
	GoalConj.

%execute N time a given transaction (sequential AND)
Tr rs Time :-
	length(List,Time),
	assign_l(List,Tr),
	ctr:construct_seq(List,Conj),
	writeln(Conj),
	Conj.

%execute N time a given transaction (concurrent AND)
Tr rc Time :-
	length(List,Time),
        assign_l(List,Tr),
	ctr:construct_cseq(List,Conj),
	writeln(Conj),
        Conj.	

%communication channels - this MUST be totally broken
send(Channel,Message) :-
	message_queue_create(Channel),
	thread_send_message(Channel,Message).
peek(Channel,Message) :-
	thread_peek_message(Channel,Message).
receive(Channel,Message) :-
	thread_get_message(Channel,Message).

%utils
construct_conj([T],T).
construct_conj([H|T1],H and T2) :-
	construct_conj(T1,T2).
construct_seq([T], T).
construct_seq([H|T1],H >> T2) :-
	construct_seq(T1,T2).
construct_cseq([T],T).
construct_cseq([H|T1],H # T2) :-
	construct_cseq(T1,T2).
concconj_to_list(A,[A]) :-
	A\='#'(_,_).
concconj_to_list('#'(A,B),[A|T]) :-
	concconj_to_list(B,T).
receive_ok_failed(Status,N,Failed,Ok) :-
	receive_ok_failed(Status,N,0,0,0,Failed,Ok).
receive_ok_failed(_,N,N,Failed,Ok,Failed,Ok) :-
	!.
receive_ok_failed(Status,N,K,F,O,Failed,Ok) :-
	thread_get_message(Status,Msg),
	(Msg=failed->(NF is F + 1); NF is F),
	(Msg=ok->(NO is O + 1);NO is O),
	NK is K+1,
	receive_ok_failed(Status,N,NK,NF,NO,Failed,Ok).
send_n_message(N,Channel,Msg) :-
	send_n_message(N,0,Channel,Msg).
send_n_message(N,N,_,_) :- !.
send_n_message(N,K,Channel,Msg) :-
	thread_send_message(Channel,Msg),
	NK is K + 1,
	send_n_message(N,NK,Channel,Msg).


assign_l([],_).
assign_l([H|T],H) :-
	assign_l(T,H).

cut_list(N,List,[List]) :-
	length(List,M),
	M < N,!.
cut_list(N,List,[H|T]) :-
	length(H,N),
	append(H,Tail,List),
	cut_list(N,Tail,T).



%state
s :- 
	db_time(N,L),
	nl,
	format(' - Current database time (number of atomic changes) : ~w\n',[N]),
	(N > 0 -> (
		format(' - Last atomic change :\n',[]),L=[H|_],
		format('   ~w',[H])
		) ; true),
	nl,nl,
	writeln(' - To get a complete list of atomic changes to the database, use the db_time(-Time,-Changes) predicate'),nl,
	writeln(' - To get access to the history of issued commands, type !h.').
