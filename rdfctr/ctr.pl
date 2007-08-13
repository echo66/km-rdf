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

%enable transactions - retract this term to disable them
tr.

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

A >> B :- 
	tr,
	A,
	B.

A and B :- (A, B) *-> true;(B, A). %should be A,B -> true;B,A ? perhaps with a soft cut?

A or B :- A; B.

A # B :-
	concconj_to_list((A#B),List),length(List,N),
	message_queue_create(Status),
	message_queue_create(Gstatus),
	findall(Id,(
			member(G,List),
			thread_create((
				(G,thread_send_message(Status,ok),thread_get_message(Gstatus,Msg),((Msg=allfail,fail);(Msg=allok)),!);(thread_send_message(Status,failed),fail)
			),Id,[]),
			format('created thread ~w for ~w\n',[Id,G])
		
		),Ids),
	receive_ok_failed(Status,N,_,Ok),
	(Ok=N->send_n_message(N,Gstatus,allok);send_n_message(Ok,Gstatus,allfail)),
	forall(member(Id,Ids),(
			format('joining thread ~w\n',[Id]),
			thread_join(Id,true)
		)),
	message_queue_destroy(Status),
	message_queue_destroy(Gstatus).
	

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
	GoalCSeq.

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
