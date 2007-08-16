:- module(tabling,[
		op(100,fx,table)
	,	(table)/1
	,	lookup/1
	,	store/3
]).

/**
 * A module handling tabling of predicates
 * (not really XSB-style)
 */

:- op(100,fx,table).

:- module_transparent([
		(table)/1
	]).


:- use_module(ctr).

/**
 * Set base uri for all predicates defined here
 */
base_uri('http://omras2.org/user/yvesr/pred').
set_base_uri(URI) :-
	retractall(base_uri(_)),
	assert(base_uri(URI)).

/**
 * Memoise according to determinism
 */
:- use_module(meta).

%det case
table A :- match(A,_,_,det) >> lookup(A) or match_pre(A,Post,det) >> \+lookup(A) >> statistics(real_time,[Start,_]) >> A >> statistics(real_time,[_,Duration]) >> check_mode(A,Post) >> writeln(store(A,Start,Duration)),store(A,Start,Duration).

%nondet case
table A :- match(A,_,_,nondet) >> lookup(A) or match_pre(A,Post,nondet) >> ((\+lookup(A) >> statistics(real_time,[Start,_]) >> A >> statistics(real_time,[_,Duration]) >> check_mode(A,Post)) =>> store(A,Start,Duration)) >> lookup(A).


/**
 * Basic talbing utilities - store fact, retrieve fact
 * everything goes into the rdf db, so you can browse through
 * the different set of operations leading to a result
 */

:- use_module(namespaces).
:- use_module(library('semweb/rdf_db')).

lookup(A) :-
	functor(A,Pred,Arity) and pred_uri(Pred/Arity,PredURI) >>
	A =.. [Pred|Args] >>
	triple(PredURI,cm:evaluation,EvaluationURI,tabling) >>
	assign_args(EvaluationURI,1,Args).

assign_args(_,_,[]).
assign_args(EvaluationURI,ArgNum,[Arg|T]) :-
	atom_concat('arg',ArgNum,PropArg) and
	rdf_global_id(cm:PropArg,Prop)  >> 
	triple(EvaluationURI,Prop,literal(Arg),tabling) and
	N is ArgNum +1 and
	assign_args(EvaluationURI,N,T).

store(A,Start,Duration) :-
	functor(A,Pred,Arity) and pred_uri(Pred/Arity,URI) and evaluation_uri(Pred/Arity,Start,EvalURI) >> 
	assert_t(URI,cm:evaluation,EvalURI,tabling) >> 
	assert_t(URI,cm:at,literal(Start),tabling) >> assert_t(URI,cm:duration,literal(Duration),tabling) >>
	A =.. [Pred|Args] and list(Arity,ArgNumbers) >>
	((member(ArgNumber,ArgNumbers) and atom_concat('arg',ArgNumber,PropArg) and nth1(ArgNumber,Args,Arg) and rdf_global_id(cm:PropArg,Prop)) =>> assert_t(EvalURI,Prop,literal(Arg),tabling)).


/**
 * Some useful URI creation predicates
 */
evaluation_uri(Pred/Arity,Start,URI) :-
	base_uri(Base),format(atom(URI),'~w/~w/~w/~w',[Base,Pred,Arity,Start]).

pred_uri(Pred/Arity,URI) :-
	base_uri(Base),format(atom(URI),'~w/~w/~w',[Base,Pred,Arity]).


/**
 * some tools
 */
list(N,List) :-
	list(N,0,[],List).
list(N,N,L,L) :- !.
list(N,K,L,List) :-
	NK is K + 1,
	list(N,NK,[NK|L],List).

