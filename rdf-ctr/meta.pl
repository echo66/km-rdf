:- module(meta,[
		op(1150,fx,mode)
	,	(mode)/3
	,	match/4
	,	match_pre/3
	,	get_mode_pre_post/3
	,	check_mode/2
	]).


:- op(1150,fx,mode).
:- module_transparent (mode)/3.


/**
 * Some meta information about transactions - mode and determinism handling
 */


match_pre(Head,Post,Det) :-
	match(Head,Pre,Post,Det),
	check_mode(Head,Pre).

match(Head,Pre,Post,Det) :-
	functor(Head,Pred,Arity),
	mode(Pred/Arity,PredMode,Det),
	get_mode_pre_post(PredMode,Pre,Post).

mode(Pred/Arity,PredMode,Determinism) :- 
	(mode PredMode is Determinism),
	functor(PredMode,Pred,Arity).

get_mode_pre_post(PredMode,PreInst,PostInst) :-
	PredMode=..[Pred|ArgMode],
	pre_post(ArgMode,PreMode,PostMode),
	PreInst=..[Pred|PreMode],
	PostInst=..[Pred|PostMode].

pre_post([],[],[]).
pre_post([H|T],[H2|T2],[H3|T3]) :-
	mode_pre_post(H,H2,H3),
	pre_post(T,T2,T3).

mode_pre_post(in,ground,ground).
mode_pre_post(out,free,ground).
mode_pre_post(out(A),free,A).
mode_pre_post(partial,partial,partial).
mode_pre_post(any,free,free).

check_mode(Head,Inst) :-
	copy_term(Head,Head2),
	Head2=..[Pred|Args],
	Inst=..[Pred|Modes],
	check_m(Args,Modes).
check_m([],[]).
check_m([H|T],[H2|T2]) :-
	inst_match(H2,H),
	check_m(T,T2).

inst_match(free,_).
inst_match(ground,A)  :- ground(A).
inst_match(partial,A) :- nonvar(A).


