:- module(tmp,[get_list/2,list_id/2,list/2,check_tmp/1,rdf_tmp/3,persist/1]).

:- use_module(library('semweb/rdf_db')).

:- dynamic list/2.
list_id(L1,L2) :- % HIGHLY experimental :-)
	is_list(L1),is_list(L2),!,L1=L2.
list_id(List,Id) :-
	format(user_error,'DEBUG: Calling ~w\n',[list_id(List,Id)]),
	is_list(List),rdf_is_bnode(Id),ground(Id),ground(List),!,assert(list(Id,List)).
list_id(List,Id) :-
        rdf_is_bnode(Id),
        get_list(Id,List),List\=Id,!.
list_id(List,Id) :-
        %is_list(List),
        list(Id,List),!.
list_id(List,Id) :-
        is_list(List),
        rdf_bnode(Id),
        assert(list(Id,List)).
	


get_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.
get_list(S,[H|T]) :-
        rdf_db:rdf(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H2),
        get_list(H2,H),
        rdf_db:rdf(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',S2),
        !,get_list(S2,T).
get_list(A,A).


:- dynamic rdf_tmp/3.
check_tmp(rdf(S,P,O)) :-
        %rdf_db:rdf(S,P,O),!.
        rdf_tmp(S,P,O),!.


persist(rdf(S,P,O)) :-
        format(user_error,'DEBUG: Persisting ~w (local cache)\n',[rdf(S,P,O)]),
        %persist_l(S),persist_l(O),
        assert(rdf_tmp(S,P,O)).



