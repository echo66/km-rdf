:- module(utils,[list_to_conj/2]).


list_to_conj([H],H) :- !.
list_to_conj([H,T],(H,T)) :- !.
list_to_conj([H|T],(H,T2)) :-
        list_to_conj(T,T2).

