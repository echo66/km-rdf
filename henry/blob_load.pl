:- module(blob_load,[mem_load/1]).

:- use_module('../swiaudiodata/audiodata').

mem_load(N) :-
        atomic(N),
        atom_concat('__data_',_Id,N),
        bin_db(DB),\+active_id(N),!,
        ((reserve(N),!);true),
        format(atom(File),'~w/~w',[DB,N]),
        format(user_error,'DEBUG: Loading node ~w from ~w\n',[N,File]),
        data_in(File,N),
        on_backtracking(N).
mem_load([]) :- !.
mem_load([H|T]) :-
        mem_load(H),!,
        mem_load(T).
mem_load(_).

reserve(N) :-
        format(user_error,'DEBUG: Reserving data ID ~w\n',[N]),
        reserve_id(N).


on_backtracking(_).
on_backtracking(N) :- clean_data(N),!,fail.
