:- module(utils,[list_to_conj/2,literal_to_node/2]).


list_to_conj([H],H) :- !.
list_to_conj([H,T],(H,T)) :- !.
list_to_conj([H|T],(H,T2)) :-
        list_to_conj(T,T2).

% subject as literals hack...
literal_to_node(Literal,Node) :- var(Literal), atom_concat('__literal',_,Node),atom_concat('__',At,Node),term_to_atom(Literal,At),!.
literal_to_node(At,At) :- atomic(At).
literal_to_node(literal(Lit),Node) :- var(Node), term_to_atom(literal(Lit),At), atom_concat('__',At,Node).

