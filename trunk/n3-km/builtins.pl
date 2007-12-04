:- module(builtins,[builtin/2,convert/4]).



:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_meta builtin(r,?).


convert(rdf(S,P,O),rdf(SS,P,OO),Args,Bindings) :-
	convert_node(S,SS,SL,SB),
	convert_node(O,OO,OL,OB),
	append(SL,OL,Args),
	append(SB,OB,Bindings).
convert_node(S,S,[SL],[]) :-
	rdfs_list_to_prolog_list(S,SL),!.
	%findall(t(C,B),(member(M,SL),convert(M,C,B)),T),
	%findall(C,member(t(C,_),T),SL),
	%findall(B,member(t(_,B),T),T2),
	%flatten(T2,Bindings),!.
convert_node(S,T,[T],[match(S,T)]) :-
	rdf_is_bnode(S),!.
convert_node(literal(type(_,Lit)),literal(type(_,Lit)),[SL],[]) :-
	atom_to_term(Lit,SL,[]),!.
convert_node(literal(lang(_,Lit)),literal(lang(_,Lit)),[SL],[]) :-
	atom_to_term(Lit,SL,[]),!.
convert_node(literal(Lit),literal(Lit),[SL],[]) :-
	atom_to_term(Lit,SL,[]),!.
convert_node(S,[S],[]).



builtin('http://purl.org/ontology/swi/member',list:member).



