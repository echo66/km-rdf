:- module(n3_load,[n3_load/1]).

:- use_module(n3_dcg).

n3_load(File) :-
        tokenise(File,Tokens),
        absolute_file_name(File,Absolute),
        format(atom(Base),'file://~w',[Absolute]), % That's probably not right
        phrase(n3_dcg:document(Base,Doc),Tokens),
        forall(member(rdf(A,B,C,D),Doc),rdf_db:rdf_assert(A,B,C,D)).

n3_retract(File) :-
	graphs_in_file(File,Graphs),
	forall(member(Graph,Graphs),rdf_db:rdf_retractall(_,_,_,Graph)).

graphs_in_file(File,T) :-
	absolute_file_name(File,Absolute),
	format(atom(Base),'file://~w',[Absolute]),
	graphs_in(Base,Gs),list_to_set(Gs,T).


graphs_in(Graph,[Graph|Graphs]) :-
	findall([N|T],((rdf_db:rdf(N,_,_,Graph);(rdf_db:rdf(_,_,N,Graph),atomic(N))),graph(N),graphs_in(N,T)),Ts),
	flatten(Ts,Graphs).

graph(G) :-
	atom_concat('__bnode_graph',_,G).

