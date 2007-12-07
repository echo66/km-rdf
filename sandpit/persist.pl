:- module(persist,[]).

/**
 * Handle some persistency features
 *
 */

:- dynamic current_db/1.

attach_db(_) :-
	current_db(DB),!,
	format('Database ~w already in use ...',[DB]).
attach_db(DB) :-
	exists_directory(DB),!,format('Loading database ~w ...',[DB]),
	format(atom(Manifest),'~w/manifest.pl',[DB]),
	consult(Manifest),
	assert(current_db(DB)).
attach_db(DB) :- 
	format('Creating database ~w ...',[DB]),
	make_directory(DB),
	format(atom(Bin),'~w/bin',[DB]),
	make_directory(Bin),
	assert(current_db(DB)).


persist(rdf(S,P,O)) :-
	store(rdf(S,P,O)), %could be handled by swi rdf persistency? rdf_assert
	persist_n(S),
	persist_n(P),
	persist_n(O).

persist_n(N) :-
	blob(N),!,
	blob_data(Node,Data),
	store_data(Node,Data).
persist(_).


store(rdf(S,P,O)) :-
	current_db(DB),
	format(atom(Manifest),'~w/manifest.pl',[DB]),
	open(Manifest,append,S,[]),
	write_canonical(S,rdf(S,P,O)),writeln(S,'.').

store_data(Node,Data) :-
	current_db(DB),
	format(atom(Bin),'~w/bin/~w/blob',[DB,Node]),
	open(Bin,write,S,[]),
	write_canonical(S,data(Node,Data)),writeln(S,'.'). %this should be binary


load(rdf(S,P,O)) :-
	rdf(S,P,O),
	load_n(S),
	load_n(P),
	load_n(O).

load_n(N) :-
	current_db(DB),
	format(atom(Bin),'~w/bin/~w/blob',[DB,N]),
	exists_file(Bin),!,
	consult(Bin), %this should be binary
	data(N,Data),
	load_data(Data,N).

load_n(_).


