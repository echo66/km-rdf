:- module(persist,[]).

/**
 * Handle some persistency features
 *
 */


:- use_module(library('semweb/rdf_db')).
:- use_module('../swiaudiodata/audiodata')). %why all prolog operations on blobs are defined there??


:- dynamic current_db/1.

attach_db(_) :-
	current_db(DB),!,
	format('Database ~w already in use ...',[DB]).
attach_db(DB) :-
	exists_directory(DB),!,format('Loading database ~w ...',[DB]),
	assert(current_db(DB)).
attach_db(DB) :- 
	format('Creating database ~w ...',[DB]),
	make_directory(DB),
	format(atom(Bin),'~w/bin',[DB]),
	make_directory(Bin),
	assert(current_db(DB)).


persist(rdf(S,P,O)) :-
	persist_n(S),
	persist_n(P),
	persist_n(O),
	rdf_assert(S,P,O).

persist_n(N) :-
	id_blob(N,_),!,
	data(N,Data),
	store_data(N,Data).
persist(_).


store_data(Node,Data) :-
	current_db(DB),
	format(atom(Bin),'~w/bin/~w/blob.pl',[DB,Node]),
	open(Bin,write,S,[]),
	write_canonical(S,data_n(Node,Data)),writeln(S,'.'). %this should be binary


%data/2 hook
data(ID,List) :-
	\+id_blob(ID,_),!,
	current_db(DB),
	format(atom(Bin),'~w/bin/~w/blob.pl',[DB,ID]),
	consult(Bin),
	data_n(ID,List),
	data_load(List,ID),
	retractall(data_n(_,_)).
	data(ID,List).





