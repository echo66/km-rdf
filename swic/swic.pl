:- module(swic,[swic/1,load/1]).

/**
 * A Semantic Web client for SWI-Prolog
 *
 * Sees the complete semantic web as a single RDF graph
 *
 * This client uses the same algorithm described in:
 * http://sites.wiwiss.fu-berlin.de/suhl/bizer/ng4j/semwebclient/#how
 *
 * It exports an operator <?>/1, which tries to instantiate a graph pattern, using retrieved
 * rdf graphs.
 *
 * Example of query :
 * <?> [rdf('http://richard.cyganiak.de/foaf.rdf#cygri',foaf:knows,Friend,G1), rdf(Friend,foaf:mbox_sha1sum,Mbox,G2), rdf(FriendURI,foaf:mbox_sha1sum,Mbox,G3), rdf(FriendURI,foaf:name,Name,G4)].
 *
 *
 * Yves Raimond, Centre for Digital Music, Queen Mary, University of London, 2006/2007
 */


:- consult(library('semweb/rdf_db')).
:- consult(library('semweb/rdf_litindex')).
:- consult(library('semweb/rdf_turtle')).
:- consult(library('semweb/rdf_cache')).
:- use_module(rdf_http_plugin).
:- use_module(config).
:- use_module(query).


:- multifile rdf_db:rdf_file_type/2.
rdf_db:rdf_file_type(xml,xml).


:- dynamic swic:loaded/1.
:- dynamic swic:failed/1.
:- dynamic swic:unsupported/1.


/**
 * Some help
 */

swic:h :-
	nl,
	writeln('-------'),
	nl,
	writeln(' - SWI semantic web client plugin help'),
	nl,
	writeln('-------'),
	nl,
	writeln(' Description - This plugin sees the whole Semantic Web as one unique graph, and dynamically
	access it to answer a query'),
	nl,
	writeln('-------'),
	nl,
	writeln('?- swic(on). and ?- swic(off). : toggle swic status on and off'),
	nl,
	writeln('? - swic:status. : display current SWIC status'),
	nl,
	writeln('-------'),
	nl,
	writeln(' Examples - '),
	writeln('        ?- <?> [rdf(''http://moustaki.org/foaf.rdf#moustaki'',B,C,G)].'),
	writeln('        will get some information about me, by dynamically grabing my FOAF file.'),
	writeln('        This FOAF file will be cached for later use'),
	nl,
	writeln('        ?- <?> [rdf(''http://moustaki.org/foaf.rdf#moustaki'',foaf:knows,C,G),rdf(C,foaf:interest,O,G2)].'),
	writeln('        will get the interests of the people I know.'),
	nl,
	writeln('        ?- <?> [rdf(''http://richard.cyganiak.de/foaf.rdf#cygri'',foaf:knows,Friend,G1), rdf(Friend,foaf:mbox_sha1sum,Mbox,G2), rdf(FriendURI,foaf:mbox_sha1sum,Mbox,G3), rdf(FriendURI,foaf:name,Name,G4)].'),
	writeln('        (same example query as for the Semantic Web Client Library)'),
	nl,
	writeln(' Extra features - '),
	writeln('       * You can load an extra RDF file into the local RDF KB using load(+Url) (example: ?- load(''http://moustaki.org/foaf.rdf''))'),
	nl,nl.

:- dynamic swic:status/1.
swic(on) :-
	retractall(swic:status(_)),
	assert(swic:status(on)).
swic(off) :-
	retractall(swic:status(_)),
	assert(swic:status(off)).

:- swic(on).


status :- 
	swic:status(Status),
	nl,write('- SWIC status: '), writeln(Status),nl.

/**
 * Caching policy
 * Handles "global caching" by default (see SWI semweb doc)
 * To make it work, just create a directory named "cache"
 *
 * You can access the URL of loaded documents using
 * --> loaded/1
 *
 * We also store dead links identified during a session, to not try access them more than one time
 * --> dead/1
 *
 * And also unparsable files:
 * --> unsupported/1
 */

:- rdf_set_cache_options([enabled(true),global_directory(cache)]).



/**
 * Main interface: the <?> operator tries to instantiate a graph pattern using
 * RDF graphs retrieved on the semantic-*web*
 *
 * 1st step: fetch relevant information, using the fetch/3 predicate
 * 2nd step: actually instantiate the graph pattern
 */
query:'<?>'(List) :-
	swic:status(on),
	fetch(List,[]),
	query(List).

fetch([],_).
fetch([rdf(A,B,C,_)|T],Query) :-
	findall(rdf(A,B,C),query(Query),Results),
	forall(member(rdf(A,B,C),Results),
	(format(' - Fetching ~w\n',[rdf(A,B,C)]),
	fetch(A,B,C),
	NewQuery = [rdf(A,B,C)|Query],
	fetch(T,NewQuery))).
fetch([rdf(A,B,C)|T],Query) :-
	query(Query),
	format(' - Fetching ~w\n',[rdf(A,B,C)]),
	fetch(A,B,C),
	NewQuery = [rdf(A,B,C)|T],
	fetch(T,NewQuery).

query([]).
query([rdf(A,B,C,G)|T]) :-
	rdf_global_id(A,AX),rdf_global_id(B,BX),rdf_global_id(C,CX),
	rdf(AX,BX,CX,G),
	query(T).
query([rdf(A,B,C)|T]) :-
	rdf_global_id(A,AX),rdf_global_id(B,BX),rdf_global_id(C,CX),
	rdf(AX,BX,CX),
	query(T).

/**
 * Fetch graphs related to a particular triple pattern
 * Uses the same algorithm as described in 
 * http://sites.wiwiss.fu-berlin.de/suhl/bizer/ng4j/semwebclient/#how
 *
 * The maximum number of retrieval step is: max_retrieval_step(N)
 *
 * fetch(?Subject,?Predicate,?Object)
 */
:- rdf_meta fetch(r,r,t).


fetch(A,_,_) :-
	nonvar(A),
	rdf_global_id(A,AA),
	load(AA),
	fail.
fetch(_,B,_) :-
	nonvar(B),
	rdf_global_id(B,BB),
	load(BB),
	fail.
fetch(_,_,C) :-
	nonvar(C),
	\+C=literal(_),
	rdf_global_id(C,CC),
	load(CC),
	fail.
fetch(_,_,_) :- join_load_threads,fail.
fetch(A,B,C) :-
	rdf_global_id(A,AA),rdf_global_id(B,BB),rdf_global_id(C,CC),
	iter(fetch(AA,BB,CC),1),
	fail.
fetch(_,_,_) :- join_load_threads.


iter(_,N) :-
	max_retrieval_steps(M),
	N > M,!.
iter(fetch(A,B,C),N) :-
	format(' - Fetching ~w ~w ~w, step ~w\n',[A,B,C,N]),
	findall(Free,(free_variables(rdf(A,B,C),Free),rdf_db:rdf(A,B,C)),NewVariables2),!,
	flatten(NewVariables2,NewVariables3),list_to_set(NewVariables3,NewVariables),writef(' - Looking up information for: %w \n',[NewVariables]),
	forall((member(R,NewVariables),\+R=literal(_)),(load(R))),
	M is N + 1,
	iter(fetch(A,B,C),M).

join_load_threads :-
	nl,writeln(' - Joining all threads...'),nl,
	forall((current_load_thread(Id,Url),current_thread(Id,_)),(
			writef(' - Joining thread %w \n',[Id]),
			(thread_join(Id,Status),retract(current_load_thread(Id,Url))),
			(
				(Status=true,writef(' - Loaded %w \n',[Url]),assert(swic:loaded(Url)));
				(Status=false,write(' - Non supported format: %w \n',[Url]),assert(swic:unsupported(Url)));
				(Status=exception(Exc),writeln(Exc),assert(swic:failed(Url)))
			)
		)).


/**
 * Get resource's URI, or get another resource, linked to this URI through
 * rdfs:seeAlso
 *
 * load(+URI)
 */

load(literal(_)) :- !,fail.
load(Uri) :- %writeln(Uri),
	uri(Uri),
	atomic(Uri),
	toDereference(Uri,Url),
	\+swic:failed(Url),
	\+swic:unsupported(Url),
	\+loaded(Url), %Mmmmm...
	writef(' - Loading graph %w...\n',[Url]),
	rdf_load_t(Url),
	fail.
load(Uri) :-
	atomic(Uri),
	findall(Y,rdf(Uri,rdfs:seeAlso,Y),Ys),
	forall(member(Y,Ys),load(Y)),
	fail.
load(_).

:- dynamic current_load_thread/2.
rdf_load_t(A) :-
        findall(Id,current_load_thread(Id,_),Bag),
	length(Bag,N),
	config:max_threads(Max),
	N<Max-1,
	!,
	load_thread(A).
rdf_load_t(A) :-
	join_load_threads,
	rdf_load_t(A).


load_thread(Url) :-
	thread_create(load_in(Url),Id,[]),
	writef(' - Created thread %w to load %w \n',[Id,Url]),
	assert(current_load_thread(Id,Url)).

load_in(Url) :-
	catch((rdf_db:rdf_load(Url),assert(swic:loaded(Url))),E,(writeln(E),assert(swic:failed(Url)))).


toDereference([],[]).
toDereference([Uri|T1],[Url|T2]) :-
	toDereference(Uri,Url),
	toDereference(T1,T2).
toDereference(FOAF,'http://xmlns.com/foaf/0.1/') :-
        atomic(FOAF),
	atom_concat('http://xmlns.com/foaf/0.1/',_,FOAF),!.

toDereference(URI,URL) :-
	atomic(URI),
	parse_url(URI,[protocol(P),host(H),port(Port),path(Path)|_]),
	sformat(S,'~w://~w:~w~w',[P,H,Port,Path]),
	string_to_atom(S,URL).
toDereference(URI,URL) :-
        atomic(URI),
	parse_url(URI,[protocol(P),host(H),path(Path)|_]),
        sformat(S,'~w://~w~w',[P,H,Path]),
        string_to_atom(S,URL).


/**
 * Cheap utils
 */

uri(Uri) :-
	\+atom_concat('__',_,Uri).

