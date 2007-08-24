:- module(sparql,[
		new_sparql_end_point/1
	,	new_sparql_end_point/3
	,	new_sparql_end_point/4
	,	sparql_query/2
	,	query/3
	,	query/2
	,	sparql/1
	]).

/**
 * A module handling query to SPARQL end points
 *
 * Yves Raimond, C4DM, Queen Mary, University of London
 */

:- use_module('sparql_client').
:- use_module('query').

:- dynamic sparql_end_point/4.
:- multifile sparql_end_point/4.

/**
 * Some help
 */
sparql:h :-
	nl,
        writeln('-------'),
        nl,
        writeln(' - SPARQL plugin help'),
        nl,
	writeln('-------'),
	nl,
	writeln('?- sparql(on). and ?- sparql(off). : toggle sparql client status on and off'),
	nl,
	writeln('?- sparql:status. : show sparql client status (on or off)'),
	nl,
        writeln('-------'),
        nl,
        writeln(' Description - This plugin brings the ability to query SPARQL end points'),
        nl,
        writeln(' Example - '),
	writeln('        ?- new_sparql_end_point(''http://purl.org/dbtune/sparql'').'),
	writeln('        will register a new sparql end point.'),
        writeln('        ?- <?> [rdf(''http://dbtune.org/magnatune/artist/altri_stromenti'',B,C,G)].'),
        writeln('        will get some information about that particular artist, by querying the end point, after failing to dereference the URI'),
        nl,
        writeln(' Extra features - '),
        writeln('       * You can launch a SPARQL query by using sparql_query(+Query,-Row))'),
	writeln('         This will bypass all the other plugins and allow you to use queries directly written in SPARQL'),
	nl,
	writeln(' Example - '),
	writeln('       ?- sparql_query(''select ?b ?c where {<http://dbtune.org/magnatune/resource/artist/altri_stromenti> ?b ?c}'',C).'),
        nl,nl.


:- dynamic sparql:status/1.
sparql(on) :-
	retractall(sparql:status(_)),
	assert(sparql:status(on)).
sparql(off) :-
	retractall(sparql:status(_)),
	assert(sparql:status(off)).
:-
	sparql(on).

status :-
	sparql:status(Status),
	nl,write('- SPARQL status: '), writeln(Status),nl.

/**
 * Assert a new sparql end point
 *
 * new_sparql_end_point(+EndPoint,+Options)
 */
new_sparql_end_point(Url) :-
	parse_url(Url,Parsed),
	member(host(Host),Parsed),
	member(path(Path),Parsed),
	(member(port(Port),Parsed);Port=80),
	new_sparql_end_point(Host,Port,Path).
new_sparql_end_point(Host,Port,Path) :-
	new_sparql_end_point(Host,Port,Path,[]).
new_sparql_end_point(Host,Port,Path,Options) :-
	assert(sparql:sparql_end_point(Host,Port,Path,Options)).

/**
 * Query all available end-points
 * using a prolog RDF context
 * like [rdf(A,rdf:type,C)]
 */
query:'<?>'(Context) :-
	sparql:status(on),
	query_c(Context).

query_c(Context) :-
	query_c(Context,[]).
query_c(Context,Options) :-
	context_to_sparql_q(Context,SC),
	writeln(SC),
	query(SC,Context,Options).

/**
 * Query available end points - SPARQL
 */
query(Query,PQ) :-
	query(Query,PQ,[]).
query(Query,PQ,Options) :-
	findall(swsources:sparql_end_point(Host,Port,Path,OptionsEP),sparql_end_point(Host,Port,Path,OptionsEP),EndPoints),
	nl,
        writeln('-------------'),
        nl,
        writeln(' - Executing SPARQL query against known end-points...'),
        nl,
        writeln('-------------'),nl,
	query(EndPoints,Query,PQ,Options).

/**
 * Query a set of end-points
 */
query(EndPoints,Query,PQ,Options) :-
	member(swsources:sparql_end_point(Host,Port,Path,OptionsEP),EndPoints),
	sparql_client:sparql_set_server([host(Host),port(Port),path(Path)|OptionsEP]),
	sparql_client:sparql_query(Query,Row,Options),
	provenance(PQ,sparql_end_point([host(Host),port(Port),path(Path)|OptionsEP])),
	term_variables(PQ,Vars),
	Row=..[row|Vars].

provenance([],_).
provenance([rdf(_,_,_)|T],Provenance) :- provenance(T,Provenance).
provenance([rdf(_,_,_,Provenance)|T],Provenance) :-
	provenance(T,Provenance).

sparql_query(Query,Row) :-
	findall(swsources:sparql_end_point(Host,Port,Path,OptionsEP),sparql_end_point(Host,Port,Path,OptionsEP),EndPoints),
	member(swsources:sparql_end_point(Host,Port,Path,OptionsEP),EndPoints),
	sparql_client:sparql_set_server([host(Host),port(Port),path(Path)|OptionsEP]),
	sparql_client:sparql_query(Query,Row,[]).

/**
        Tools to use to convert RDF prolog queries to a SPARQL query
        */
/**
        Prolog triple to SPARQL triple
        */
pl_to_sparql_t(rdf(A,B,C),ST) :-
	pl_to_sparql_t(rdf(A,B,C,_),ST).
pl_to_sparql_t(rdf(A2,B2,C2,_),ST) :-
        expand_goal(rdf(A2,B2,C2),rdf(A,B,C)),
        (var(A) -> (
                        term_to_atom(A,AA),
                        replace_atom('_','',AA,AAA),
                        atom_concat('?',AAA,STA)
                        );(enclose_url(A,STA))),
        (var(B) -> (
                        term_to_atom(B,BB),
                        replace_atom('_','',BB,BBB),
                        atom_concat('?',BBB,STB)
                        );(enclose_url(B,STB))),
        (var(C) -> (
                        term_to_atom(C,CC),
                        replace_atom('_','',CC,CCC),
                        atom_concat('?',CCC,STC)
                        );
                ((C=literal(type(_,_)) -> (pl_lit_to_sparql_lit(C,STC));
                        enclose_url(C,STC)))),
        atom_concat(STA,' ',T1),
        atom_concat(T1,STB,T2),
        atom_concat(T2,' ',T3),
        atom_concat(T3,STC,ST).

/**
        Convert a prolog literal to its SPARQL form
        */
pl_lit_to_sparql_lit(literal(type(Type,Lit)),SLit) :-
        sformat(SSLit,'"~w"^^<~w>',[Lit,Type]),
        string_to_atom(SSLit,SLit).


/**
        Gives a SPARQL query from a given context
        */
context_to_sparql_q(Context,Query) :-
        context_to_sparql_c(Context,SC),
        T1 = 'select * where {',
        concat_atom(SC,T2),
        atom_concat(T1,T2,T3),
        atom_concat(T3,'}',Query).


context_to_sparql_c([],[]).
context_to_sparql_c([T|Tail],[ST|Tail2]) :-
        pl_to_sparql_t(T,ST1),
        atom_concat(ST1,'.',ST),
        context_to_sparql_c(Tail,Tail2).

/**
 * Enclose URL between < and >
 */
enclose_url(Url,R) :-
        atom_concat('<',Url,T1),
        atom_concat(T1,'>',R).

/**
 * Replace a sub-atom by another in a given atom
 */
replace_atom(C,_,A,A) :-
        \+((atom_concat(_A1,A2,A),atom_concat(C,_A3,A2))).
replace_atom(C,C2,A,B) :-
        atom_concat(A1,A2,A),
        atom_concat(C,A3,A2),
        atom_concat(A1,C2,B1),
        atom_concat(B1,A3,B2),
        !,
        replace_atom(C,C2,B2,B).



