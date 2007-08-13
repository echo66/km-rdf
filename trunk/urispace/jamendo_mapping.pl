:- module(jamendo_mapping,[]).

/**
 * Jamendo mapping for 303 redirects
 */


:- use_module(mapping).


sparql_end_point('http://dbtune.org:2105/sparql/').
html_renderer('http://www4.wiwiss.fu-berlin.de/rdf_browser/?browse_uri=').
directory('http://dbtune.org/jamendo/all/').
namespace('http://dbtune.org/jamendo/').

/**
 * Redirecting doc
 */
mapping:see_other(requested_pattern([]),redirect_pattern(['http://moustaki.org/','jamendo/'])).

/**
 * Redirecting end point
 */
mapping:see_other(requested_pattern([sparql]),redirect_pattern(['http://dbtune.org:2105/sparql/'])):-!.

/**
 * Redirecting end point web interface
 */
mapping:see_other(requested_pattern([wsparql]),redirect_pattern(['http://dbtune.org:2105/'])):-!.

/**
 * Redirecting resources whithin the namespace
 */

mapping:see_other_rdf(requested_pattern(T),redirect_pattern([SparqlEndPoint,'?query=describe%20%3C',NameSpace,Path,'%3E'])) :-
	sparql_end_point(SparqlEndPoint),
	namespace(NameSpace),
	concat_atom(T,'/',Path).
mapping:see_other(requested_pattern(T),redirect_pattern([Renderer,NameSpace,Path])) :-
	html_renderer(Renderer),
	namespace(NameSpace),
	concat_atom(T,'/',Path).


/**
 * directories
 */
mapping:see_other_rdf(requested_pattern([all,artist]),redirect_pattern([SparqlEndPoint,'?query=',
	'describe%20',Concept])) :-
	sparql_end_point(SparqlEndPoint),	
	Concept='%3Chttp://purl.org/ontology/mo/MusicArtist%3E'.
mapping:see_other_rdf(requested_pattern([all,track]),redirect_pattern([SparqlEndPoint,'?query=',
	'describe%20',Concept])) :-
	sparql_end_point(SparqlEndPoint),        
	Concept='%3Chttp://purl.org/ontology/mo/Track%3E'.
mapping:see_other_rdf(requested_pattern([all,record]),redirect_pattern([SparqlEndPoint,'?query=',
	'describe%20',Concept])) :-
	sparql_end_point(SparqlEndPoint),        
	Concept='%3Chttp://purl.org/ontology/mo/Record%3E'.
mapping:see_other_rdf(requested_pattern([all,performance]),redirect_pattern([SparqlEndPoint,'?query=',
	'describe%20',Concept])) :-
	sparql_end_point(SparqlEndPoint),        
	Concept='%3Chttp://purl.org/ontology/mo/Performance%3E'.
mapping:see_other_rdf(requested_pattern([all,signal]),redirect_pattern([SparqlEndPoint,'?query=',
	'describe%20',Concept])) :-
	sparql_end_point(SparqlEndPoint),        
	Concept='%3Chttp://purl.org/ontology/mo/Signal%3E'.
mapping:see_other_rdf(requested_pattern([all,timeline]),redirect_pattern([SparqlEndPoint,'?query=',
	'describe%20',Concept])) :-
	sparql_end_point(SparqlEndPoint),        
	Concept='%3Chttp://purl.org/NET/c4dm/timeline.owl#TimeLine%3E'.

mapping:see_other(requested_pattern([all,Concept]),redirect_pattern([Renderer,Directory,Concept])) :-
	html_renderer(Renderer),
	directory(Directory).
