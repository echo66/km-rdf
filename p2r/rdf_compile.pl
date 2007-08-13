:- module(rdf_compile,[init_db/0,rdf_compile/0]).

/**
 * Compile possible RDF statements to
 * improve responsiveness of the sparql 
 * end point, but loose the dynamicity of
 * the underlying data
 */

:- use_module(p2r_entailment).
:- use_module(library('semweb/rdf_persistency')).
:- use_module(library('semweb/rdf_db')).

init_db :-
	nl,writeln(' - Attaching DB...'),nl,
	rdf_attach_db('db',[]),
	settings:set_setting(serql_parms:default_entailment, rdfs).

rdf_compile :-
	nl,writeln(' - Deriving all possible RDF statements'),nl,
	forall(
		p2r_entailment:rdf(A,B,C)
	,
		rdf_assert(A,B,C)
		).



