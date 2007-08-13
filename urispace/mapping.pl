:- module(mapping,[
		see_other_rdf/2
	,	see_other/2
	]).

/**
 * See other when Accept: application/rdf+xml 
 * is in the request header
 */
:- multifile see_other_rdf/2.

/**
 * See other, default behavior
 */
:- multifile see_other/2.

