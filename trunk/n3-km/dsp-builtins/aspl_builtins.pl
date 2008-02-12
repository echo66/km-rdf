:- module(aspl_builtins,[]).

:- use_module(library('semweb/rdf_db')).

:- use_module('../builtins').

:- use_module('../../swiaudiosource/audiosource').
:- use_module('../../swiaudiodata/audiodata').

:- rdf_register_ns(mo,'http://purl.org/ontology/mo/').
:- rdf_register_ns(tl,'http://purl.org/NET/c4dm/timeline.owl#').

/**
 * Registering the builtin
 */

%:- rdf_db:rdf_assert('http://purl.org/ontology/dsp/aspl_decode',rdf:type,'http://purl.org/ontology/tabling/TabledPredicate').

builtins:builtin('http://purl.org/ontology/dsp/aspl_decode',aspl_builtins:decode).

decode(Af,[literal(Ch),literal(Sr),literal(L),Sigs]) :- %plug uri resolution for Af here
	aspl_builtins:aspl_decode(Af,Out2),
	Out2='Signal'(Ch,Sr,L,Sigs).



