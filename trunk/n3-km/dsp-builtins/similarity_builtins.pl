/**
	Similarity builtins
	David Pastor 2008
*/

:- module(similarity_builtins,[]).

:- use_module(library('semweb/rdf_db')).

:- use_module('../builtins').
:- use_module('../swivamp/vamp').
:- use_module('../feature-extractor/fe').
:- use_module('../swiaudiodata/audiodata').
:- use_module('../../similarity/similarity').

:- rdf_register_ns(mo,'http://purl.org/ontology/mo/').
:- rdf_register_ns(tl,'http://purl.org/NET/c4dm/timeline.owl#').

/**
 * Registering the builtin
 */

%:- rdf_db:rdf_assert('http://purl.org/ontology/similarity/mfcc_kldiv',rdf:type,'http://purl.org/ontology/tabling/CachedPredicate').

builtins:builtin('http://purl.org/ontology/dsp/mfcc_kldiv',similarity_builtins:mfcc_kldiv).

/**
 *	Input: __data_id for the means and variances of two different audio files.
 */

mfcc_kldiv([[Means1, Vars1], [Means2, Vars2]], literal(Distance)):- 
	literal_list_to_list(Means1,Means1D),literal_list_to_list(Vars1,Vars1D),
	literal_list_to_list(Means2,Means2D),literal_list_to_list(Vars2,Vars2D),
	next_id(IM1), reserve_id(IM1),
	next_id(IM2), reserve_id(IM2),
	next_id(IV1), reserve_id(IV1),
	next_id(IV2), reserve_id(IV2),
	load_data_list(Means1D, IM1),	
	load_data_list(Means2D, IM2),
	load_data_list(Vars1D, IV1),
	load_data_list(Vars2D, IV2),
	smpl_mfcc_kldiv(IM1, IM2, IV1, IV2, Distance).

literal_list_to_list([],[]).
literal_list_to_list([literal(H)|T],[H|T2]) :-	
	literal_list_to_list(T,T2).









