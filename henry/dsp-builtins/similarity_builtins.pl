/**
	Similarity builtins
	David Pastor 2008
*/

:- module(similarity_builtins,[]).

:- use_module(library('semweb/rdf_db')).

:- use_module('../builtins').
:- use_module('../swidata/data').
:- use_module('../../similarity/similarity').

:- rdf_register_ns(mo,'http://purl.org/ontology/mo/').
:- rdf_register_ns(tl,'http://purl.org/NET/c4dm/timeline.owl#').

/**
 * Registering the builtin
 */

%:- rdf_db:rdf_assert('http://purl.org/ontology/similarity/mfcc_kldiv',rdf:type,'http://purl.org/ontology/tabling/CachedPredicate').

builtins:builtin('http://purl.org/ontology/dsp/mfcc_kldiv',similarity_builtins:mfcc_kldiv).
builtins:builtin('http://purl.org/ontology/dsp/cosine_dis',similarity_builtins:cosine_dis).

/**
 *	Input: __data_id for the means and variances of two different audio files. Remember all swi-dsp modules work in this fashion
 */

mfcc_kldiv([[Means1, Vars1], [Means2, Vars2]], literal(Distance)):- 
	literal_list_to_list(Means1,Means1D),literal_list_to_list(Vars1,Vars1D),
	literal_list_to_list(Means2,Means2D),literal_list_to_list(Vars2,Vars2D),
	smpl_mfcc_kldiv(Means1D, Vars1D, Means2D, Vars2D, Distance).

cosine_dis([Beat1, Beat2], literal(D)):-
	literal_list_to_list(Beat1, Beat1D), list_blob(Beat1D, I1),
	literal_list_to_list(Beat2, Beat2D), list_blob(Beat2D, I2),
	smpl_cosine_distance(I1, I2, D).

literal_list_to_list([],[]).
literal_list_to_list([literal(H)|T],[H|T2]) :-	
	literal_list_to_list(T,T2).









