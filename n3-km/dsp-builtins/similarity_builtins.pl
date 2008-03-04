/**
	Similarity builtins
	David Pastor 2008
*/

:- module(similarity_builtins,[]).

:- use_module(library('semweb/rdf_db')).

:- use_module('../builtins').

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
	reserve_id('mean1'),
	reserve_id('mean2'),
	reserve_id('var1'),
	reserve_id('var2'),
	load_data_list(Means1D, 'mean1'),	
	load_data_list(Means2D, 'mean2'),
	load_data_list(Vars1D, 'var1'),
	load_data_list(Vars2D, 'var2'),
	//have to make and if here and not prolog lists!!!!!
	smpl_mfcc_kldiv('mean1', 'mean2', 'var1', 'var2', Distance).

literal_list_to_list([],[]).
literal_list_to_list([literal(H)|T],[H|T2]) :-	
	literal_list_to_list(T,T2).

