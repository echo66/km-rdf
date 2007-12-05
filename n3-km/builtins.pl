:- module(builtins,[builtin/2]).


/**
 * This modules provides anchors for builtin predicates.
 * By registering a new builtin(Predicate,CorrespondingPrologPredicate)
 * clause, proving Predicate using the n3_entailment
 * module will call CorrespondingPrologPredicate.
 *
 * The file examples/list.n3 gives an illustration of that, 
 * and uses the list:member/2 SWI predicate.
 *
 * Copyright Yves Raimond (c) 2007
 * Centre for Digital Music, Queen Mary, University of London
 */


:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_meta builtin(r,?).

/**
 *
 * Declaration of builtin predicates
 *
 */

builtin('http://www.w3.org/2000/10/swap/list#in',builtin:member).

builtin:member(A,B) :- nonvar(B),member(A,B).


