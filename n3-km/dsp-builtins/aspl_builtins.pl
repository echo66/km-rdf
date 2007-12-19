:- module(aspl_builtins,[]).

:- use_module('../builtins').

:- use_module('../../swiaudiosource/audiosource').
:- use_module('../../swiaudiodata/audiodata').



/**
 * Registering the builtin
 */

builtins:builtin('http://purl.org/ontology/mo/encodes',aspl_builtins:decode).

decode([Af],[Out]) :-
	aspl_builtins:aspl_decode(Af,Out).


