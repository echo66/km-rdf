:- module(aspl_builtins,[]).

:- use_module('../builtins').

:- use_module('../../swiaudiosource/audiosource').
:- use_module('../../swiaudiodata/audiodata').

encodes(File,Signal) :-
	aspl_decode(File,Signal).


/**
 * Registering the builtin
 */

builtins:builtin('http://purl.org/ontology/mo/encodes',aspl_builtins:aspl_decode).



