:- module(aspl_builtins,[]).

:- use_module('../builtins').

:- use_module('../../swiaudiosource/audiosource').

builtins:builtin('http://purl.org/ontology/mo/encodes',audiosource:aspl_decode).





