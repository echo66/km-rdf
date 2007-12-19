:- module(c4dml_builtins,[]).


:- use_module('../builtins').


:- load_foreign_library('../../c4dmlswi/c4dmlswi.so').



builtins:builtin('http://purl.org/NET/c4dm/c4dml.rdf#read',c4dml_builtins:af_read).
builtins:builtin('http://purl.org/NET/c4dm/c4dml.rdf#mfcc',c4dml_builtins:mfcc).


af_read([literal(Af),literal(Start),literal(End),literal(SampleRate)],Samples) :-
	atom_to_term(Start,S,[]),atom_to_term(End,E,[]),atom_to_term(SampleRate,SR,[]),
	c4dml_read(Af,S,E,SR,Samples).


mfcc([Samples,literal(SampleRate),literal(Dim)],Mfcc) :-
	length(Samples,Length),
	atom_to_term(SampleRate,SR,[]),atom_to_term(Dim,D,[]),
	c4dml_mfcc(Samples,Length,SR,D,Mfcc),write(user_error,Mfcc).




