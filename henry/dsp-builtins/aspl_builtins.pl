:- module(aspl_builtins,[]).

:- use_module(library('semweb/rdf_db')).

:- use_module('../builtins').

:- use_module('../../swiaudiosource/audiosource').
:- use_module('../../swiaudiodata/audiodata').

:- rdf_register_ns(mo,'http://purl.org/ontology/mo/').
:- rdf_register_ns(tl,'http://purl.org/NET/c4dm/timeline.owl#').

:- use_module(library('http/http_client')).
:- use_module('persistency/tmp').

/**
 * Registering the builtin
 */

%:- rdf_db:rdf_assert('http://purl.org/ontology/dsp/aspl_decode',rdf:type,'http://purl.org/ontology/tabling/TabledPredicate').

builtins:builtin('http://purl.org/ontology/dsp/aspl_decode',aspl_builtins:decode).

decode(Af,[literal(Ch),literal(Sr),literal(L),Sigs]) :- %plug uri resolution for Af here
	atom_concat('file://',File,Af),
	format(user_error,'DEBUG: Decoding file ~w\n',[File]),
	aspl_builtins:aspl_decode(File,Out2),
	Out2='Signal'(Ch,Sr,L,Sigs).


builtins:builtin('http://purl.org/ontology/dsp/cache',aspl_builtins:cache).
cache(Af,File) :-
	handler(Protocol,Pred),
	atom_concat(Protocol,_,Af),
	ToCall =.. [Pred,Af,File],
	ToCall.


/**
 * Declare your custom audio file access predicates here
 */
:- multifile handler/2. % hook 
handler(http,http_uri).

http_uri(Http,FileURI) :-
	www_form_encode(Http,Key),
	format(atom(File),'httpcache/~w',[Key]),
	absolute_file_name(File,Abs),
	format(user_error,'~w\n',[exists_file(Abs)]),
	exists_file(Abs),writeln(coucou),!,
	atom_concat('file://',Abs,FileURI).

http_uri(Http,FileURI) :-
	www_form_encode(Http,Key),
	format(atom(File),'httpcache/~w',[Key]),
	format(user_error,'DEBUG: Caching ~w as ~w\n',[Http,File]),
	format(atom(Command),'wget ~w -O- > ~w',[Http,File]),
	shell(Command),
	absolute_file_name(File,Abs),
	atom_concat('file://',Abs,FileURI).

handler(file,file_uri).

file_uri(FURI,FURI) :-
	atom_concat('file://',_,FURI).

