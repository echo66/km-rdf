:- module(web_access,[]).


:- use_module(entailment).
:- use_module(n3_to_prolog).
:- use_module(library('semweb/rdf_db'),[rdf_global_term/2]).

/**
 * Web access module
 *
 * To add: POST handling of new audio files?
 */

:- http_handler(prefix('/data/'), rdf_reply, []).

rdf_reply(Request) :-
	memberchk(path(Path),Request),
	format(user_error,'Requested path: ~w\n',[Path]),
	Path = '/data/all',
	format('Content-type: application/rdf+xml; charset=UTF-8~n~n', []),
	findall(rdf('/data/all',rdfs:seeAlso,Graph),(implies(_BG,HG),atom_concat('__bnode_',Gr,HG),atom_concat('/data/',Gr,Graph)),Bag),
	rdf_global_term(Bag,BagX),
	format(user_error,'~w\n',BagX),
	rdf_write_xml(current_output,BagX).



