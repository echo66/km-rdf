:- module(n3,[
		parse/3
	,	load/2
	,	interp/0
	]).

/**
 * Start of a "N3 interpreter"
 *
 * Yves Raimond, C4DM, QMUL, (c) 2007
 */


:- use_module(library('semweb/rdf_db')).
:- use_module('../n3/n3_dcg').
:- use_module('namespaces').
:- use_module(library('semweb/rdfs')).

/**
 * Some Prolog wrappers for common N3 constructs
 */

:- op(1110,xfy,'<=').

HeadGraph <= TailGraph :- 
	rdf(If,owl:inverseOf,log:implies),
	rdf(HeadGraph,If,TailGraph).
HeadGraph <= TailGraph :-
	rdf(TailGraph,log:implies,HeadGraph).

		
interp :- 
	n3_dcg:turtle_tokens(user_input,Tokens),
	phrase(document('http://example.org/',N3),Tokens),
	writeln(N3).

rdf_uq_var(UQVar) :-
	rdf(UQVar,rdf:type,'http://purl.org/ontology/km/UQVar').


prove(triple(S,P,O)) :-
	((rdfs_list_to_prolog_list(S,NewS),!);S=NewS),
	((rdfs_list_to_prolog_list(O,NewO),!);O=NewO), %lists as properties?
	P=NewP,
	prove(NewS,NewP,NewO).


/**
 * Parsing/Loading tools
 */
parse(Document,Triples,Options) :-
	tokenise(Document,Tokens),
	(member(base_uri(Base),Options) ->
		phrase(document(Base,Triples),Tokens) ;
		(absolute_file_name(Document,Absolute),
	 	 format(atom(Base),'file://~w#',[Absolute]),
		 phrase(document(Base,Triples),Tokens))).

load(Document,Options) :-
	parse(Document,Triples,Options),
	((member(base_uri(Base),Options),!);(absolute_file_name(Document,Absolute),format(atom(Base),'file://~w#',[Absolute]))),
	load_parsed_n3(Base,Triples).

:- dynamic base_ns/1.
:- dynamic ns/2.

load_parsed_n3(_,[]).
load_parsed_n3(Base,[namespace(Name,URI)|T]) :- 
	assert(ns(Name,URI)),
	load_parsed_n3(Base,T).
load_parsed_n3(Base,[namespace(base,URI)|T]) :- 
	assert(base_ns(URI)),
	load_parsed_n3(Base,T).
load_parsed_n3(Base,[rdf(S,P,O,G)|T]) :- 
	expand(Base,S,SE),expand(Base,P,PE),expand(Base,O,OE),
	rdf_assert(SE,PE,OE,G),
	load_parsed_n3(Base,T).

:- op(100,xfx,':').
:- op(100,fx,':').
expand(Base,:Name,OE) :-
	((base_ns(NS),!);NS=Base),
	atom_concat(NS,Name,OE),!.
expand(_,NS:Name,OE) :-
	ns(NS,URI),
	atom_concat(URI,Name,OE),!.
expand(Base,':',OE) :-
	((base_ns(OE),!);OE=Base),!.
expand(_,Name,Name).


