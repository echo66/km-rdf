:- module(n3,[
		parse/3
	,	load/2
	,	interp/0
	,	interp/1
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

uqvar(UQVar) :-
        rdf(UQVar,rdf:type,'http://purl.org/ontology/km/UQVar').

	

/**
 * N3 interpreter - top-level predicates
 */
interp :-
	interp('').
interp(BaseURI) :- 
	n3_dcg:turtle_tokens(user_input,Tokens),
	phrase(document(BaseURI,N3),Tokens),
	writeln(N3).


/**
 * Proof mechanims, 2nd attempt
 */
graph(Graph) :-
	rdf(_,_,_,Graph),!.

n3_clause_to_pl_clause('<='(Graph1,Graph2), clause(graph(TriplesHead,Bindings),graph(TriplesBody,Bindings))) :-
	graph_triples(Graph1,Triples1),
	graph_triples(Graph2,Triples2),
	clean_vars(Triples1,TriplesHead,Bindings1),writeln(Bindings1),
	clean_vars(Triples2,TriplesBody,Bindings1,Bindings),
	append(Bindings1,T,Bindings),writeln(T).
	%clean_lists(T1,TriplesHead),
	%clean_lists(T2,TriplesBody).


graph_triples(GraphURI,Triples) :-
	bagof(triple(S,P,O),rdf(S,P,O,GraphURI),Triples).

%clean_lists([],[]).
%clean_lists([triple(S,P,O)|Tail],[triple(SL,PL,OL)|Tail2]) :-
%	((rdfs_list_to_prolog_list(S,SL),!);S=SL),
%	((rdfs_list_to_prolog_list(P,PL),!);P=PL),
%	((rdfs_list_to_prolog_list(O,OL),!);O=OL),
%	clean_lists(Tail,Tail2).

clean_vars(Triples,Cleaned,Bindings) :-
	clean_vars(Triples,Cleaned,[],Bindings).
clean_vars([],[],Bindings,Bindings):- !.
clean_vars([triple(S,P,O)|Tail],[triple(SV,PV,OV)|Tail2],BSF,Bindings) :-
	((uqvar(S),!,((member(binding(S,SV),BSF),!,SB=[]);SB=[binding(S,SV)]));(S=SV,SB=[])),
	((uqvar(P),!,((member(binding(P,PV),BSF),!,PB=[]);PB=[binding(P,PV)]));(P=PV,PB=[])),
	((uqvar(O),!,((member(binding(O,OV),BSF),!,OB=[]);OB=[binding(O,OV)]));(O=OV,OB=[])),
	append(SB,PB,T),append(T,OB,NewB),
	append(BSF,NewB,NewBSF),
	clean_vars(Tail,Tail2,NewBSF,Bindings).

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


