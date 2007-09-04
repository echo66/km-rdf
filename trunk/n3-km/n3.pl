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

uqv(UQVar) :-
	atomic(UQVar),
	atom_concat(_,'_uqvar',UQVar).
	

/**
 * N3 interpreter - top-level predicates
 */

repl :- interp('').
interp :-
	interp('').
interp(BaseURI) :- 
	n3_dcg:turtle_tokens(user_input,Tokens),
	phrase(document(BaseURI,N3),Tokens),
	writeln(N3),
	prove(N3,Binding),(writeln(Binding)).

/**
 * Proof mechanims, 2nd attempt
 */
graph(Graph) :-
	rdf(_,_,_,Graph),!.

pl_clause(Clause,Bindings) :-
	(A <= B),
	n3_clause_to_pl_clause((A <=B),Clause,Bindings).
n3_clause_to_pl_clause('<='(Graph1,Graph2), clause(graph(TriplesHead),graph(TriplesBody)),Bindings) :-
	graph_triples(Graph1,Triples1),
	graph_triples(Graph2,Triples2),
	clean_vars(Triples1,TriplesHead,Bindings1),
	clean_vars(Triples2,TriplesBody,Bindings1,Bindings).
	%clean_lists(T1,TriplesHead),
	%clean_lists(T2,TriplesBody).


graph_triples(GraphURI,Triples) :-
	bagof(triple(S,P,O),rdf(S,P,O,GraphURI),Triples).

clean_lists([],[]).
clean_lists([rdf(_,_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#List',_)|T],T2) :-
	!,clean_lists(T,T2).
clean_lists([rdf(_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',_,_)|T],T2) :-
	!,clean_lists(T,T2).
clean_lists([rdf(_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',_,_)|T],T2) :-
	!,clean_lists(T,T2).
clean_lists([rdf(S,P,O,G)|Tail],[rdf(SL,PL,OL,G)|Tail2]) :-
	((rdfs_list_to_prolog_list(S,SL),!);S=SL),
	((rdfs_list_to_prolog_list(P,PL),!);P=PL),
	((rdfs_list_to_prolog_list(O,OL),!);O=OL),
	clean_lists(Tail,Tail2).
	

clean_vars(Triples,Cleaned,Bindings) :-
	clean_vars(Triples,Cleaned,[],Bindings).
clean_vars([],[],Bindings,Bindings):- !.
clean_vars([triple(S,P,O)|Tail],[triple(SV,PV,OV)|Tail2],BSF,Bindings) :-
	((uqv(S),!,((member(binding(S,SV),BSF),!,SB=[]);SB=[binding(S,SV)]));(S=SV,SB=[])),
	((uqv(P),!,((member(binding(P,PV),BSF),!,PB=[]);PB=[binding(P,PV)]));(P=PV,PB=[])),
	((uqv(O),!,((member(binding(O,OV),BSF),!,OB=[]);OB=[binding(O,OV)]));(O=OV,OB=[])),
	append(SB,PB,T),append(T,OB,NewB),
	append(BSF,NewB,NewBSF),
	clean_vars(Tail,Tail2,NewBSF,Bindings).


bnodes_to_vars(Triples,Cleaned,Bindings) :-
        bnodes_to_vars(Triples,Cleaned,[],Bindings).
bnodes_to_vars([],[],Bindings,Bindings):- !.
bnodes_to_vars([triple(S,P,O)|Tail],[triple(SV,PV,OV)|Tail2],BSF,Bindings) :-
        ((rdf_is_bnode(S),!,((member(binding(S,SV),BSF),!,SB=[]);SB=[binding(S,SV)]));(S=SV,SB=[])),
        ((rdf_is_bnode(P),!,((member(binding(P,PV),BSF),!,PB=[]);PB=[binding(P,PV)]));(P=PV,PB=[])),
        ((rdf_is_bnode(O),!,((member(binding(O,OV),BSF),!,OB=[]);OB=[binding(O,OV)]));(O=OV,OB=[])),
        append(SB,PB,T),append(T,OB,NewB),
        append(BSF,NewB,NewBSF),
        bnodes_to_vars(Tail,Tail2,NewBSF,Bindings).


to_graph([],[]).
to_graph([rdf(S,P,O,_)|T],[triple(S,P,O)|T2]) :- to_graph(T,T2).


prove(N3Query,Bindings) :-
	match(N3Query,Q,_,Bindings),
	ground(Bindings),writeln(Q),ground(Q). %ground solution
prove(N3Query,Bindings) :-
	match(N3Query,Q,Graphs,Bindings),
	forall(
		member(Graph,Graphs)
	    ,
	    	(
		  (Graph <= Graph2),
		  graph_triples(Graph2,Triples),
		  prove(Triples,_)
		)
	     ),
	ground(Bindings),ground(Q).


match(N3Query,Q,Graphs,Bindings) :-
	to_graph(N3Query,Query),
	clean_vars(Query,T,_),
	bnodes_to_vars(T,Q,_),
	p(Q,Graphs2,Bindings2),
	list_to_set(Graphs2,Graphs),
	list_to_set(Bindings2,Bindings),
	\+conflict(Bindings).
p([],[],[]).
p([triple(S,P,O)|T],[Graph|GraphTail],Bindings) :-
	p(triple(S,P,O),Graph,Binding),
	append(Binding,BindingTail,Bindings),
	p(T,GraphTail,BindingTail).

p(triple(S,P,O),Graph,Binding) :-
	quad(SS,PP,OO,Graph),
	((uqv(SS),SB=[binding(S,SS)]);(S=SS,\+uqv(S),SB=[])),
	((uqv(PP),PB=[binding(P,PP)]);(P=PP,\+uqv(P),PB=[])),
	((uqv(OO),OB=[binding(O,OO)]);(O=OO,\+uqv(O),OB=[])),
	append(SB,PB,T),append(T,OB,Binding).

conflict([]) :- fail.
conflict([binding(A,B)|T]) :-
	member(binding(D,B),T),
	A\==D,!.
conflict([_|T]) :-
	conflict(T).

/**
 * Builtins
 */

quad(S,P,O,G) :-
	rdf(S,P,O,G).
quad([A,B],'http://purl.org/ontology/swipl/concat',D) :-
	atom_concat(A,B,D).

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


