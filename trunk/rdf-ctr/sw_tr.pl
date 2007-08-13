:- module(sw_tr,[
		load/2
	,	grab/1
	,	uri_url/2
	,	query/4
	,	entail/4
]).

/**
 * A module holding some semweb related transactions
 * Some examples:
 * 	?- query('http://moustaki.org/foaf.rdf#moustaki',foaf:knows,O,S) >> query(O,foaf:name,Name,S2). 
 *	?- query('http://dbtune.org/jamendo/artist/5',B,C,S).
 */

:- use_module(ctr).
:- use_module(ops).
:- use_module(rdf_at).

:- use_module(library(rdf)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('http/http_open')).
:- use_module(library('semweb/rdf_turtle')).


grab(URI) :- atomic(URI) >> load(URI,Triples) and uri_url(URI,URL) >> (member(rdf(S,P,O),Triples) => triple(S,P,O,URL)). 

load(URI,Triples) :- 
	atomic(URI) and uri_url(URI,URL) and http_open(URL,Stream,[timeout(2),request_header('Accept'='application/rdf+xml')]) >> 
	(load_rdf(Stream,Triples,[base_uri(URL)]) or rdf_load_turtle(Stream,Triples,[base_uri(URL)])) >>
	Triples\=[].

uri_url(URI,URL) :-
	parse_url(URI,List) >> ((select(fragment(_),List,Rest) >> parse_url(URL,Rest)) or (\+select(fragment(_),List,_) and URI=URL)).

:- rdf_meta(query(r,r,o,r)).
query(S,P,O,Source) :-
	(
		(grab(S) >> (triple(S,rdfs:seeAlso,DocS,_) >> grab(DocS) or (triple(S,owl:sameAs,SameS,_) or triple(SameS,owl:sameAs,S,_)) >> grab(SameS) or true) or true) # 
		(grab(P) >> (triple(P,rdfs:seeAlso,DocP,_) >> grab(DocP) or (triple(P,owl:sameAs,SameP,_) or triple(SameP,owl:sameAs,P,_)) >> grab(SameP) or true) or true) # 
		(grab(O) >> (triple(O,rdfs:seeAlso,DocO,_) >> grab(DocO) or (triple(O,owl:sameAs,SameO,_) or triple(SameO,owl:sameAs,O,_)) >> grab(SameO) or true) or true) # 
		(grab(Source) or true)
	) >> 
	entail(S,P,O,Source).

%query(Triples) :-
%	(member(rdf(S,P,O,Source),Triples) =#> query(S,P,O,Source)).

%some entailment rules
:- rdf_meta(entail(r,r,o,r)).
entail(S,P,O,Source) :-
	triple(S,P,O,Source).
entail(S,P,O,Source) :-
	(triple(S,owl:sameAs,Same,_) or triple(Same,owl:sameAs,S,_)) and
	triple(Same,P,O,Source).



