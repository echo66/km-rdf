:- module(skolemize,[skolemize/3]).

/**
 * Some unit tests
 */
:- begin_tests(skolemize).
test(skolemize) :-
	skolemize(
	[rdf(A,has,bnode(Nose)),rdf(bnode(Nose),colour,bnode(Colour)),rdf(bnode(Colour),label,pink)],
	[rdf(A,isa,man)],
	[rdf(A, has, individual(Nose, [A])), rdf(individual(Nose, [A]), colour, bnode(Colour)), rdf(individual(Colour, [A]), label, pink)]).
:- end_tests(skolemize).
/**
 * End of unit tests
 */


skolemize([],_,[]).
skolemize([rdf(S,P,O)|T1],Body,[rdf(individual(B,Vars),P,O)|T2]) :-
	S=@=bnode(B),S=bnode(B),
	free_variables(Body,Vars),!,
	skolemize(T1,Body,T2).
skolemize([rdf(S,P,O)|T1],Body,[rdf(S,P,individual(B,Vars))|T2]) :-
        O=@=bnode(B),O=bnode(B),
	free_variables(Body,Vars),!,
        skolemize(T1,Body,T2).






