:- module(skolemize,[skolemize/3]).

/**
 * Some unit tests
 */
:- begin_tests(skolemize).
test(skolemize) :-
	skolemize(
	[rdf_e(A,has,bnode(Nose)),rdf_e(bnode(Nose),colour,bnode(Colour)),rdf_e(bnode(Colour),label,pink)],
	[rdf_e(A,isa,man)],
	[rdf_e(A, has, individual(Nose, [A])), rdf_e(individual(Nose, [A]), colour, bnode(Colour)), rdf_e(individual(Colour, [A]), label, pink)]).
:- end_tests(skolemize).
/**
 * End of unit tests
 */


skolemize([],_,[]).
skolemize([rdf_e(S,P,O)|T1],Body,[rdf_e(individual(B,Vars),P,O)|T2]) :-
	S=@=bnode(B),S=bnode(B),
	free_variables(Body,Vars),!,
	skolemize(T1,Body,T2).
skolemize([rdf_e(S,P,O)|T1],Body,[rdf_e(S,P,individual(B,Vars))|T2]) :-
        O=@=bnode(B),O=bnode(B),
	free_variables(Body,Vars),!,
        skolemize(T1,Body,T2).
skolemize([H|T1],Body,[H|T2]) :-
	skolemize(T1,Body,T2).




