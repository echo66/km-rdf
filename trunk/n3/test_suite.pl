:- module(test_suite,[run/0,tokenise_and_parse/1,tokenise/2]).

/**
 * Some N3 parsing tests
 * (it is clearly not exhaustive though,
 * so there may still be some bugs)
 */

:- use_module(n3_dcg).

n3_dir('/home/moustaki/work/workspace/swap/test').


run :-
	forall(n3_dir(Dir),
		(
		nl,
		format(' - Parsing N3 files available in ~w\n',[Dir]),
		atom_concat(Dir,'/*.n3',Wildcard),
		expand_file_name(Wildcard,Files),
		forall(member(File,Files),
			(
			nl,format(' - Parsing ~w\n',[File]),
			tokenise_and_parse(File) -> (writeln(' - Success'),assert(success(File))); (writeln(' - Failure'),assert(failure(File)))
			))
		)),
	results.

results :-
	bagof(F,failure(F),Failures),
	bagof(S,success(S),Success),
	nl,
	format(' - ~w files successfully parsed\n',[Success]),
	format(' - ~w files not parsed\n',[Failures]).

tokenise_and_parse(File) :-
        tokenise(File,Tokens),
        phrase(n3_dcg:document,Tokens).

tokenise(File,Tokens) :-
        open(File,read,S),
        grab_tokens(S,Tokens).

grab_tokens(S,Tokens) :-
        grab_tokens(S,[],Tokens).

grab_tokens(S,SoFar,Tokens) :-
        n3_dcg:turtle_tokens(S,T),
        (T = end_of_file -> (SoFar=Tokens);(
                append(SoFar,T,T2),
                grab_tokens(S,T2,Tokens)
                )).






