:- module(n3_load,[n3_load/1]).

:- use_module(n3_dcg).

n3_load(File) :-
        tokenise(File,Tokens),
        absolute_file_name(File,Absolute),
        format(atom(Base),'file://~w',[Absolute]), % That's probably not right
        phrase(n3_dcg:document(Base,Doc),Tokens),
        forall(member(rdf(A,B,C,D),Doc),rdf_db:rdf_assert(A,B,C,D)).

