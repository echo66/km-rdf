#!/usr/local/bin/pl -L0 -G0 -s

:- use_module(query).
:- use_module(swic).
:- use_module(sparql).
:- use_module(namespaces).

:-
 nl,
 writeln('----------------------'),
 nl,
 writeln(' - SWIC'),
 writeln(' swi semantic web client'),
 nl,
 writeln('Yves Raimond, Centre for Digital Music, Queen Mary, University of London'),
 nl,
 writeln('----------------------'),
 nl,
 writeln(' - type ''h.'' for help'),
 nl.

h :-
 nl,
 writeln(' - Usage:'),
 nl,
 writeln(' <?> [triplePattern1, triplePattern2, ... , triplePatternN] '),
 writeln(' where triplePattern* is of the form rdf(''http://.../foaf#me'',Property,Object)'),
 writeln(' (variables start with an uppercase or an underscore character)'),
 nl,
 swic:h,
 sparql:h.


