:- module(query,[
		op(600,fx,'<?>')
	,	'<?>'/1
	]).


/**
 * Common interface for different type of queries
 *
 * All is done through the <?> operator, which might make use
 * of clause exported elsewhere
 *
 * This interface is "implemented" in swic.pl (semwebclient-like SW access)
 * and in sparql.pl (access to sparql end point)
 *
 * (SW browsing, or query to SPARQL end points)
 */
:- multifile (<?>)/1.

'<?>'(_) :-
	nl,
	writeln('--------'),
	nl,
	writeln('Querying...'),
	nl,
	writeln('--------'),
	nl,fail.

