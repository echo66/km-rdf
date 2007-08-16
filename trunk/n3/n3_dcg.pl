:- module(n3_dcg,[]).

/**
 * A second attempt to create a DCG grammar for
 * SWI - this time I hope I won't loose all
 * my data... :-(
 *
 * Yves Raimond, C4DM, Queen Mary, University of London
 */


%in the n3.n3 grammar, doesn't "zeroOrMore" overlaps with
%() or ...?
document -->
	declaration,
	universal,
	existential,
	statements_optional.

statements_optional -->
	statement,
	['.'],!,
	statements_optional.
statements_optional.

formulacontent -->
	declarations,
	universals,
	existentials,
	statementlist.

declarations -->
	declaration,!,
	declarations.
declarations.

universals -->
	universal,!,
	universals.
universals.

existentials --> 
	existential,!,
	existentials.
existentials.

statementlist -->
	statement,!,
	statementtail.
statementlist.

statementtail -->
	['.'],!,
	statementlist.
statementtail.

universal --> 
	['@forAll'],
	csl_symbol.

existential --> 
	['@forSome'],
	csl_symbol.

declaration -->
	['@prefix'],
	qname,
	explicituri,
	['.'].
declaration -->
	['@keywords'],
	csl_barename.

statement -->
	subject,
	propertylist.

propertylist -->
	verb,
	!,
	object,
	objecttail,
	propertylisttail.
propertylist.

propertylisttail -->
	[';'],!,
	propertylist.
propertylisttail.

objecttail --> 
	[','],!,
	object,
	objecttail.
objecttail.

verb -->
	path.
verb -->
	['@has'],
	path.
verb -->
	['@is'],
	path,
	['@of'].
verb -->
	['@a'].
verb -->
	['='].
verb -->
	['=>'].
verb -->
	['<='].

prop -->
	node.

subject --> 
	path.

object -->
	path.

path -->
	node,
	pathtail.

pathtail -->
	['!'],!,
	path.
pathtail -->
	['^'],!,
	path.
pathtail.

node -->
	symbol.
node -->
	['{'],
	formulacontent,
	['}'].
node -->
	variable.
node -->
	numericliteral.
node --> 
	literal.
node -->
	['['],
	propertylist,
	[']'].
node --> 
	['('],
	pathlist,
	[')'].
%node -->
%	['@this']. %deprecated

pathlist --> 
	path,!,pathlist.
pathlist.

symbol --> 
	explicituri,
	qname.

literal --> 
	string,
	dtlang.

dtlang -->
	['@'],!,langcode.
dtlang -->
	['^^'],!,symbol.
dtlang.

/**
 * Coma separated period terminated list grammar
 */
csl_symbol -->
	symbol,
	[','],!,
	csl_symbol.
csl_symbol -->
	symbol,
	['.'].
csl_barename -->
	barename,
	[','],!,
	csl_barename.
csl_barename -->
	barename,
	['.'].


/**
 * TERMINALS
 */

numericliteral -->
	[NumericLiteral],
	{matches(NumericLiteral,'[-+]?[0-9]+(\\.[0-9]+)?(e[-+]?[0-9]+)?')}.

explicituri -->
	[ExplicitURI],
	{matches(ExplicitURI,'<[^>]*>')}.

qname -->
	[QName],
	{matches(QName,'(([a-zA-Z_][a-zA-Z0-9_]*)?:)?([a-zA-Z_][a-zA-Z0-9_]*)?')}.

barename -->
	[BareName],
	{matches(BareName,'[a-zA-Z_][a-zA-Z0-9_]*')}.

variable -->
	[Variable],
	{matches(Variable,'\\?[a-zA-Z_][a-zA-Z0-9_]*')}.

/**
 * RegExp match
 */
matches(Atom,RegEx) :-
	new(S,string(Atom)),
	new(R,regex(string(RegEx))),
	send(R,match,S).

