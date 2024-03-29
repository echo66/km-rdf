/*  $Id: sparql_client.pl,v 1.2 2007/05/13 14:26:49 yves Exp $

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2006, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(sparql_client,
	  [ sparql_query/3,		% +Query, -Row, +Options
	    sparql_set_server/1		% +Options
	  ]).
:- use_module(library('http/http_open')).
:- use_module(sparql_xml_result).
:- use_module(library(lists)).
:- use_module(library(rdf)).


sparql_query(Query, Row, Options) :-
	sparql_param(host(Host), Options),
	sparql_param(port(Port), Options),
	sparql_param(path(Path), Options),
	http_open([ protocol(http),
		    host(Host),
		    port(Port),
		    path(Path),
		    search([ query = Query
			   ])
		  ], In,
		  [ header(content_type, ContentType)
		  ]),
	read_reply(ContentType, In, Row).

read_reply(Type, In, Row) :- 
	atom_concat('application/rdf+xml; charset=',_Charset,Type),
	!,writeln(1),	
	read_reply('application/rdf+xml',In,Row).
read_reply(Type, In, Row) :-
        atom_concat('application/sparql-results+xml; charset=',_Charset,Type),
        !,writeln(2),
        read_reply('application/sparql-results+xml',In,Row).
read_reply('application/rdf+xml', In, Row) :- !,
	call_cleanup(load_rdf(stream(In), RDF), close(In)),
	member(Row, RDF).
read_reply('application/sparql-results+xml', In, Row) :- !,
	call_cleanup(sparql_read_xml_result(stream(In), Result),
		     close(In)),
	xml_result(Result, Row).
read_reply(Type, In, _) :-
	close(In),
	throw(error(domain_error(sparql_result_document, Type), _)).

xml_result(ask(Bool), Result) :- !,
	Result = Bool.
xml_result(select(_VarNames, Rows), Result) :-
	member(Result, Rows).





		 /*******************************
		 *	      SETTINGS		*
		 *******************************/

:- dynamic
	sparql_setting/1.

sparql_param(Param, Options) :-
	memberchk(Param, Options), !.
sparql_param(Param, _Options) :-
	sparql_setting(Param), !.
sparql_param(Param, _Options) :-
	functor(Param, Name, _),
	throw(error(existence_error(option, Name), _)).

%%	sparql_set_server(+OptionOrList)
%	
%	Set sparql server default options.  Provided defaults are:
%	host, port and repository.  For example:
%	
%%		set_sparql_default([ host(localhost),
%%				     port(8080)
%%				     repository(world)
%				   ])

sparql_set_server([]) :- !.
sparql_set_server([H|T]) :- !,
	sparql_set_server(H),
	sparql_set_server(T).
sparql_set_server(Term) :-
	functor(Term, Name, Arity),
	functor(Unbound, Name, Arity),
	retractall(sparql_setting(Unbound)),
	assert(sparql_setting(Term)).
