#!/usr/local/bin/pl -s

:- use_module(ctr).
:- use_module(rdf_at).
:- use_module(ops).
:- use_module(namespaces).
:- use_module(sw_tr).
:- use_module(meta).
:- use_module(tabling).


%init the rdf persistency layer
:- use_module(library('semweb/rdf_persistency')).
:- rdf_attach_db(db,[]).

