:- module(namespaces,[]).

:- use_module(library('semweb/rdf_db')).

:- rdf_register_ns(pl,'http://purl.org/ontology/swipl/').
:- rdf_register_ns('log','http://www.w3.org/2000/10/swap/log#').
:- rdf_register_ns(rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_ns(owl,'http://www.w3.org/2002/07/owl#').


