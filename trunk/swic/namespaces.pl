:- module(namespaces,[]).

:- use_module(library('semweb/rdf_db')).

:-
        rdf_register_ns(dc,'http://purl.org/dc/elements/1.1/'),
        rdf_register_ns(foaf,'http://xmlns.com/foaf/0.1/'),
        rdf_register_ns(geo,'http://www.w3.org/2003/01/geo/wgs84_pos#'),
        rdf_register_ns(doap,'http://usefulinc.com/ns/doap#'),        rdf_register_ns(swrc,'http://swrc.ontoware.org/ontology#'),
        rdf_register_ns(mo,'http://purl.org/ontology/mo/'),
        rdf_register_ns(event,'http://purl.org/NET/c4dm/event.owl#'),
        rdf_register_ns(tl,'http://purl.org/NET/c4dm/timeline.owl#'),
	rdf_register_ns(time,'http://www.w3.org/TR/owl-time/'),
	rdf_register_ns(skos,'http://www.w3.org/2004/02/skos/core#'),
	rdf_register_ns(dbpedia,'http://dbpedia.org/resource/').

