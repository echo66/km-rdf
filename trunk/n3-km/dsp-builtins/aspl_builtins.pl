:- module(aspl_builtins,[]).

:- use_module(library('semweb/rdf_db')).

:- use_module('../builtins').

:- use_module('../../swiaudiosource/audiosource').
:- use_module('../../swiaudiodata/audiodata').


%:- use_module('../rdf-ctr/ctr').
%:- use_module('../rdf-ctr/ops').
%:- use_module('../rdf-ctr/rdf_at').
%:- use_module('../rdf-ctr/sw_tr').

:- rdf_register_ns(mo,'http://purl.org/ontology/mo/').
:- rdf_register_ns(tl,'http://purl.org/NET/c4dm/timeline.owl#').

/**
 * Registering the builtin
 */

builtins:builtin('http://purl.org/ontology/mo/encodes',aspl_builtins:decode).

decode(Af,[literal(Ch),literal(Sr),literal(L)|Sigs]) :- %plug uri resolution for Af here
	aspl_builtins:aspl_decode(Af,Out2),
	Out2='Signal'(Ch,Sr,L,Sigs).

	%Out = 'Signal'(Channels,SR,Length,Signals),
	%rdf_bnode(Sig),rdf_bnode(T),
	%(trans assert_t(Af,mo:encodes,Sig,user) >>
	%       assert_t(Sig,mo:channels,literal(Channels)) >>
	%       assert_t(Sig,mo:sample_rate,literal(SR)) >>
	%       assert_t(Sig,mo:time,T) >>
	%       assert_t(T,tl:duration,literal(Length))
	%       ).




