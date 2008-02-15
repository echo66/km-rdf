:- module(vamp_builtins,[]).


:- use_module('../builtins').
:- use_module(library('semweb/rdf_db')).


:- use_module('../swivamp/vamp').
:- use_module('../feature-extractor/fe').
:- use_module('../swiaudiodata/audiodata').
:-use_module('../swiaudiosource/audiosource').

builtins:builtin('http://purl.org/ontology/vamp/qm-keydetector',vamp_builtins:keydetector).

keydetector(Input,Features) :-
	nonvar(Input),
	Input = [literal(Channels),literal(SR),literal(L),Sigs],
	vmpl_load_plugin_for('libqm-vamp-plugins:qm-keydetector','Signal'(Channels,SR,L,Sigs),Plugin),
	vmpl_get_blockSize(Plugin,Win),
	vmpl_get_stepSize(Plugin,Hop),
	vmpl_initialize_plugin(Plugin,1,Win,Hop),
	vamp_compute_feature('Signal'(Channels,SR,L,Sigs),Win,Hop,0,Plugin,F),
	findall([literal(Otp),literal(TS),literal(TE),literal(Data)],(member('Feature'(Otp,'Timestamp'(TS,TE),DataBin),F),data(DataBin,DataL),DataL=[Data]),Features).

