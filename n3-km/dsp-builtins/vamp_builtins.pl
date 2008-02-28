:- module(vamp_builtins,[]).


:- use_module('../builtins').
:- use_module(library('semweb/rdf_db')).


:- use_module('../swivamp/vamp').
:- use_module('../feature-extractor/fe').
:- use_module('../swiaudiodata/audiodata').
:-use_module('../swiaudiosource/audiosource').

builtins:builtin('http://purl.org/ontology/vamp/qm-keydetector',vamp_builtins:keydetector).
builtins:builtin('http://purl.org/ontology/vamp/qm-mfccmeans',vamp_builtins:mfccmeans).
builtins:builtin('http://purl.org/ontology/vamp/qm-mfccvars',vamp_builtins:mfccvars).


keydetector(Input,Features) :-
	nonvar(Input),
	Input = [literal(Channels),literal(SR),literal(L),Sigs],
	vmpl_load_plugin_for('libqm-vamp-plugins:qm-keydetector','Signal'(Channels,SR,L,Sigs),Plugin),
	vmpl_get_blockSize(Plugin,Win),
	vmpl_get_stepSize(Plugin,Hop),
	vmpl_initialize_plugin(Plugin,Channels,Win,Hop),
	vamp_compute_feature('Signal'(Channels,SR,L,Sigs),Win,Hop,2,Plugin,F),
	findall([literal(Otp),literal(TS),literal(TE),literal(Data)],(member('Feature'(Otp,'Timestamp'(TS,TE),DataBin),F),data(DataBin,DataL),DataL=[Data]),Features).

/**
	Specific builtins to extract MFCC means and vars from the similarity plugin. Necessary to mix down the input to not confuse the plugin with 2
	different tracks. Just timbre mode. (note: how to combine different similarities?)
*/		

mfccparameters(Input, [MLData, VLData]) :-
	nonvar(Input),
	Input = [literal(Channels),literal(SR),literal(L),Sigs],
	mix_stereo('Signal'(Channels, SR, L, Sigs), 'Signal'(ChannelsM, SRM, LM, SigsMono)),
	vmpl_load_plugin('libqm-vamp-plugins:qm-similarity',SR,Plugin),
	vmpl_get_blockSize(Plugin, BlockSize),
	vmpl_get_stepSize(Plugin, StepSize),
	vmpl_set_parameter(Plugin, 'featureType', 0),
	vmpl_initialize_plugin(Plugin, 1, StepSize, BlockSize),
	vamp_compute_feature('Signal'(ChannelsM, SRM, LM, SigsMono), StepSize, BlockSize, [3,4], Plugin, F),
	F= [[Mean, Var]],
	Mean = ['Feature'(_MType, 'Timestamp'(_StartM, _EndM), MBinData)],
	Var = ['Feature'(_VType, 'Timestamp'(_StartV, _EndV), VBinData)],
	data(MBinData,MData),
	to_literal_list(MData,MLData),
	data(VBinData,VData),
	to_literal_list(VData,VLData).


to_literal_list([],[]).
to_literal_list([H|T],[literal(H)|T2]) :-
	to_literal_list(T,T2).

