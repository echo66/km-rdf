:- module(vamp_builtins,[]).


:- use_module('../builtins').
:- use_module(library('semweb/rdf_db')).


:- use_module('../swivamp/vamp').
:- use_module('../swivamp/vamp_transform').
:- use_module('../swidata/data').
:- use_module('../swiaudio/audio').
:- use_module('musicutils').

builtins:builtin('http://purl.org/ontology/vamp/qm-keydetector',vamp_builtins:keydetector).
%builtins:builtin('http://purl.org/ontology/vamp/qm-mfccparameters',vamp_builtins:mfccparameters).
%builtins:builtin('http://purl.org/ontology/vamp/qm-similarity',vamp_builtins:similarity).
%builtins:builtin('http://purl.org/ontology/vamp/qm-beats',vamp_builtins:beats).


/**
	Outputs a to:key. I still need to put some rules for the feature output (lists against blobs and timestamps)
*/

keydetector(Input,Features) :-
	nonvar(Input),
	Input = [literal(SR), Sigs],
	transform(signal(SR,Sigs),'qm-vamp-plugins','qm-keydetector',[key],RF),
	flatten(RF, F),
	findall([literal(Otp),literal(TS),literal(TE),Key],(member(feature(Otp,timestamp(TS,TE),[Data]),F), to_key(Data, Key)),Features).

/**
	Specific builtins to extract MFCC means and vars from the similarity plugin. Necessary to mix down the input to not confuse the plugin with 2
	different tracks. Just timbre mode. (note: how to combine different similarities?)


%These wont work, but we have to change them anyway
mfccparameters(Input, [MLData, VLData]) :-
	nonvar(Input),
	Input = [literal(Channels),literal(SR),literal(L),Sigs],
	mix_stereo('Signal'(Channels, SR, L, Sigs), 'Signal'(ChannelsM, SRM, LM, SigsMono)),
	vmpl_load_plugin('qm-vamp-plugins:qm-similarity',SR,Plugin),
	vmpl_set_parameter(Plugin, 'featureType', 0),
	vmpl_get_blockSize(Plugin, BlockSize),
	vmpl_get_stepSize(Plugin, StepSize),
	vmpl_initialize_plugin(Plugin, 1, StepSize, BlockSize),
	vamp_compute_feature2('Signal'(ChannelsM, SRM, LM, SigsMono), StepSize, BlockSize, [3,4], Plugin, F),
	F= [Mean, Var],
	Mean = ['Feature'(_MType, 'Timestamp'(_StartM, _EndM), MBinData)],
	Var = ['Feature'(_VType, 'Timestamp'(_StartV, _EndV), VBinData)],
	data(MBinData,MData),
	to_literal_list(MData,MLData),
	data(VBinData,VData),
	to_literal_list(VData,VLData).

similarity(Input, [MLData, VLData, BLData]) :-
	nonvar(Input),
	Input = [literal(Channels),literal(SR),literal(L),Sigs],
	mix_stereo('Signal'(Channels, SR, L, Sigs), 'Signal'(ChannelsM, SRM, LM, SigsMono)),
	vmpl_load_plugin('qm-vamp-plugins:qm-similarity',SR,Plugin),
	vmpl_set_parameter(Plugin, 'featureType', 1),
	vmpl_get_blockSize(Plugin, BlockSize),
	vmpl_get_stepSize(Plugin, StepSize),
	vmpl_initialize_plugin(Plugin, 1, StepSize, BlockSize),
	vamp_compute_feature2('Signal'(ChannelsM, SRM, LM, SigsMono), StepSize, BlockSize, [3,4, 5], Plugin, F),
	F= [Mean, Var, Beat],
	%see fe.pl to for vamp_compute_feature
	Mean = ['Feature'(_MType, 'Timestamp'(_StartM, _EndM), MBinData)],
	Var = ['Feature'(_VType, 'Timestamp'(_StartV, _EndV), VBinData)],
	Beat = ['Feature'(_BType, 'Timestamp'(_StartB, _EndB), BBinData)],
	data(MBinData,MData),
	to_literal_list(MData,MLData),
	data(VBinData,VData),
	to_literal_list(VData,VLData),
	data(BBinData,BData),
	to_literal_list(BData,BLData).

beats(Input, BeatDetec):-
	nonvar(Input),
	Input = [literal(Channels),literal(SR),literal(L),Sigs],
	vmpl_load_plugin('qm-vamp-plugins:qm-tempotracker',SR,Plugin),
	vmpl_get_blockSize(Plugin, BlockSize),
	vmpl_get_stepSize(Plugin, StepSize),
	vmpl_initialize_plugin(Plugin, 1, StepSize, BlockSize),
	vamp_compute_feature2('Signal'(Channels, SR, L, Sigs), StepSize, BlockSize, [0], Plugin, F),
	flatten(F, BeatsList),
	findall(literal(BeatPoint), member('Feature'(_T, 'Timestamp'(BeatPoint, _Res), _NoData), BeatsList), BeatDetec).

*/

to_literal_list([],[]).
to_literal_list([H|T],[literal(H)|T2]) :-
	to_literal_list(T,T2).

