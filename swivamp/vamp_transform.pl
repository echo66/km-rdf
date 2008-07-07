/**
	Vamp Transform Module on top of the Vamp module. This module defines procedures for feature extraction using the Vamp plugins and a high level predicate that wraps the call to the API as a DSP transform

	David Pastor Escuredo, c4dm, Queen Mary University of London
	2007-2008
	*/

:-module(vamp_transform, [
				transform/5
			,	transform/6
			,	vamp_compute_feature/6
			,	vamp_compute_feature2/6

	]).

:-use_module('../swiaudio/audio').
:-use_module('../swidata/data').
:-use_module('vamp').
%:-use_module('../feature-extractor/fe').
%:-use_module('../front-end/transform').
:-use_module(library('option')).

%% transform(+Signal, +LibraryId, +PluginId, +ListOfOutputIds, -FinalOutput) is semidet
% Call to the Vamp API passing arguments without options and returning a complex list as Final Output

transform(Signal,Lib,Id,Outputs,FinalOutputs):-
	transform(Signal,Lib,Id,Outputs,FinalOutputs,[]).

%% transform(+Signal, +LibraryId, +PluginId, +ListOfOutputIds, -FinalOutput, +Options) is semidet
% Transform predicate passing arguments. Options:

transform(Signal, Lib, Id, Outputs, FinalOutputs, Options):-
	is_list(Options),
	Signal = signal(Sr, _Data),
	get_channels(Signal, Ch),
	plugin_key(Lib, Id, Key),
	plugin_output_setting(Key, Outputs, Indexes),
	vmpl_load_plugin(Key, Sr, Plugin),
	((option(block(Block), Options, _),!) ; (vmpl_get_blockSize(Plugin,Block))),
        ((option(step(Step), Options, _),!) ; (vmpl_get_stepSize(Plugin,Step))),
        ((option(parameters(Params), Options, []),!) ; Params=[]),
        ((option(program(Prog), Options, _),!) ; true),
	vamp_set_parameters(Plugin, Params),
	set_program(Plugin, Prog),
	vmpl_initialize_plugin(Plugin, Ch, StepSize, BlockSize),
	vamp_compute_feature2(Signal, StepSize, BlockSize, Indexes, Plugin, FinalOutputs).

% concats the plugin key
plugin_key(Lib, Id, Key):-
	nonvar(Lib),
	nonvar(Id),
	atom_concat(Lib, :, HalfKey),
	atom_concat(HalfKey, Id, Key).
plugin_key(Lib, Id, _):-
	var(Lib), var(Id).

%Different configurations for plugin-output setting
plugin_output_setting(Key, Outputs, Indexes):-
	nonvar(Key),
	var(Outputs),
	vamp_plugin_system(Key),
	Indexes = [0].

%If we pass plugin and outputs we need to check validity
plugin_output_setting(Key, Outputs, Indexes):-
	nonvar(Key),
	nonvar(Outputs),
	is_list(Outputs),
	length(Outputs, L),
	L>0,
	plugin_outputs_matching(Key, Outputs, Indexes).

%We dont accept variable plugin and outputs
plugin_output_setting(Key, Outputs, _Indexes):-
	var(Key),
	var(Outputs),!,
	fail.

%Outputs and not plugin. I may need to cut backtracking here
plugin_output_setting(Key, Outputs, Indexes):-
	var(Key),
	nonvar(Outputs),
	is_list(Outputs),
	length(Outputs, L),
	L>0,
	plugin_outputs_matching(Key, Outputs, Indexes).
	
%matching outputs passed (as a list) and keys
plugin_outputs_matching(Key, Outputs, Indexes):-	
	nonvar(Key),
	findall(Index, plugin_output_matching(Key, Outputs, Index), Indexes).	

%worst case: outputs from different plugins? one at a time. We process one plugin for output even when used more than once
plugin_outputs_matching(Key, Outputs, Indexes):-
	var(Key),
	member(Output, Outputs),
	vamp_plugin_for(Key, Output, Index),
	Indexes = [Index].
	
%Will fail if one output does not come from the plugin given
plugin_output_matching(Key, Outputs, Index):-
	nonvar(Key), nonvar(Outputs),
	member(Output, Outputs),
	vamp_plugin_for(Key, Output, Index).
	
% api-specific routines for parameters setup
vamp_set_parameters(_, _).
vamp_set_parameters(Plugin, [H|T]):-
	H = parameter(Name, Value),
	vmpl_pluginParameters(Plugin, List),
	member(Name, List),
	vmpl_set_parameter(Plugin, Name, Value),
	vamp_set_parameters(Plugin, T).	

set_program(Plugin, Program):-
	nonvar(Program),
	vmpl_select_program(Plugin, Program).
set_program(_, P):-
	var(P).

%% vamp_compute_feature(+Signal, +StepSize, +BlockSize, +OutputsList, +Plugin, -Feature)
% Computes the plugin for the given set up returnin the outputs selected in the list. The framing necessary for the processing is done by swiaudio.

vamp_compute_feature(Signal, StepSize, BlockSize, Output, Plugin, WholeFeature):-
	get_samples_per_channel(Signal, Samples),
	findall(Features, vamp_process_signal(Signal, Samples, StepSize, BlockSize, Output, Plugin, Features), FeatureSet),
	get_sample_rate(Signal, SampleRate),	
	vmpl_remaining_features(Plugin, Samples, SampleRate, Output, Remaining),
	append(FeatureSet, Remaining, RawFeatures),
	delete(RawFeatures, [], WholeFeature).

%% vamp_compute_feature2(+Signal, +StepSize, +BlockSize, +OutputsList, +Plugin, -Feature)
% Computes the plugin for the given set up returnin the outputs selected in the list. The framing necessary for the processing is done in C++ code (is faster than the first version)

vamp_compute_feature2(Signal, StepSize, BlockSize, Output, Plugin, WholeFeature):-
	get_samples_per_channel(Signal, Samples),
	findall(Features, vamp_process_signal2(Signal, Samples, StepSize, BlockSize, Output, Plugin, Features), FeatureSet),
	get_sample_rate(Signal, SampleRate),	
	vmpl_remaining_features(Plugin, Samples, SampleRate, Output, Remaining),
	append(FeatureSet, Remaining, RawFeatures),
	delete(RawFeatures, [], WholeFeature).


%	This other predicate allows to process a set of frames as framed signal instead of the original term for the signal. Remember that Output is a list

vamp_compute_feature_frames(Frames, SamplesWholeSignal, SampleRate, Output, Plugin, WholeFeature):-
	findall([Features], vamp_process_frames(Frames, Output, Plugin, Features), FeatureSet),	
	vmpl_remaining_features(Plugin, SamplesWholeSignal, SampleRate, Output, Remaining),
	append(FeatureSet, Remaining, RawList),
	flatten(RawList, WholeFeature).

%	This predicate gets the signal and extracts the frames giving the parameters getting the features on the air for each frame. This is implemented as non-deterministic predicate. It cleans the data of each frame from memory after returning the feature
		
vamp_process_signal(Signal, Samples, StepSize, BlockSize, Output, Plugin, FeatureSet):-	
	set_limit_framing(Samples, StepSize, Limit),	
	set_framing(StepSize, Samples, Limit, Start),
	get_frame(Signal, Start, BlockSize, Frame),
	vamp_process_frame(Plugin, Frame, Output, FeatureSet),
	clean(Frame).

% The same as before but relying on C++ for framing to speed up and save BLOBIDs.

vamp_process_signal2(Signal, Samples, StepSize, BlockSize, Output, Plugin, FeatureSet):-	
	set_limit_framing(Samples, StepSize, Limit),	
	set_framing(StepSize, Samples, Limit, Start),
	vmpl_process_block_framing(Plugin, Signal, Start, BlockSize, Output, FeatureSet).

% Helping and atomic predicates

vamp_process_frames(Frames, Output, Plugin, FeatureSet):-
	member(Frame, Frames),
	vamp_process_frame(Plugin, Frame, Output, FeatureSet),
	clean(Frame).

vamp_process_frame(Plugin, Frame, Output, Features):-
	get_frame_timestamp(Frame, FrameTimeStamp),	
	vmpl_process_block(Plugin, Frame, FrameTimeStamp, Output, Features).

