/**
	Feature Extraction module for the KM
	David Pastor Escuredo. Jan 2008, c4dm, Queen Mary, University of London.
	ToDo: formalize this properly and documentation.
	So far, we just retrieve vamp features.
	*/

:-module(feature_extraction, [feature_of/3
				,	get_feature_of/4
				,	vamp_output/5
				]).

:-use_module('/home/david/km-rdf/swiaudiodata/audiodata').
:-use_module('/home/david/km-rdf/swivamp/vamp').
:-use_module('/home/david/km-rdf/swiaudiosource/audiosource').

:-style_check(-discontiguous).

feature_of(FeatureType, Signal, Feature):-
	findall([Features, Remaining], get_feature_of(FeatureType, Signal, Features, Remaining), RawList),
	flatten(RawList, Feature).

get_feature_of(FeatureType, Signal, Features, Remaining) :-
	vamp_plugin_for(PluginKey, FeatureType, Output),
	vamp_output(PluginKey, Signal, Output, Features, Remaining).

vamp_output(PluginKey, Signal, Output, Features, Remaining) :-
	vmpl_load_plugin_for(PluginKey,Signal,Plugin),
	vmpl_get_blockSize(Plugin, BlockSize),
	vmpl_get_stepSize(Plugin, StepSize),
	get_channels(Signal, Channels),
	vmpl_initialize_plugin(Plugin, Channels, StepSize, BlockSize),
	get_samples_per_channel(Signal, Samples),
	set_framing(StepSize, Samples, Limit, Start),
	get_frame(Signal, Start, BlockSize, Frame),
	get_frame_timestamp(Frame, FrameTimeStamp),
	get_sample_rate(Signal, SampleRate),
	vmpl_process_block(Plugin, Frame, FrameTimeStamp, Output, Features),
	get_remaining_features(Start, StepSize, Limit, Plugin, Samples, SampleRate, Output, Remaining).
	
get_remaining_features(Start, StepSize, Limit, Plugin, Samples, SampleRate, Output, Features):-
	LastStart is StepSize * Limit,
	Start = LastStart,
	!,
	vmpl_remaining_features(Plugin, Samples, SampleRate, Output, Features);
	Features = [].

set_framing(StepSize, Samples, Limit, Start):-
	set_limit_framing(Samples, StepSize, Limit),
	between(0, Limit, N),
	Start is StepSize * N.
	





