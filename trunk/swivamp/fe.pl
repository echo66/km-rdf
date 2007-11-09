
/**
	Loading KM modules (paths of the system). Should be compilled together later on
	*/

:-use_module('/home/david/km-rdf/swiaudiodata/audiodata').
:-use_module('vamp').
:-use_module('/home/david/km-rdf/swiaudiosource/audiosource').


:-style_check(-discontiguous).

/**
	Declaration of features. This is like a database of all the possible features that we can extract named as they are in the local feature 		extraction APIs (so far few Vamp Plugins). Makes feature_of/3 non deterministic. Makes a prolog ground truth about KM features.
	*/

feature('tempo').
feature('beats').
feature('detection_fn').
feature('chromagram').
feature('mode').
feature('key').
feature('tonic').
feature('constantq').
feature('tctransform').
feature('tcfunction').
feature('changepositions').


feature_of(FeatureType, AudioFile, Features) :-
	feature(FeatureType),
	vamp_plugin_for(FeatureType, PluginKey, Output),
	vamp_output(PluginKey,AudioFile,Output,Features).

vamp_output(PluginKey,AudioFile,Output,Features) :-
	aspl_decode(AudioFile, Signal),
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
	vmpl_remaining_features(Start, StepSize, Limit, Plugin, Samples, SampleRate, Output, Features).
	

get_remaining_features(Start, StepSize, Limit, Plugin, Samples, SampleRate, Output, Features):-
	LastStart is StepSize * Limit,
	Start = LastStart,
	!,
	vmpl_remaining_features(Plugin, Samples, SampleRate, Output, Features).

set_framing(StepSize, Samples, Limit, Start):-
	set_limit_framing(Samples, StepSize, Limit),
	between(0, Limit, N),
	Start is StepSize * N.
	
/**
	Extract a feature from an already decoded frame. Unlikely to be useful
	*/

feature_of_single_frame(FeatureType, Frame, Feature):-
	feature(FeatureType),
	vamp_plugin_for(FeatureType, PluginKey, Output),
	get_sample_rate(Frame, SampleRate),
	vamp:vmpl_load_plugin(PluginKey, SampleRate, Plugin),
	vamp:vmpl_get_blockSize(Plugin, BlockSize),
	vamp:vmpl_get_stepSize(Plugin, StepSize),
	get_channels(Frame, Channels),
	vamp:vmpl_initialize_plugin(Plugin, Channels, StepSize, BlockSize),
	vamp:vmpl_process_block(Plugin, Frame, Output, Feature).



feature_of_frame_n(FeatureType, AudioFile, FrameNumber, Feature):-
	feature(FeatureType),
	aspl_decode(AudioFile, Signal),
	vamp_plugin_for(FeatureType, PluginKey, Output),
	vamp:vmpl_load_plugin_for(PluginKey, Signal, Plugin),
	vamp:vmpl_get_blockSize(Plugin, BlockSize),
	vamp:vmpl_get_stepSize(Plugin, StepSize),
	get_channels(Signal, Channels),
	vamp:vmpl_initialize_plugin(Plugin, Channels, StepSize, BlockSize),
	Start is FrameNumber*StepSize,
	get_frame(Signal, Start, BlockSize, Frame),
	get_frame_timestamp(Frame, FrameTimeStamp),
	vamp:vmpl_process_block(Plugin, Frame, FrameTimeStamp, Output, Feature).

