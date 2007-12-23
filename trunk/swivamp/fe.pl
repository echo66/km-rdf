
/**
	FEATURE EXTRACTION PROGRAM FOR VAMP PLUGINS
	Loading KM modules (paths of the system). Should be compilled together later on
	*/

:-use_module('../swiaudiodata/audiodata').
:-use_module('../swivamp/vamp').
:-use_module('../swiaudiosource/audiosource').

:-style_check(-discontiguous).

feature_of(FeatureType, AudioFile, Feature):-
	findall([Features, Remaining], get_feature_of(FeatureType, AudioFile, Features, Remaining), RawList),
	flatten(RawList, Feature).

get_feature_of(FeatureType, AudioFile, Features, Remaining) :-
	vamp_plugin_for(PluginKey, FeatureType, Output),
	vamp_output(PluginKey, AudioFile, Output, Features, Remaining).

vamp_output(PluginKey,AudioFile,Output,Features, Remaining) :-
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
	





