/**
	Feature Extraction module for the KM. It currently support Vamp Features, but we could use some other tools for feature extraction (c4dml).
	We define some routines to extract features, but the detailed interface must be checked at the sources. Complex queries for plugins can be
	done using the swivamp module. We can set parameters, programs, we can deal with complex inputs...
	David Pastor Escuredo. Jan 2008, c4dm, Queen Mary, University of London.

	Should be cleaning data frames as we compute the features???
	
	*/

:-module(fe, [decode/2
				,	decode_mono/2
				,	decode_framed/4
				,	feature/2
				,	feature/1
				,	feature_of/3
				,	vamp_feature_of/3
				,	vamp_process_signal/7
				,	vamp_process_frame/4
				, 	vamp_process_frames/4
				, 	vamp_compute_feature/6
				,	vamp_compute_feature_frames/6
				]).

:-use_module('/home/david/km-rdf/swiaudiodata/audiodata').
:-use_module('/home/david/km-rdf/swivamp/vamp').
:-use_module('/home/david/km-rdf/swiaudiosource/audiosource').

:-style_check(-discontiguous).

/**
	feature(?Feature): Checks for features in system. Now we have just Vamp ones, but should be able to add more defining similar predicates.
*/
feature(Feature, Api):-
	vamp_feature_system(Feature),
	Api='Vamp'.

feature(Feature):-
	feature(Feature, _).

/**
	decode(+File, -Signal): calling the audiosource one.
	Please note that so far the Signal returned is
		'Signal'(channels, samplerate, samples, [Listofpcm as __data_id])
	and we don't support more than 2 channel signals (to change).
*/
decode(AudioFile, Signal):-
	aspl_decode(AudioFile, Signal).

/**
	This ones forces the mixing to a mono signal
*/
decode_mono(AudioFile, Signal):-
	aspl_decode(AudioFile, MaybeStereo),
	mix_stereo(MaybeStereo, Signal).

/**
	decoded_framed(+AudioFile, +StepSize, +BlockSize, -ListOfFrames): we may want to retrieve a frames instead a whole signal. We must pass
	framing parameters.
		'Frame'(channels, samplerate, startingsample, [listofpcm as __data_id]) has the same channels than the parent signal
*/
decode_framed(AudioFile, StepSize, BlockSize, Frames):-
	aspl_decode(AudioFile, Signal),
	framed_signal(Signal, StepSize, BlockSize, Frames).
	
/**
	get_feature_of(?Type, +Signal, -Feature): this predicate hides any feature extraction process and library. 
	Signal can be a stereo, mono or framed signal. Should be a redefinition for each library
	We assume that Feature is a list of:
		'Feature'(type, 'timestamp'(start, duration), __data_id).
*/
feature_of(FeatureType, Signal, Feature):-
	feature(FeatureType, Api),
	Api = 'Vamp',
	vamp_feature_of(FeatureType, Signal, Feature).

/**
	There are some critical cases where a feature can be extracted from two plugins, we just one of them to not process them twice. This predicates
	help in that.
*/
select_plugin(Type, PluginKey, Output):-
	Type = 'means',!,PluginKey = 'libqm-vamp-plugins:qm-mfcc', Output=1;
	vamp_plugin_for(PluginKey, Type, Output).
	
select_plugin(Type, PluginKey, Output):-
	Type = 'variances',!, Type = 'mfccvariances',
	vamp_plugin_for(PluginKey, Type, Output).

select_plugin(Type, PluginKey, Output):-
	Type = 'distancematrix',!, false.

select_plugin(Type, PluginKey, Output):-
	Type = 'distancevector',!, false.

select_plugin(Type, PluginKey, Output):-
	Type = 'sorteddistancevector',!, false.

/**
	vamp_feature_of/3. We don't specify the framing as the plugin does it by itself. If there are not preferred parameters for framing, we use
	arbitrary ones Step=1024 Block=2048. If we need specific options for this we should use the framing version of this predicate.
	The main difference is that here we get the frames on the air instead of creating a framed version of the signal.
*/
vamp_feature_of(Type, Signal, WholeFeature) :-
	select_plugin(Type, PluginKey, Output),
	vmpl_load_plugin_for(PluginKey, Signal, Plugin),
	set_blockSize(Plugin, BlockSize),
	set_stepSize(Plugin, StepSize),
	get_channels(Signal, Channels),
	vmpl_initialize_plugin(Plugin, Channels, StepSize, BlockSize),
	vamp_compute_feature(Signal, StepSize, BlockSize, Output, Plugin, WholeFeature).

/**
	This predicate allow us to compute features given the entire signal or a framed version of it.
	It returns the whole list of features collected from the non-deterministic predicate
*/
vamp_compute_feature(Signal, StepSize, BlockSize, Output, Plugin, WholeFeature):-
	get_samples_per_channel(Signal, Samples),
	findall([Features], vamp_process_signal(Signal, Samples, StepSize, BlockSize, Output, Plugin, Features), FeatureSet),
	get_sample_rate(Signal, SampleRate),	
	vmpl_remaining_features(Plugin, Samples, SampleRate, Output, Remaining),
	append(FeatureSet, Remaining, RawList),
	flatten(RawList, WholeFeature).

/**
	Set the step and block sizes
*/
set_stepSize(Plugin, StepSize):-	
	vmpl_get_stepSize(Plugin, StepSize),
	StepSize = 0, !, StepSize is 1024; vmpl_get_stepSize(Plugin, StepSize).

set_blockSize(Plugin, BlockSize):-
	vmpl_get_blockSize(Plugin, BlockSize),
	BlockSize = 0, !, BlockSize is 2048; vmpl_get_blockSize(Plugin, BlockSize).

/**
	This other predicate allows to process a set of frames as framed signal instead of the original term for the signal
*/
vamp_compute_feature_frames(Frames, SamplesWholeSignal, SampleRate, Output, Plugin, WholeFeature):-
	findall([Features], vamp_process_frames(Frames, Output, Plugin, Features), FeatureSet),	
	vmpl_remaining_features(Plugin, SamplesWholeSignal, SampleRate, Output, Remaining),
	append(FeatureSet, Remaining, RawList),
	flatten(RawList, WholeFeature).

/**
	This predicate gets the signal and extracts the frames giving the parameters getting the features on the air for each frame. 
	This is implemented as non-deterministic predicate.
	It cleans the data of each frame from memory after returning the feature
		
		vamp_process_signal(+Signal, +Samples/Channel, +StepSize, +BlockSize, +Output, +Plugin, -FeatureSet)
*/
vamp_process_signal(Signal, Samples, StepSize, BlockSize, Output, Plugin, FeatureSet):-	
	set_limit_framing(Samples, StepSize, Limit),	
	set_framing(StepSize, Samples, Limit, Start),
	get_frame(Signal, Start, BlockSize, Frame),
	vamp_process_frame(Plugin, Frame, Output, FeatureSet),
	clean_frame(Frame).

/**
	Just returns the features for a given frame and output. This may very useful if we demand features in a frame basis

		vamp_process_frame(+Plugin, +Frame, +Output, -Features):-

	The plugin is a working and initialized plugin
*/
vamp_process_frame(Plugin, Frame, Output, Features):-
	get_frame_timestamp(Frame, FrameTimeStamp),	
	vmpl_process_block(Plugin, Frame, FrameTimeStamp, Output, Features).


/**
	Process all the members of the frame. 
*/
vamp_process_frames(Frames, Output, Plugin, FeatureSet):-
	member(Frame, Frames),
	vamp_process_frame(Plugin, Frame, Output, FeatureSet),
	clean_frame(Frame).


/**********************************************************************************
				SPECIFIC FEATURES PROCEDURES
***********************************************************************************/

feature('mfccmeans').
feature('mfccvariances').

vamp_plugin_for('libqm-vamp-plugins:qm-similarity', 'mfccvariances', 4).
vamp_plugin_for('libqm-vamp-plugins:qm-similarity', 'mfccmeans', 3).

vamp_feature_of('mfccmeans', Signal, WholeSignal):-
	mix_stereo(Signal, S),
	select_plugin('mfccmeans', Key, O),
	vmpl_load_plugin_for(Key, S, Plugin),
	set_blockSize(Plugin, BlockSize),
	set_stepSize(Plugin, StepSize),
	get_channels(Signal, Channels),
	vmpl_set_parameter('featureType', 0),
	vmpl_initialize_plugin(Plugin, Channels, StepSize, BlockSize),
	vamp_compute_feature(Signal, StepSize, BlockSize, O, Plugin, WholeFeature).

vamp_feature_of('mfccvariances', Signal, WholeSignal):-
	mix_stereo(Signal, S),
	select_plugin('mfccvariances', Key, O),
	vmpl_load_plugin_for(Key, S, Plugin),
	set_blockSize(Plugin, BlockSize),
	set_stepSize(Plugin, StepSize),
	get_channels(Signal, Channels),
	vmpl_set_parameter('featureType', 0),
	vmpl_initialize_plugin(Plugin, Channels, StepSize, BlockSize),
	vamp_compute_feature(Signal, StepSize, BlockSize, O, Plugin, WholeFeature).



/**will crash
vamp_feature_of('sbtimbral', Signal1, Signal2, Distance):-
	mix_stereo(Signal1, S1),
	mix_stereo(Signal2, S2),
	combine_sb_input(S1, S2, Input).
combine_sb_input('Signal'(_, _ , _, [Pcm1]), 'Signal'(_,_,_, [Pcm2]), 'Signal'(_,_,_,[Pcm1, Pcm2])).
**/









