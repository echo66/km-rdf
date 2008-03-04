/**
	Feature Extraction module for the KM. It currently support Vamp Features, but we could use some other tools for feature extraction (c4dml).
	We define some routines to extract features, but the detailed interface must be checked at the sources. Complex queries for plugins can be
	done using the swivamp module. We can set parameters, programs, we can deal with complex inputs...
	David Pastor Escuredo. Jan 2008, c4dm, Queen Mary, University of London.

	Changes: retrieving different outputs in one run (28 Feb). need to check the rest of predicates
	*/

:-module(fe, [decode/2
				,	decode_mono/2
				,	decode_framed/4
				,	feature/2
				,	feature/1
				,	feature_of/3
				,	vamp_feature_of/3
				,	vamp_all_features_for/3
				,	vamp_outputs_for/4
				,	vamp_process_signal/7
				,	vamp_process_frame/4
				, 	vamp_process_frames/4
				, 	vamp_compute_feature/6
				,	vamp_compute_feature_frames/6
				]).

:-use_module('../swiaudiodata/audiodata').
:-use_module('../swivamp/vamp').
:-use_module('../swiaudiosource/audiosource').

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
	
select_plugin(Type, _, _):-
	Type = 'distancematrix',!, fail.

select_plugin(Type, _, _):-
	Type = 'distancevector',!, fail.

select_plugin(Type, _, _):-
	Type = 'sorteddistancevector',!, fail.

/**
	vamp_feature_of/3. We don't specify the framing as the plugin does it by itself. If there are not preferred parameters for framing, we use
	arbitrary ones Step=1024 Block=2048. If we need specific options for this we should use the framing version of this predicate.
	The main difference is that here we get the frames on the air instead of creating a framed version of the signal.
	If there is only one feature is better to flatten the returned list
*/
vamp_feature_of(Type, Signal, WholeFeature) :-
	select_plugin(Type, PluginKey, Output),
	vamp_outputs_for(Signal, PluginKey, [Output],F),
	flatten(F, WholeFeature).

/**
	Thus we get all the outputs for a plugin automatically. The returned list is quite messy, should do something with it here
*/

vamp_all_features_for(PluginKey, Signal, WholeFeatureSet):-
	vamp_plugin_numberOutputs(PluginKey, N),
	numlist(0, N-1, Outputs),
	vamp_outputs_for(Signal, PluginKey, Outputs, WholeFeatureSet).

/**
	vamp_outputs_for(+Signal, +PluginKey, +Outputs, -WholeFeature):-

	where Outputs = [0, 2, 3] for example

	and WholeFeature is [[[Feature1Out1Frame1,..], [Feature1Out2Frame1, ...]], Frame2, ...]

	there is one exception on this. If there are only remaining features the list will be simpler:

	WholeFeature (=Remaining) is [[Output1], [Output2], ...]

	not flatten as we may need to know this structure to retrieve them. We only delete [] lists

	It's the same that vamp_compute_feature/5 returns
*/

vamp_outputs_for(Signal, PluginKey, Outputs, WholeFeature):-
	vmpl_load_plugin_for(PluginKey, Signal, Plugin),
	set_blockSize(Plugin, BlockSize),
	set_stepSize(Plugin, StepSize),
	get_channels(Signal, Channels),
	vmpl_initialize_plugin(Plugin, Channels, StepSize, BlockSize),
	vamp_compute_feature(Signal, StepSize, BlockSize, Outputs, Plugin, WholeFeature).

/**
	This predicate allow us to compute features given the entire signal or a framed version of it.
	It returns the whole list of features collected from the non-deterministic predicate
	
	Note: output is a list with the number of outputs to retrieve
*/
vamp_compute_feature(Signal, StepSize, BlockSize, Output, Plugin, WholeFeature):-
	get_samples_per_channel(Signal, Samples),
	findall(Features, vamp_process_signal(Signal, Samples, StepSize, BlockSize, Output, Plugin, Features), FeatureSet),
	get_sample_rate(Signal, SampleRate),	
	vmpl_remaining_features(Plugin, Samples, SampleRate, Output, Remaining),
	append(FeatureSet, Remaining, RawFeatures),
	delete(RawFeatures, [], WholeFeature).

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
	This other predicate allows to process a set of frames as framed signal instead of the original term for the signal.
	Remember that Output is a list
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
		
		vamp_process_signal(+Signal, +Samples/Channel, +StepSize, +BlockSize, +OutputList, +Plugin, -FeatureSet)
*/
vamp_process_signal(Signal, Samples, StepSize, BlockSize, Output, Plugin, FeatureSet):-	
	set_limit_framing(Samples, StepSize, Limit),	
	set_framing(StepSize, Samples, Limit, Start),
	get_frame(Signal, Start, BlockSize, Frame),
	vamp_process_frame(Plugin, Frame, Output, FeatureSet),
	clean_frame(Frame).

/**
	Process all the members of a framed signal. 
*/
vamp_process_frames(Frames, Output, Plugin, FeatureSet):-
	member(Frame, Frames),
	vamp_process_frame(Plugin, Frame, Output, FeatureSet),
	clean_frame(Frame).

/**
	Just returns the features for a given frame and output. This may very useful if we demand features in a frame basis

		vamp_process_frame(+Plugin, +Frame, +Output, -Features):-

	The plugin is a working and initialized plugin

	Output is a list!
*/
vamp_process_frame(Plugin, Frame, Output, Features):-
	get_frame_timestamp(Frame, FrameTimeStamp),	
	vmpl_process_block(Plugin, Frame, FrameTimeStamp, Output, Features).


/**********************************************************************************
				SPECIFIC FEATURES PROCEDURES
***********************************************************************************/

/**
	MFCC parameters.
	returns just the data id containing the mean and var
*/
vamp_mfcc_param(Signal, DMean, DVar):-
	mix_stereo(Signal, S),
	vmpl_load_plugin_for('libqm-vamp-plugins:qm-similarity', S, Plugin),
	vmpl_set_parameter(Plugin, 'featureType', 0),
	set_blockSize(Plugin, BlockSize),
	set_stepSize(Plugin, StepSize),
	get_channels(S, Channels),
	vmpl_initialize_plugin(Plugin, Channels, StepSize, BlockSize),
	vamp_compute_feature(S, StepSize, BlockSize, [3,4], Plugin, WholeFeature),
	WholeFeature = [Mean, Var],
	Mean = ['Feature'(_TypeM, _TimeM, DMean)],
	Var = ['Feature'(_TypeV, _TimeV, DVar)].

vamp_beatSpectra_of(Signal, Beat):-
	mix_stereo(Signal, S),
	vmpl_load_plugin_for('libqm-vamp-plugins:qm-similarity', S, Plugin),
	vmpl_set_parameter(Plugin, 'featureType', 4),
	set_blockSize(Plugin, BlockSize),
	set_stepSize(Plugin, StepSize),	
	vmpl_initialize_plugin(Plugin, 1, StepSize, BlockSize),
	vamp_compute_feature(S, StepSize, BlockSize, [5], Plugin, F),
	flatten(F, WholeFeature),
	WholeFeature = 'Feature'(_T, _TS, Beat).

vamp_similarity_features(Signal, DMean, DVar, BeatSpec):-
	mix_stereo(Signal, S),
	vmpl_load_plugin_for('libqm-vamp-plugins:qm-similarity', S, Plugin),
	vmpl_set_parameter(Plugin, 'featureType', 1),
	set_blockSize(Plugin, BlockSize),
	set_stepSize(Plugin, StepSize),	
	vmpl_initialize_plugin(Plugin, 1, StepSize, BlockSize),
	vamp_compute_feature(S, StepSize, BlockSize, [3,4,5], Plugin, F),
	F = [Mean, Var, Beat],
	Mean = ['Feature'(_TypeM, _TimeM, DMean)],
	Var = ['Feature'(_TypeV, _TimeV, DVar)],
	Beat = ['Feature'(_T, _TS, BeatSpec)].

/**This won't work**/

vamp_histogram(Signal, Histogram):-
	mix_stereo(Signal, S),
	vmpl_load_plugin_for('libqm-vamp-plugins:qm-similarity', S, Plugin),
	vmpl_set_parameter(Plugin, 'featureType', 2),
	set_blockSize(Plugin, BlockSize),
	set_stepSize(Plugin, StepSize),	
	vmpl_initialize_plugin(Plugin, 1, StepSize, BlockSize),
	vamp_compute_feature(S, StepSize, BlockSize, [3], Plugin, F),
	F = [Hist],
	Hist = ['Feature'(_TypeM, _TimeM, Histogram)].









