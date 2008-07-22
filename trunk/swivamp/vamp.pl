/**
	This is the Prolog bridge to the Vamp Plugins API, written by
	Chris Cannam for the c4dm at Queen Mary University of London.
	Thus, it is possible to deal with the plugins from a SWI-Prolog machine.
	
	It loads the shared library "swivamp.so" containing the Vamp/SWI-Prolog interface that integrates the host definition libvamp-hostsdk (Chris 		Cannam). Some of the main concepts of the this interface are inherited from this host defintion. Check <www.vamp-plugins.org/>
	
	There are two types of names:

		vamp_xxx These can be done as direct calls on prolog (designed on prolog). They are documented in this file. These predicates have a PluginKey as reference to the plugin (libraryidentifier:pluginidentifier)

		vmpl_xxx These should be part of a bigger query (but maybe they can be called isolated). They are the plain interface (see source). These predicates handle a Prolog atom identifying a working instance (__plugin::vamp_id)


	Centre for Digital Music, Queen Mary, University of London.
	Copyright (c) 2008 David Pastor Escuredo and QMUL.

	@author David Pastor Escuredo	
	@license GPL
	@version 1.0
*/

:- module(vamp,[

		/**Our plugin library*/
		vamp_plugin_system/1    			
	,	vmpl_plugins/1
	,	vamp_plugin_maker/2
	,	vamp_plugin_identifier/2
	,	vamp_plugin_name/2
	,	vamp_plugin_description/2
	,	vamp_plugin_copyright/2
	,	vamp_plugin_vampVersion/2
	,	vamp_plugin_version/2

		/** 1st step lifecycle: querying available outputs */
	,	vmpl_plugin_numberOutputs/2
	,	vamp_feature_system/1
	,	vamp_plugin_for/3
	, 	vamp_plugin_numberOutputs/2

		/** 2nd step lifecycle: loading. We need the sample rate*/			
	,	vmpl_load_plugin/3
	,	vmpl_load_plugin_for/3
	
		/** 3rd step lifecycle: querying, setting programs and parameters. Work with instances not PluginKeys (not finished)*/
	,	vmpl_pluginPrograms/2
	,	vmpl_select_program/2
	,	vmpl_pluginParameters/2
	,	vmpl_set_parameter/3

		/** 4th step lifecycle: querying preferred initializing arguments */
	,	vmpl_get_blockSize/2
	,	vmpl_get_stepSize/2
	,	vmpl_get_min_channel/2
	,	vmpl_get_max_channel/2

		/** 5th step lifecycle: initializing (from here one, we have a specific and single OutputDescriptor) */
	,	vmpl_initialize_plugin/4

		/** 6th step lifecycle: output (before, the output wasn't determined so may work with instances and not with keys) 
		Note: xxx/3 has the index of the output as argument */
	,	vmpl_outputDescriptor_identifier/3	
	,	vmpl_outputDescriptor_name/3
	,	vmpl_outputDescriptor_description/3
	,	vmpl_outputDescriptor_unit/3
	,	vmpl_outputDescriptor_hasFixedBinCount/2
	,	vmpl_outputDescriptor_binCount/3
	,	vmpl_outputDescriptor_hasKnownExtents/2
	,	vmpl_outputDescriptor_minValue/3
	,	vmpl_outputDescriptor_maxValue/3
	,	vmpl_outputDescriptor_isQuantized/2
	,	vmpl_outputDescriptor_quantizeStep/3
	,	vmpl_outputDescriptor_sampleType/3
 	,	vmpl_outputDescriptor_sampleRate/3
		
		/**Advanced predicates to query the output */
	,	vamp_plugin_output_sampleType/3
	, 	vamp_plugin_output_metadata/6

		/** 7th step lifecycle: process frames (call this predicate for all the frames iteratively). List of outputs to retrieve */
	,	vmpl_process_block/5
	,	vmpl_process_block_framing/6

		/** 8th step lifecycle: at the end and just once, collect remaining features if any. List of outputs to retrieve */
	,	vmpl_remaining_features/5

		/** 9th step lifecycle: reset and initialize again */
	,	vmpl_plugin_reset/1

		/**free space*/
	,	vmpl_destroy_plugin/1
	]).

:- style_check(-discontiguous).
:- load_foreign_library(swivamp).
%:- use_module(library(pldoc)).
:- use_module('../swiaudio/audio').
:- use_module('../swidata/data').

%% vmpl_plugins(+ListPlugins) is det
% A list of available plugins after scanning

%% vamp_plugin_system(?PluginKey) is nondet
% Checks plugins available in your local system

vamp_plugin_system(PluginKey):-
	vmpl_plugins(ListOfPlugins),
	member(PluginKey, ListOfPlugins).

%% vamp_feature_system(?PluginKey) is nondet
% Checks for vamp features computed in your local system (output types for each plugin)

vamp_feature_system(FeatureType):-
	findall(FeatureTypeIn, vamp_plugin_for(_, FeatureTypeIn, _), AvailableFeatures),
	member(FeatureType, AvailableFeatures).

%% vamp_plugin_for(?PluginKey, ?FeatureType, ?Index) is nondet
% Finds out about relationships between the Plugin key, the different outpout (feature) types and the position they occupy in the plugin.

vamp_plugin_for(PluginKey, FeatureType, Index):-
	var(FeatureType),
	vamp_plugin_system(PluginKey),
	vmpl_load_plugin(PluginKey, 44100, Plugin),
	vmpl_plugin_numberOutputs(Plugin, Size),
	vamp_plugin_features(Plugin, Size, FeatureType, Index).
vamp_plugin_for(PluginKey, FeatureType, Index):-
	nonvar(PluginKey),
	nonvar(FeatureType),
	is_feature_of_plugin(PluginKey, FeatureType, Index).
vamp_plugin_for(PluginKey, FeatureType, Index):-
	var(PluginKey),
	var(Index),
	nonvar(FeatureType),
	vamp_plugin_system(Plugin),
	is_feature_of_plugin(Plugin, FeatureType, Index),
	PluginKey = Plugin.

%% is_feature_of_plugin(+Plugin, ?Feature, ?Index) is nondet
% Given a plugin we can check its features and their position

is_feature_of_plugin(Plugin, Feature, Index):-
	nonvar(Plugin),
	vamp_plugin_features(Plugin, List),
	member([Feature,  Index], List).
vamp_plugin_features(Plugin, OutIn):-
	nonvar(Plugin),
	findall([Output, In], vamp_plugin_for(Plugin, Output, In), OutIn).
vamp_plugin_features(Plugin, Size, FeatureType, Index):-
	Limit is Size-1,
	between(0, Limit, Index),
	vmpl_outputDescriptor_identifier(Plugin, Index, FeatureType).

%% vamp_plugin_numberOutputs(?PluginKey, -Size) is semidet
% Gets the number of outputs of each plugin on the system given the key (difference with vmpl_x)

vamp_plugin_numberOutputs(PluginKey, Size):-
	vamp_plugin_system(PluginKey),
	vmpl_load_plugin(PluginKey, 44100, Plugin),
	vmpl_plugin_numberOutputs(Plugin, Size).

%% vamp_plugin_maker(+PluginKey, -Maker) is nondet
% Gets the maker of the plugin

vamp_plugin_maker(PluginKey, Maker):-
	vmpl_load_plugin(PluginKey, 44100, Plugin),
	vmpl_get_maker(Plugin, Maker).

%% vamp_plugin_identifier(+PluginKey, -Identifier) is semidet
% Gets the plugin machine-readable identifier

vamp_plugin_identifier(PluginKey, Identifier):-
	vmpl_load_plugin(PluginKey, 44100, Plugin),
	vmpl_get_identifier(Plugin, Identifier).

%% vamp_plugin_name(+PluginKey, -Name) is semidet
% Gets the plugin human-readable identifier

vamp_plugin_name(PluginKey, Name):-
	vmpl_load_plugin(PluginKey, 44100, Plugin),
	vmpl_get_name(Plugin, Name).

%% vamp_plugin_copyright(+PluginKey, -Copyright) is semidet
% Gets the plugin copyright

vamp_plugin_copyright(PluginKey, Copyright):-
	vmpl_load_plugin(PluginKey, 44100, Plugin),
	vmpl_get_copyright(Plugin, Copyright).

%% vamp_plugin_description(+PluginKey, -Description) is det
% Gets the plugin description as atom

vamp_plugin_description(PluginKey, Description):-
	vmpl_load_plugin(PluginKey, 44100, Plugin),
	vmpl_get_description(Plugin, Description).

%% vamp_plugin_vampVersion(+PluginKey, -VampAPIVersion) is semidet
% Gets the vamp API version used in this plugin

vamp_plugin_vampVersion(PluginKey, VampVersion):-
	vmpl_load_plugin(PluginKey, 44100, Plugin),
	vmpl_get_vampVersion(Plugin, VampVersion).

%% vamp_plugin_version(+PluginKey, -Version) is semidet
% Gets the plugin version

vamp_plugin_version(PluginKey, Version):-
	vmpl_load_plugin(PluginKey, 44100, Plugin),
	vmpl_get_pluginVersion(Plugin, Version).

%% vmpl_load_plugin(+PluginKey, +Sr, -Plugin) is semidet
% Loads a plugin for the given sample rate

%% vmpl_load_plugin_for(+PluginKey, +Input, -Plugin) is semidet
% Loads a plugin for the given input

vmpl_load_plugin_for(P, signal(Sr, _), Plugin):-
	vmpl_load_plugin(P, Sr, Plugin).

vmpl_load_plugin_for(P, frame(Sr, _, _), Plugin):-
	vmpl_load_plugin(P, Sr, Plugin).

%% vamp_plugin_output_sampleType(?PluginKey, ?FeatureType, -SampleType) is nondet
% Returns the sample type of the outputs of a plugin

vamp_plugin_output_sampleType(PluginKey, FeatureType, SampleType):-
	vamp_plugin_system(PluginKey),
	vmpl_load_plugin(PluginKey, 44100, Plugin),
	vamp_plugin_for(PluginKey, FeatureType, Index),
	vmpl_outputDescriptor_sampleType(Plugin, Index, SampleTypeIndex),
	vamp_sampleTypes(SampleTypeIndex, SampleType).

vamp_sampleTypes(SampleTypeIndex, SampleType):-
	SampleTypeIndex = 0,
	SampleType = 'OneSamplePerStep'.

vamp_sampleTypes(SampleTypeIndex, SampleType):-
	SampleTypeIndex = 1,
	SampleType = 'FixedSampleRate'.

vamp_sampleTypes(SampleTypeIndex, SampleType):-
	SampleTypeIndex = 2,
	SampleType = 'VariableSampleRate'.

%% vmpl_plugin_numberOutputs(+Plugin, -NumberOutputs) is semidet
% Returns the number of outputs for the working plugin

%% vamp_plugin_output_metadata(?PluginKey, ?FeatureType,-OutputName, -Description, -Unit, -OutputIndex) is nondet
% General information about the plugin outputs

vamp_plugin_output_metadata(PluginKey, FeatureType, OutputName, Description, Unit, OutputIndex):-
	vamp_plugin_system(PluginKey),
	vmpl_load_plugin(PluginKey, 44100, Plugin),
	vamp_plugin_for(PluginKey, FeatureType, OutputIndex),
	vmpl_outputDescriptor_name(Plugin, OutputIndex, OutputName),
	vmpl_outputDescriptor_description(Plugin, OutputIndex, Description),
	vmpl_outputDescriptor_unit(Plugin, OutputIndex, Unit).

%% vmpl_pluginPrograms(+Plugin, -ListOfPrograms) is semidet
% Returns available programs for the plugin
	
%% vmpl_select_program(+Plugin, +Program) is semidet
% Selects a program setting the plugin

%% vmpl_pluginParameters(+Plugin, -ListOfParameters) is semidet
% Selects the processing parameters of the algorithm implemented in the working plugin instance

%% vmpl_set_parameter(+Plugin, -ParameterName, -Value) is semidet
% Sets a value for the given parameter of the working plugin

%% vmpl_get_blockSize(+Plugin, -PreferredBlockSize) is semidet
% Retrieves the preferred block size for framing of the working plugin 

%% vmpl_get_stepSize(+Plugin, -PreferredStepSize) is semidet
% Retrieves the preferred step size of the working plugin 
	
%% vmpl_get_min_channel(+Plugin, -MinChannels) is semidet
% Retrieves the minimum of accepted channels of the working plugin 

%% vmpl_get_max_channel(+Plugin, +MaxChannels) is semidet
% Retrieves the max of accpted channels of the working plugin

%% vmpl_initialize_plugin(+Plugin, +Channels, +BlockSize, +StepSize) is semidet
% Initializes the loaded plugin giving the number of channels and the actual hop and window size for framing
	
%% vmpl_process_block(+Plugin, +Frame, +FrameTimeStamp, +ListOfOutputs, -ListOfFeatures) is semidet
% Process a frame term and returns the outputs queried in the list as a feature terms list. See vmpl_process_blob_framing/6

vmpl_process_block(Plugin, Frame, timestamp(_Start, _Duration), ListOfOutputs, ListOfFeatures):-
	Frame = frame(Sr, Init, ListOfBlobids),
	get_channels(Frame, Ch),
	get_samples_per_channel(Frame, L),
	vmpl_process_store(Plugin, Ch, Sr, L, Init, ListOfBlobids),
	findall(Features, collect_features(Plugin, ListOfOutputs, Features), RawFeatures),
	delete(RawFeatures, [], ListOfFeatures).

%% vmpl_process_block_framing(+Plugin, +Signal, +StartSample, +Size, +ListOfOutputs, -ListOfFeatures) is semidet
% Similar to vmpl_process_block but the framing is done inside the C++ code (actually faster) so you need to pass the framing parameters. ListOfOutputs is a plain list with the outputs identifiers to retrieve in the process. ListOfFeatures is a list of features of the given frame which is sub-divided into lists for the features of the same type within the input data frame. SparseOutputs data is represented with a list and DenseOutputs use a BLOBID (the swivamp library outputs BLOBIDs for any type).
% deprecated vmpl_process_block_framing/6

vmpl_process_block_framing(Plugin, Signal, StartSample, Size, ListOfOutputs, ListOfFeatures):-
	Signal = signal(Sr, ListOfBlobids),
	get_channels(Signal, Ch),
	get_samples_per_channel(Signal, L),
	vmpl_process_store_framing(Plugin, Ch, Sr, L, ListOfBlobids, StartSample, Size),
	findall(Features, collect_features(Plugin, ListOfOutputs, Features), RawFeatures),
	delete(RawFeatures, [], ListOfFeatures).

%% vmpl_remaining_features(+Plugin, +LastSample, +SampleRate, +ListOfOutputs, -ListOfFeatures) is semidet
% Retrieves the last features that the plugin may have kept till the end of the process


vmpl_remaining_features(Plugin, L, SR, ListOfOutputs, ListOfFeatures):-
	vmpl_store_remaining(Plugin, L, SR),	
	findall(Features, collect_features(Plugin, ListOfOutputs, Features), ListOfFeatures).

collect_features(Plugin, ListOfOutputs, ListOfFeatures):-
	member(Output, ListOfOutputs),
	collect_feature(Plugin, Output, ListOfFeatures).

collect_feature(Plugin, Output, ListOfFeatures):-
	vmpl_featureSet_output(Plugin, Output, ListOfFeatures).
	%vmpl_outputDescriptor_sampleType(Plugin, Output, SampleType),
	%findall(Feature, constrain_feature(ListOfFeatures, SampleType, Feature), ListOfFeatures2).

constrain_feature(L, ST, F):-
	member(F1, L),
	F1 = feature(T, Time, Data),
	blob_size(Data, Le),
	format_for_data(Data, ST, Le, Data2),
	F = feature(T, Time, Data2).

%Sparse output has literals or lists as data representation
%May constrain the timestamp

%format_for_data(_Data, 2, 0, Data2):- Data2 = '__event_'.

%format_for_data(Data, 2, 1, Data2):- blob_list(Data, [Data2]).

format_for_data(Data, 2, _L, Data2):-
	blob_list(Data, Data2).

%Dense output keeps blobids for data representation

format_for_data(Data, 0, _, Data2):-
	Data2 = Data.
format_for_data(D, 1, _, D2):-
	D2 = D.
	

%% vmpl_plugin_reset(+Plugin) is semidet
% Comes back to the earlier stages of the lifecycle before initializing

%% vmpl_destroy_plugin(+Plugin) is semidet
% Unloads the plugin relieving memory


