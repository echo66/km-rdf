/**
	This is the bridge to the Vamp Plugins API, written by
	Chris Cannam for C4DM.
	Thus, it is possible to deal with the plugins from a SWI-Prolog machine.
	David Pastor Escuredo 2007
	*/

/** 

	Prolog library to deal with the Vamp Plugins

	*/

:- module(vamp,[plugins/1, 
		availablePlugin/1,
		plugin_for_audioFile/3, 
		plugin_for_information/1, 
		metaData/8,  
		plugin_outputs/2, 
		plugin_parameters/2, 
		plugin_programs/2, 
		initialize_plugin/1,
		prefInitPlugin_for_audioFile/2, 
		run_plugin_for_audioFile/4]).

/**
	Module vamp for the Knowledge Machine

	*/

:- style_check(-discontiguous).
:- load_foreign_library(vampswi).

/**
	It loads the interface "vampswi" for Vamp that integrates the host definition libvamp-hostsdk (Chris Cannam)

	*/


/**	
	PluginsList(-listOfPlugins): list of plugins in the system (Vamp predefined paths)

	*/

plugins(X):-
	pluginsList(X).

/** 
	AvailablePlugins(?PluginName): This predicates checks if the Plugin is on the system (PluginKey nomenclature description is library:pluginName)

	*/

availablePlugin(X):-
	plugins(Y),
	member(X,Y).

/** 
	plugin_for_audioFile(+PluginKey, +pathAudioFile, -audioFileChannels): loads the plugin given the key and the inputSampleRate of the tune
	passed. The adapter flag is passed as 0, but actually is implemented ADAPT_ALL by default.
	Instantiate_plugin can be used to create any plugin with a any inputSampleRate and any flag for the adapters
	The user is in charge of destroying the plugin once it has been used!!

	*/

plugin_for_audioFile(X,Y,C):-
	inputSampleRate(Y,Z),
	channels_information(Y,C),
	instantiate_plugin(X,Z,0).

/** 
	plugin_for_information(+PluginKey): This plugin is only useful to retrieve information as the inputSampleRate is set arbitrarly
	Instantiate_plugin can be used to create any plugin with a any inputSampleRate and any flag for the adapters
	The user is in charge of destroying the plugin once it has been used!!

	*/

plugin_for_information(X):-
	instantiate_plugin(X,128,0).

/**
	metaData(+PluginKey, - rest of arguments): This predicate queries all the meta data about the plugin given (key).

	*/

metaData(X,M,D,N,C,I,V,P):-	
	plugin_for_information(X),
	pluginMaker(M),
	pluginDescription(D),
	pluginName(N),
	pluginCopyright(C),
	pluginIdentifier(I),
	pluginVampVersion(V),
	pluginVersion(P),
	destroy_plugin.

/**
	The following predicates retrieves information about the plugin. The plugin is not instantiated with the correct input sample rate. 
	A plugin with a random isr is used to extract general information about that class of plugins. Other information like the number of values
	per output requires the correct instantiation and initialization of the plugin for a given audio file.

	*/

/**
	plugin_outputs(+PluginKey, - outputsList): This predicate returns a list of the available outputs of the plugin. It is necessary to 
	query this information to pass the number of output to the predicante run_plugin_for_audioFile (unless we already know the outputs)

	*/

plugin_outputs(X,Y):-
	plugin_for_information(X),
	pluginOutputs(Y),
	destroy_plugin.

/**
	plugin_parameters(+PluginKey, - parametersList): This predicate returns a list of the parameters of the plugin. This is only necessary if we 
	want to change the default settings of the plugin (it is not possible to do it yet)

	*/

plugin_parameters(X,Y):-
	plugin_for_information(X),
	pluginParameters(Y),
	destroy_plugin.

/**
	plugin_programs(+PluginKey, - programsList): This predicate returns a list of the programs of the plugin. This is only necessary if we 
	want to change the default settings of the plugin (it is not possible to do it yet)

	*/

plugin_programs(X,Y):-
	plugin_for_information(X),
	pluginPrograms(Y),
	destroy_plugin.

/**
	prefInitPlugin_for_audioFile(+pluginKey, +pathToAudioFile): Initialization with the preferred parameters (other may give rise to problems)
	and with the channels of the audio file as there is a channels adapter flagged and there is no need to take care of the mixing and multiplexing.
	We are in charge of deleting the plugin.
	*/

prefInitPlugin_for_audioFile(X,Y):-
	plugin_for_audioFile(X,Y,C),	
	initialize_plugin(C).	

/**
	run_plugin_for_audioFile(+pluginKey, +pathToAudioFile, +#output(starting from 0), -ListofFeatures)
	prolog List:  [output, #values/output, [sublist of features for input block 1], [sublist block 2]....]
	sublist:      [sec, nsec, values, sec, nsec, values, ...]
	one extension could be the possible settings (programs and parameters)
	*/

run_plugin_for_audioFile(X,Y,Z,R):-
	plugin_for_audioFile(X,Y,C),	
	initialize_plugin(C),
	run_plugin(Y,Z,R),
	destroy_plugin.
	







	
