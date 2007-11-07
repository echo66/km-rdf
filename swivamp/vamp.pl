/**
	This is the Prolog bridge to the Vamp Plugins API, written by
	Chris Cannam for the c4dm at Queen Mary University of London.
	Thus, it is possible to deal with the plugins from a SWI-Prolog machine.
	David Pastor Escuredo 2007 for c4dm, Queen Mary, University of London
	*/

/** 

	Prolog library to deal with Vamp Plugins. Most of the useful predicates are defined in swivamp.cpp as vmpl_X so they can be used from this 
	module or from other modules by using Vamp:predicate syntax

	NOTE: To use this interface it is necessary to load other modules to decode files and manage the audio data
		-/swiaudiosource/audiosource
		-/swiaudiodata/audiodata
	*/

:- module(vamp,[vamp_availablePlugin/1]).



:- style_check(-discontiguous).
:- load_foreign_library(swivamp).

/**
	It loads the shared library "swivamp.so" containing the Vamp/SWI-Prolog interface that integrates the host definition libvamp-hostsdk (Chris 		Cannam). Some of the main concepts of the this interface are inherited from this host defintion. Check http://www.vamp-plugins.org/code-doc/

	*/

						/***************************************************
						********** Quering available Vamp plugins **********
						***************************************************/

/** 
	AvailablePlugins(?PluginName): This predicates checks if the Plugin is on the system (PluginKey nomenclature description is library:pluginName)
	This predicate is non-deterministic

	*/

vamp_plugin(PluginKey):-
	vmpl_plugins(ListOfPlugins),
	member(PluginKey, ListOfPlugins).

vmpl_plugin_for(FeatureType, Plugin, Output):-
	FeatureType ='tempo', 
	Plugin = 'qm-vamp-plugins:qm-tempotracker',
	Output = 2.

vmpl_plugin_for(FeatureType, Plugin, Output):-
	FeatureType ='beats', 
	Plugin = 'qm-vamp-plugins:qm-tempotracker',
	Output = 0.

vmpl_plugin_for(FeatureType, Plugin, Output):-
	FeatureType ='beats', 
	Plugin = 'qm-vamp-plugins:qm-tempotracker',
	Output = 0.

vmpl_plugin_for(FeatureType, Plugin, Output):-
	FeatureType ='chromagram', 
	Plugin = 'qm-vamp-plugins:qm-chromagram',
	Output = 0.


