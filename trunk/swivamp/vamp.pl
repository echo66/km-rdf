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

:- module(vamp,[vamp_plugin_system/1,
		vamp_feature_system/1,
		vamp_plugin_for/3]).



:- style_check(-discontiguous).
:- load_foreign_library(swivamp).

/**
	It loads the shared library "swivamp.so" containing the Vamp/SWI-Prolog interface that integrates the host definition libvamp-hostsdk (Chris 		Cannam). Some of the main concepts of the this interface are inherited from this host defintion. Check http://www.vamp-plugins.org/code-doc/

	*/

/** 
	vamp_plugins_sytem/1 This predicates checks if the Plugin is on the system (PluginKey nomenclature description is library:pluginName)
	This predicate is non-deterministic

	*/

vamp_plugin_system(PluginKey):-
	vmpl_plugins(ListOfPlugins),
	member(PluginKey, ListOfPlugins).

/** 
	vamp_feature_system/1 says if the feature can be computed by the systems (plugin installed or not). IF not, the plugin should be installed. To 
	know which plugin we need we use vmpl_plugin_for. 

	*/
vamp_feature_system(FeatureType):-
	vamp_plugin_for(FeatureType, PluginKey, _),
	vamp_plugin_system(PluginKey).

/**
	Definition of the plugin necessary for each feature and the output from which they are obtained. Necessary for the host...
	This are not system dependent, they are based in general knowledge and defintion of existing plugins. It's a sort of database storing
	metadata of plugins in prolog for non-deterministic queries
	*/

vamp_plugin_for(FeatureType, PluginKey, Output):-
	FeatureType ='tempo', 
	PluginKey = 'qm-vamp-plugins:qm-tempotracker',
	Output = 2.

vamp_plugin_for(FeatureType, PluginKey, Output):-
	FeatureType ='beats', 
	PluginKey = 'qm-vamp-plugins:qm-tempotracker',
	Output = 0.

vamp_plugin_for(FeatureType, PluginKey, Output):-
	FeatureType ='detection_fn', 
	PluginKey = 'qm-vamp-plugins:qm-tempotracker',
	Output = 1.

vamp_plugin_for(FeatureType, PluginKey, Output):-
	FeatureType ='chromagram', 
	PluginKey = 'qm-vamp-plugins:qm-chromagram',
	Output = 0.

vamp_plugin_for(FeatureType, PluginKey, Output):-
	FeatureType ='mode', 
	PluginKey = 'qm-vamp-plugins:qm-keydetector',
	Output = 1.

vamp_plugin_for(FeatureType, PluginKey, Output):-
	FeatureType ='tonic', 
	PluginKey = 'qm-vamp-plugins:qm-keydetector',
	Output = 0.

vamp_plugin_for(FeatureType, PluginKey, Output):-
	FeatureType ='key', 
	PluginKey = 'qm-vamp-plugins:qm-keydetector',
	Output = 2.

vamp_plugin_for(FeatureType, PluginKey, Output):-
	FeatureType ='constantq', 
	PluginKey = 'qm-vamp-plugins:qm-constantq',
	Output = 0.

vamp_plugin_for(FeatureType, PluginKey, Output):-
	FeatureType ='tctransform', 
	PluginKey = 'qm-vamp-plugins:qm-tonalchange',
	Output = 0.

vamp_plugin_for(FeatureType, PluginKey, Output):-
	FeatureType ='tcfunction', 
	PluginKey = 'qm-vamp-plugins:qm-tonalchange',
	Output = 1.

vamp_plugin_for(FeatureType, PluginKey, Output):-
	FeatureType ='changepositions', 
	PluginKey = 'qm-vamp-plugins:qm-tonalchange',
	Output = 2.



