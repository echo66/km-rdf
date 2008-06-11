/**
	Bit module to describe vamp plugins as rdf
	based on Chris Sutton's template_generator
	David Pastor 2008, c4dm
	TODO:
		Pair plugin-library automatically (maybe from swivamp or just analysing the name...)
		Specifies the type of plugin if it is not ok with the default one
		Dump into a file rather than on the screen
		
*/

:-module(rdf_vamp, [available_libraries/1
			,	rdf_description/2
			,	rdf_description/3
			]).

:- style_check(-discontiguous).
:- load_foreign_library(template_generator).

:- use_module('../../swivamp/vamp').

/**Define your namespace for vamp plugins**/

pluginlib(qm).
pluginlib(examples).
namespace(qm, 'http://purl.org/ontology/vamp/qm-plugins/').
namespace(examples, 'http://purl.org/ontology/vamp/examples/').

/**
	Returns the available libraries on the system. This is hardcoded, so requires manual updating

	available_libraries(-LibID)
*/
available_libraries(X):-
	pluginlib(X).

/**
	RDF descriptions:

	rdf_description(?PluginKey, ?PluginLibrary, +YourURI)

	You should introduce your plugin library your plugin key and your URI. You can get some information from swivamp :)
	eg. vamp_plugin_system(?PluginKey).

**/
rdf_description(PluginKey, PluginLibrary):-
	rdf_description(PluginKey, PluginLibrary, 'http://www.vamp-plugins.org/').

rdf_description(PluginKey, PluginLibrary, YourURI):-
	vamp_plugin_system(PluginKey),
	pluginlib(PluginLibrary), namespace(PluginLibrary, Namespace),
	create_rdf_description(PluginKey, Namespace, YourURI).
	
	
