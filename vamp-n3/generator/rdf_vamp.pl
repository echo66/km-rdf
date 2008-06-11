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
			,	rdf_description/1
			]).

:- style_check(-discontiguous).
:- load_foreign_library(template_generator).

:- use_module('../../swivamp/vamp').
%:- consult('vamp_af').

/**Define your namespace for vamp plugins. This should be automated**/

pluginlib(qm).
pluginlib(examples).
pluginlib('').
namespace(qm, 'http://purl.org/ontology/vamp/qm-plugins/').
namespace(examples, 'http://purl.org/ontology/vamp/examples/').
namespaces('', 'http://purl.org/ontology/vamp/plugins').

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

rdf_description(PluginKey):-
	vamp_plugin_system(PluginKey),
	get_library(PluginKey, Lib),
	rdf_description(PluginKey, Lib, 'http://www.vamp-plugins.org/').

rdf_description(PluginKey, PluginLibrary, YourURI):-
	vamp_plugin_system(PluginKey),
	pluginlib(PluginLibrary), namespace(PluginLibrary, Namespace),
	create_rdf_description(PluginKey, Namespace, YourURI).
	
	
