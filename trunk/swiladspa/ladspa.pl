/**
	LADSPA Module for audio analysis.
	This module defines a LADSPA host for SWI-Prolog allowing querying plugin meta-data and calling plugin processing. 
	Input and Output are MO::Signal functors defined in swilib.

	David Pastor Escuredo 2008, c4dm, Queen Mary University of London
*/

:-module(ladspa, [ldpl_plugin_system/1
		,	ldpl_plugins/1
		,	ldpl_plugin_maker/2
		,	ldpl_plugin_soname/2
		]).

:- style_check(-discontiguous).
:- load_foreign_library(swiladspa).

:-ldpl_loader.

/**
	Non-deterministic scan of plugins in system
	ldpl_plugin_system(?Plugin)
	*/
ldpl_plugin_system(Plugin):-
	ldpl_plugins(List),
	member(Plugin, List).
