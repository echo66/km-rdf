/**
	LADSPA Module for audio analysis.
	This module defines a LADSPA host for SWI-Prolog allowing querying plugin meta-data and calling plugin processing. 
	Input and Output are MO::Signal or MO::Frame functors defined in swilib.

	David Pastor Escuredo 2008, c4dm, Queen Mary University of London
*/

:-module(ladspa, [ldpl_plugin_system/1
		,	ldpl_plugins/1
		,	ldpl_plugin_maker/2
		,	ldpl_plugin_library/2
		,	ldpl_plugin_ports_count/2
		,	ldpl_input_audio/2
		,	ldpl_output_audio/2
		,	ldpl_input_control/2
		,	ldpl_output_control/2
		, 	ldpl_port_name_for/3
		,	ldpl_port_description/2
		,	ldpl_set_parameter/3
		,	ldpl_set_output_control/3
		,	ldpl_instantiate_plugin/4
		,	ldpl_connect_ports/1
		,	ldpl_set_default_controls/1
		,	ldpl_activate_plugin/1
		,	ldpl_run_plugin/3
		,	ldpl_deactivate_plugin/1
		, 	ldpl_cleanup_plugin/1
		]).

:- style_check(-discontiguous).
:- load_foreign_library(swiladspa).

:- use_module('../swiaudiodata/audiodata').
:- use_module('../swiaudiosource/audiosource').

:-ldpl_loader.

/*******************************************************
*** PATTERNS *******************************************
*******************************************************/

/* 	ldpl_plugins(-ListOfPluginsInSystem)

	ldpl_plugin_maker(+PluginName, -PluginMaker)

	ldpl_plugin_library(+PluginName, -LibraryOwningThePlugin)

	ldpl_plugin_ports_count(+PluginName, -NumberOfPorts)

	ldpl_input_audio(-PluginName, -ListOfIndexesOfInputAudioPorts) Eg: ldpl_input_audio('AmplifierStereo', [0, 1])

	ldpl_output_audio(-PluginName, -ListOfIndexesOfOutputudioPorts)

	...

	ldpl_instantiate_plugin(+Key, +SampleRate, +BlockSize, -Plugin): Creats a plugin. The blockSize is set at the beginning (I may change this)

	ldpl_connect_ports(+Plugin): The host connects the plugin to the specific ports

	ldpl_run_plugin(+ListOfData, +Plugin, +BlockSize): The data is passed as list of __data_id. Results are collected later from the output port 
*/

/**
	Non-deterministic scan of plugins in system
	ldpl_plugin_system(?Plugin)
	*/
ldpl_plugin_system(Plugin):-
	ldpl_plugins(List),
	member(Plugin, List).

/**
	Returns a functor 'LadspaPort'(Type, Index, Name)

	ldpl_port_description(?PluginName, 'LadpsaPort'(?Type, ?Index, -Name))
*/
ldpl_port_description(Name, 'LadspaPort'(T, I, N)):-
	ldpl_plugin_system(Name),
	ldpl_input_audio(Name, InAu),
	ldpl_output_audio(Name, OutAu),
	ldpl_input_control(Name, InC),
	ldpl_output_control(Name, OutC),
	(is_inAu(T, InAu, I);is_outAu(T, OutAu, I);is_inC(T, InC, I);is_outC(T, OutC, I);fail),
	ldpl_port_name_for(Name, I, N).	
is_inAu(T, InAu, I):-
	member(I, InAu),
	T = 'InputAudio'.
is_outAu(T, OutAu, I):-
	member(I, OutAu),
	T = 'OutputAudio'.
is_inC(T, InC, I):-
	member(I, InC),
	T = 'InputControl'.
is_outC(T, OutC, I):-
	member(I, OutC),
	T = 'OutputControl'.

/**
	LADSPA PROCESSOR
*/
ldpl_ladspa(Input, Key, Block, Output):-
	(Input = 'Signal'(C, Sr, L, Signals); Input = 'Frame'(C, Sr, Start, Signals)),
	ldpl_instantiate_plugin(Key, Sr, Block, Plugin),
	ldpl_connect_ports(Plugin),
	ldpl_set_default_controls(Plugin),
	ldpl_activate_plugin(Plugin).
	
	
	
	

