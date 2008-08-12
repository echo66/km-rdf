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

:- use_module('../swidata/data').
:- use_module('../swiaudio/audio').

:-ldpl_loader.

/************************
****** PATTERNS *********
************************/

/* 	ldpl_plugins(-ListOfPluginsInSystem)

	ldpl_plugin_maker(+PluginName, -PluginMaker)

	ldpl_plugin_library(+PluginName, -LibraryOwningThePlugin)

	ldpl_plugin_ports_count(+PluginName, -NumberOfPorts)

	ldpl_input_audio(-PluginName, -ListOfIndexesOfInputAudioPorts) Eg: ldpl_input_audio('AmplifierStereo', [0, 1])

	ldpl_output_audio(-PluginName, -ListOfIndexesOfOutputAudioPorts)

**/

/**************************** USING PLUGIN ***********************/

/**
	We present here a semi-formal set of rules to use ladspa plugin from this interface:

	1. Create an instance of the plugin. We give the Block size from framing at this point, but it can be changed afterwards.

		ldpl_instantiate_plugin(+PluginName, +SampleRate, +BlockSize, -Plugin)

	2. Connect ports and set default values. We link each port to its buffer and set each control to 0

		ldpl_connect_ports(+Plugin)
		ldpl_set_default_controls(+Plugin)

	3. Activate (if any)

		ldpl_activate_plugin(+Plugin)
	
	4. Set controls

		ldpl_set_parameter(+Plugin, +InControlPort, +Value)
		ldpl_set_output_control(+Plugin, +OutControlPort, Value)

	5. Set buffers

	6. Run
		ldpl_run_plugin(+ListOfData, +Plugin, +BlockSize)
		ListOfData = ['__data_n', '__data_m', ...] where each id is a PCM block of the corresponding size

	7. Deactivate/Activate(optional)

	8. Re-fill buffers

	9. Run

	...

	n. Get Output
*/

/****************************** Building specific rules ************************/

/**
	Non-deterministic scan of plugins in system
	ldpl_plugin_system(?Plugin)

	IS FAILING AGAIN!!!!
	*/
ldpl_plugin_system(Plugin):-
	ldpl_plugins(List),
	member(Plugin, List).

/**
	Returns a functor 'LadspaPort'(Type, Index, Name). Non-deterministic

	ldpl_port_description(?PluginName, 'LadpsaPort'(?Type, ?Index, -Name))
*/
ldpl_port_description(Name, 'LadspaPort'(T, I, N)):-
	var(N),
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
ldpl_port_descriptor(Name, 'LasdpaPort'(T, I, N)):-
	nonvar(N),
	ldpl_port_descriptor(Name, 'LasdpaPort'(T, I, N2)),
	N=N2.
	

/**
	LADSPSA EFFECTS
*/
ladspa_plugin_effects(PluginName, Effects):-
	findall(Effect,	ldpl_port_description(PluginName, 'LadspaPort'(_, _, Effect)), Effects).

ladspa_effect_system(Effect):-
	ldpl_plugin_system(PluginName),
	ladspa_plugin_effects(PluginName, Effects),
	member(Effect, Effects).

ladspa_plugin_for(PluginKey, Effect, Index):-
	var(Effect),
	var(Index),
	vamp_plugin_system(PluginKey),
	vmpl_port_descriptor(PluginKey, 'LadspaPort'(_, Index, Effect)).
ladspa_plugin_for(PluginKey, Effect, Index):-
	nonvar(Effect),
	var(PluginKey),
	findall(FT, lasdpa_plugin_for(PluginKey, FT, _), Outputs),
	member(Effect, Outputs),	
	vmpl_port_descriptor(PluginKey, 'LadspaPort'(_, Index, Effect)).
	
	
/**
	LADSPA PROCESSOR
*/
ldpl_ladspa(Input, Key, Block, Output):-
	Input = 'Signal'(C, Sr, L, Signals),
	ldpl_instantiate_plugin(Key, Sr, Block, Plugin),
	ldpl_connect_ports(Plugin),
	ldpl_set_default_controls(Plugin),
	ldpl_set_parameter(Plugin, 0, 2),
	ldpl_activate_plugin(Plugin),
	End is Block-1,
	get_frame(Input, 4000, Block, 'Frame'(_, _, _, Data)),
	ldpl_run_plugin(Data, Plugin, Block),
	ldpl_return_output(Plugin, Output),
	ldpl_return_output(Plugin, Output).
	
	
	
	
	

