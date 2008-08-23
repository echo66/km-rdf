/**
	LADSPA Module for audio analysis. This module offers communication with the plugin libraries.	

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
*/

:-module(ladspa, [ladspa_plugin_system/1
		,	ldpl_plugins/1
		,	ldpl_plugin_maker/2
		,	ldpl_plugin_library/2
		,	ldpl_plugin_ports_count/2
		,	ldpl_input_audio/2
		,	ldpl_output_audio/2
		,	ladspa_port_description/2
		, 	ladspa_plugin_for/3
		,	ldpl_input_control/2
		,	ldpl_output_control/2
		, 	ldpl_port_name_for/3
		,	ldpl_set_parameter/3
		,	ldpl_set_output_control/3
		,	ldpl_instantiate_plugin/4
		,	ldpl_connect_ports/1
		,	ldpl_set_default_controls/1
		,	ldpl_activate_plugin/1
		,	ldpl_run_plugin/3
		,	ldpl_run_plugin_framing/4
		,	ldpl_deactivate_plugin/1
		, 	ldpl_cleanup_plugin/1
		]).

:- style_check(-discontiguous).
:- load_foreign_library(swiladspa).

:- use_module('../swidata/data').
:- use_module('../swiaudio/audio').
%:- use_module(library(pldoc)).

:-ldpl_loader.

/************************
****** PATTERNS *********
************************/

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

%%  ladspa_plugin_system(?PluginKey) is nondet
% Checks the LADSPA plugins in the system

ladspa_plugin_system(Plugin):-
	ldpl_plugins(List),
	member(Plugin, List).

%%  ladspa_port_description(?PluginKey, 'LadpsaPort'(?Type, ?Index, -Name)) is nondet
% Returns a functor 'LadspaPort'(Type, Index, Name) describing the differt ports of the plugin (audio and control input and output controls)

ladspa_port_description(Name, 'LadspaPort'(T, I, N)):-
	var(N),!,
	ladspa_plugin_system(Name),
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
ladspa_port_description(Name, 'LasdpaPort'(T, I, N)):-
	nonvar(N),!,
	ladspa_port_description(Name, 'LasdpaPort'(T, I, N2)),
	N=N2.
	

%% ladspa_transform_system(?Transform) is nondet
% Checks available transforms in your system	

ladspa_transform_system(Effect):-
	ldpl_plugin_system(PluginName),
	findall(E, ladspa_port_description(PluginName, 'LadspaPort'('OutputAudio', _, E)), Effects),
	member(Effect, Effects).

%% lasdpa_plugin_for(?PluginKey, ?Transform, ?Index) is nondet
% Relationships amongst plugins and transforms.

ladspa_plugin_for(PluginKey, Effect, Index):-
	var(Effect),var(Index),!,
	ladspa_port_description(PluginKey, 'LadspaPort'('OutputAudio', Index, Effect)).

ladspa_plugin_for(PluginKey, Effect, Index):-
	nonvar(Effect),var(PluginKey),var(Index),!,
	findall(FT, ladspa_plugin_for(PluginKey, FT, _), Outputs),
	member(Effect, Outputs),	
	ladspa_port_description(PluginKey, 'LadspaPort'('OutputAudio', Index, Effect)).

	
	
	
	
	

