/**
	LADSPA Transform Module on top of the LADSPA module. This module defines procedures to make LADSPA transforms

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
	*/

:-module(ladspa_transform, [
				transform/5
			,	transform/6

	]).

:-use_module('../swiaudio/audio').
:-use_module('../swidata/data').
:-use_module('ladspa').
%:-use_module('../front-end/transform').
:-use_module(library('option')).

%% transform(+Signal, +LibraryId, +PluginId, +ListOfOutputIds, -FinalOutput) is semidet
% Call to the LADSPA API passing arguments without options and returning a complex list as Final Output

transform(Signal,Lib,Id,Outputs,FinalOutputs):-
	transform(Signal,Lib,Id,Outputs,FinalOutputs,[]).

%% transform(+Signal, +LibraryId, +PluginId, +ListOfOutputIds, -FinalOutput, +Options) is semidet
%
% Options:
%	parameters (List) where parameter(Name, Value)
%	block
%	outControls (List) where outControl(Name, Value)

transform(Signal, Lib, Id, _TrickyOutputs, Plugin, Options):-
	is_list(Options),
	Signal = signal(Sr, _D),
	plugin_key(Lib, Id, Key),
	option(block(Block), Options, 1024),
        ldpl_instantiate_plugin(Key, Sr, Block, Plugin),
	ldpl_connect_ports(Plugin),
	adapt_input(Signal, Key, S2),
	S2 = signal(Sr, Data),	
	((ldpl_activate_plugin(Plugin),ldpl_connect_ports(Plugin),!) ; true),
	%This default set up may not be enough!!!
	ldpl_set_default_controls(Plugin),
	((option(outControls(OutCtrls), Options, _),ladspa_set_output_controls(Plugin, OutCtrls),!) ; true),
	((option(parameters(Params), Options, _),ladspa_set_parameters(Plugin, Params),!) ; true),
	ldpl_connect_ports(Plugin),
	ldpl_run_plugin_framing(Data, Plugin, 10000, Block).
	%		ldpl_collect_output(Plugin, Block).

/** works for one block but fails from there on...*/
/**
	ldpl_processed_signal(Sr, ProcessedSignal).*/
	
%plugin setting if passed

ladspa_set_parameters(Plugin, Params):-
	is_list(Params),
	member(parameter(Name, Value), Params),
	ldpl_get_key(Plugin, Key),
	ladspa_port_description(Key, 'LadspaPort'(_, Index, Name)),
	ldpl_set_parameter(Plugin, Index, Value).

ladspa_set_output_controls(Plugin, OutCtrls):-
	is_list(OutCtrls),
	member(outControls(Name, Value), OutCtrls),
	ldpl_get_key(Plugin, Key),
	ladspa_port_description(Key, 'LadspaPort'(_, Index, Name)),
	ldpl_set_output_control(Plugin, Index, Value).	

% count input ports

ladspa_input_channels(Key, Count):-
	ldpl_input_audio(Key, L),
	length(L, Count).

adapt_input(Signal, Key, Signal2):-
	get_channels(Signal, Ch),
	ladspa_input_channels(Key, Input),
	Input = Ch, !,
	Signal2 = Signal.
adapt_input(S, K, S2):-
	get_channels(S, Ch),
	ladspa_input_channels(K, L),
	Ch=2, L=1,!,
	mix_stereo(S, S2).

% concats the plugin key

plugin_key(_Lib, Id, Key):-
	nonvar(Id),!,
	atom_concat('ladspa-plugin', :, HalfKey),
	atom_concat(HalfKey, :, HalfKey2),
	atom_concat(HalfKey2, Id, Key),
	ladspa_plugin_system(Key).
plugin_key(Lib, Id, _):-
	var(Lib), var(Id).

% routine for processing frames

ldpl_process_signal(Signal, Plugin, Block):-
	Signal = signal(_Sr, Data),
	get_samples_per_channel(Signal, L),
	findall(_,ldpl_process_frames(Data, Plugin, L, Block), _).
	
ldpl_process_frames(Data, Plugin, L, Block):-
	set_framing(Block, L, _, Start),	
	ldpl_run_plugin_framing(Data, Plugin, Start, Block),
	ldpl_collect_output(Plugin).

%another routine relying on swiaudio for framing

ldpl_process_signal2(Signal, Plugin, Block):-
	get_samples_per_channel(Signal, L),
	findall(_,ldpl_process(Signal, Plugin, L, Block), _).
	
ldpl_process(Signal, Plugin, L, Block):-
	set_framing(Block, L, _, Start),
	get_frame(Signal, Start, Block, Frame),	
	ldpl_run_plugin_framing(Frame, Plugin, Block),
	ldpl_collect_output(Plugin).

