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
%	parameters (List)
%	block
%	outcontrols (List)

transform(Signal, Lib, Id, Outputs, Key, Options):-
	is_list(Options),
	Signal = signal(Sr, Data),
	get_channels(Signal, Ch),
	plugin_key(Lib, Id, Key),
	option(block(Block), Options, 2048),
        ldpl_instantiate_plugin(Key, Sr, Block, Plugin),
	ldpl_connect_ports(Plugin),
	((option(parameters(Parameters), Options, _), option(outcontrols(OutCtrl), Options, _), !) ; (ldpl_set_default_controls(Plugin))),
	(ldpl_activate_plugin(Plugin) ; true),
	ldpl_run_plugin_framing(Data, Plugin, 0, Block).
	

% concats the plugin key

plugin_key(_Lib, Id, Key):-
	nonvar(Id),!,
	atom_concat('ladspa-plugin', :, HalfKey),
	atom_concat(HalfKey, :, HalfKey2),
	atom_concat(HalfKey2, Id, Key).
plugin_key(Lib, Id, _):-
	var(Lib), var(Id).

% routine for processing frames

ldpl_process_signal(Signal, Plugin, Block, ProcessedSignal):-
	Signal = signal(_Sr, Data),
	get_samples_per_channel(Signal, L),
	findall(_,ldpl_process_frames(Data, Plugin, L, Block), _).
	
ldpl_process_frames(Data, Plugin, L, Block):-
	set_framing(Block, L, _, Start),	
	ldpl_run_plugin_framing(Data, Plugin, Start, Block).

%another routine relying on swiaudio for framing

ldpl_process_signal2(Signal, Plugin, Block, ProcessedSignal):-
	get_samples_per_channel(Signal, L),
	findall(_,ldpl_process(Signal, Plugin, L, Block), _).
	
ldpl_process(Signal, Plugin, L, Block):-
	set_framing(Block, L, _, Start),
	get_frame(Signal, Start, Block, Frame),	
	ldpl_run_plugin_framing(Frame, Plugin, Block).

