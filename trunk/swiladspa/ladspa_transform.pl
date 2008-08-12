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
:-use_module('vamp').
%:-use_module('../front-end/transform').
:-use_module(library('option')).

%% transform(+Signal, +LibraryId, +PluginId, +ListOfOutputIds, -FinalOutput) is semidet
% Call to the LADSPA API passing arguments without options and returning a complex list as Final Output

transform(Signal,Lib,Id,Outputs,FinalOutputs):-
	transform(Signal,Lib,Id,Outputs,FinalOutputs,[]).

%% transform(+Signal, +LibraryId, +PluginId, +ListOfOutputIds, -FinalOutput, +Options) is semidet

transform(Signal, Lib, Id, Outputs, FinalOutputs, Options):-
	is_list(Options),
	Signal = signal(Sr, _Data),
	get_channels(Signal, Ch),
	plugin_key(Lib, Id, Key),
	option(block(Block), Options, 2048),
        ldpl_instantiate_plugin(Key, Sr, Block, Plugin)
	vmpl_initialize_plugin(Plugin, Ch, StepSize, BlockSize),
	vamp_compute_feature2(Signal, StepSize, BlockSize, Indexes, Plugin, FinalOutputs).

% concats the plugin key

plugin_key(Lib, Id, Key):-
	nonvar(Lib),
	nonvar(Id),
	atom_concat('ladspa-plugin', :, HalfKey),
	atom_concat(HalfKey, Id, Key).
plugin_key(Lib, Id, _):-
	var(Lib), var(Id).


