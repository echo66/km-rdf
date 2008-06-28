/**
	Feature Extraction transform for Henry using the SWI-DSP (vamp plugins).
	ToDo: Adapt it as built in and constrain the output format
		Merge this and front-end transform (i'm just taking bits from different parts)
*/

:-use_module('../swiaudiosource/audiosource').
:-use_module('../swiaudiodata/audiodata').
:-use_module('vamp').
:-use_module('../feature-extractor/fe').
%:-use_module('../front-end/transform').
:-use_module(library('option')).


/**
	Options List 

		Block for framing
		Hop for framing
		** Not included: SampleRate if variable (well, audiosource does not support this so far)
		Library
		PluginName
		Outputs (given names)
		Parameters	
		Program	

		eg
		vamp_transform(Input, [block = 2048, hop = 1024, library = 'qmul-vamp-plugins', pluginid = 'qm-tempotracker', parameters = [parameter(name, value)|...], outputs = [Output1, Output3]], Result).
*/
		
%rdf_vamp(Input, RDFOptions, ExportedOutputs):-
%	Input = [literal(Ch),literal(Sr),literal(L),Sigs],	
%	Signal = 'Signal'(Ch, Sr, L, Sigs),	
	%adapt options
%	vamp_transform(Signal, Options, FinalOutputs),
	%adapt_output(FinalOutputs, ExportedOutputs).

vamp_transform(Signal, Options, FinalOutputs):-
	is_list(Options),
	Signal = 'Signal'(Ch, Sr, _, _),	
	option(block(Block), Options, _),
	option(step(Step), Options, _),
	option(library(Lib), Options, _),
	option(pluginid(Id), Options, _),
	option(parameters(Params), Options, []),
	option(program(Prog), Options, _),
	option(outputs(Outputs), Options, _),
	plugin_key(Lib, Id, Key),
	plugin_output_setting(Key, Outputs, Indexes),
	vmpl_load_plugin(Key, Sr, Plugin),
	set_parameters(Plugin, Params),
	set_program(Plugin, Prog),
	blockSize(Plugin, Block, BlockSize),
	stepSize(Plugin, Step, StepSize),
	vmpl_initialize_plugin(Plugin, Ch, StepSize, BlockSize),
	vamp_compute_feature2(Signal, StepSize, BlockSize, Indexes, Plugin, FinalOutputs).

%Different configurations for plugin-output setting
plugin_output_setting(Key, Outputs, Indexes):-
	nonvar(Key),
	var(Outputs),
	vamp_plugin_system(Key),
	Indexes = [0].

%If we pass plugin and outputs we need to check validity
plugin_output_setting(Key, Outputs, Indexes):-
	nonvar(Key),
	nonvar(Outputs),
	is_list(Outputs),
	length(Outputs, L),
	L>0,
	plugin_outputs_matching(Key, Outputs, Indexes).

%We dont accept variable plugin and outputs
plugin_output_setting(Key, Outputs, _Indexes):-
	var(Key),
	var(Outputs),
	fail.

%Outputs and not plugin. I may need to cut backtracking here
plugin_output_setting(Key, Outputs, Indexes):-
	var(Key),
	nonvar(Outputs),
	is_list(Outputs),
	length(Outputs, L),
	L>0,
	plugin_outputs_matching(Key, Outputs, Indexes).
	
%matching outputs passed (as a list) and keys
plugin_outputs_matching(Key, Outputs, Indexes):-	
	nonvar(Key),
	findall(Index, plugin_output_matching(Key, Outputs, Index), Indexes).	

%worst case: outputs from different plugins? one at a time. We process one plugin for output even when used more than once
plugin_outputs_matching(Key, Outputs, Indexes):-
	var(Key),
	member(Output, Outputs),
	vamp_plugin_for(Key, Output, Index),
	Indexes = [Index].
	
%Will fail if one output does not come from the plugin given
plugin_output_matching(Key, Outputs, Index):-
	nonvar(Key), nonvar(Outputs),
	member(Output, Outputs),
	vamp_plugin_for(Key, Output, Index).
	
%concats the plugin key
plugin_key(Lib, Id, Key):-
	nonvar(Lib),
	nonvar(Id),
	atom_concat(Lib, :, HalfKey),
	atom_concat(HalfKey, Id, Key).
plugin_key(Lib, Id, _):-
	var(Lib), var(Id).

/** api-specific routines for parameters setup**/
set_parameters(_, _).
set_parameters(Plugin, [H|T]):-
	H = parameter(Name, Value),
	vmpl_pluginParameters(Plugin, List),
	member(Name, List),
	vmpl_set_parameter(Plugin, Name, Value),
	vamp_set_parameters(Plugin, T).	

set_program(Plugin, Program):-
	nonvar(Program),
	vmpl_select_program(Plugin, Program).
set_program(_, P):-
	var(P).

blockSize(P, B, Bs):-
	vmpl_get_blockSize(P, PB),
	var(B), nonvar(PB),
	Bs = PB.
blockSize(_P, B, Bs):-
	nonvar(B),
	Bs=B.
blockSize(P, B, Bs):-
	vmpl_get_blockSize(P, PB),
	var(B), var(PB), Bs is 2048.
stepSize(P, S, Ss):-
	vmpl_get_stepSize(P, PS),
	var(S), nonvar(PS),
	Ss = PS.
stepSize(_P, S, Ss):-
	nonvar(S),
	Ss=S.
stepSize(P, S, Ss):-
	vmpl_get_stepSize(P, PS),
	var(S), var(PS), Ss is 1024.


