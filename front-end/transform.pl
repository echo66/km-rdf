/**
	General view of signal transforms (so far LADSPA effects and VAMP features over audio input)
	David Pastor Escuredo, c4dm, Queen Mary University of Lonfon
	May 2008

	Contains hardcoded rules for interpretation of a transform predicate. 
*/
:-module(transform, [api/1
		,	transform_type/1
		, 	transform/2
		,	apply_transform/3
		,	engine/1	
		]).


:-use_module('../feature-extractor/fe').
:-use_module('../swivamp/vamp').
:-use_module('../swiladspa/ladspa').
:-style_check(-discontiguous).

/*transform(TransformType, engine(API, PluginName, Version), SampleRate, StepSize, BlockSize, ListOfParameters, Program).*/


/* Hardcoded information */
transform_type('feature').
transform_type('effect').
api('vamp').
api('ladspa').
/** running time database of transforms transform_id(ID, TransformFunctor) **/
:- dynamic transform_id/2.

/**
	Available transforms (=outputs from plugins), returns the number engine for it. New APIs should be added here.
	transform(?Transform, ?Engine)
**/
transform(Transform, engine(API, PluginName, Version)):-	
	vamp_feature_system(Transform), 
	API = 'vamp',
	vamp_plugin_for(PluginName, Transform, _Index), 
	vamp_plugin_version(PluginName, Version).
transform(Transform, engine(API, PluginName, _Version)):-
	ladspa_effect_system(Transform),
	API = 'ladspa',
	ladspa_plugin_for(PluginName, Transform, _Port).


/**
	APPLY TRANSFORM

**/
apply_transform(Input, transform(Type, engine(API, PluginName, Version), Sr, Step, Block, Parameters, Configuration, Program, _Window, [QueriedOutput|Others]), Output):-
	check_input(Input, Domain),
	check_engine(Type, engine(API, _, _),
	check_plugin(PluginName, [QueriedOutput|Others]), Indexes),
	%previous make it non-deterministic
	%check_sample_rate(Sr),
	load_engine(engine(API, PluginName, Version), Plugin, Sr)
	set_engine(API, Plugin, ListOfParameters, Configuration, Program),
	engine_process(API, Plugin, Step, Block, Indexes, Outputs).
/**should run in one**/
	

/**
	Processing+framing
*/
engine_process('vamp', Plugin, Step, Block, Indexes, Output):-
	

/**
	setting up
*/
set_engine('vamp', PluginName, Plugin, Params, _, Program):-
	var(Program),
	nonvar(Params),
	vamp_set_parameters(PluginName, Plugin Params).
set_engine('vamp', _PluginName, Plugin, Params, _, Program):-
	var(Params),
	nonvar(Program),
	vamp_select_program(Plugin, Program).
set_engine('vamp', _,_Plugin, Params, _, Program):-
	var(Program), var(Params).

set_engine('ladspa',_, _Plugin, Params, _, _):-
	var(Params).
set_engine('ladspa', PluginName, Plugin, Params, _, _):-
	nonvar(Params),
	ladspa_set_parameters(PluginName, Plugin, Params).	
	
/**
	load plugin
*/
load_engine(engine('vamp', PluginName, _), Input, 'time', SampleRate, _, Plugin):-
	var(SampleRate),
	Input = signal(_, Sr, _, _),
	vmpl_load_plugin(PluginName, Sr, Plugin).
load_engine(engine('vamp', PluginName, _), Input, 'time', SampleRate, _, Plugin):-
	nonvar(SampleRate),
	Input = stream(_Data),
	vmpl_load_plugin(PluginName, SampleRate, Plugin).
load_engine(engine('vamp', _PluginName, _), _Input, 'frequency', _SampleRate, _, _Plugin).
%dont know what to do with this
	
load_engine(engine('ladspa', PluginName, _), Input, 'time', SampleRate, Block, Plugin):-
	var(SampleRate),
	Input = signal(_Ch, Sr, _, _),
	vmpl_load_plugin(PluginName, Sr, Block, Plugin).
load_engine(engine('ladspa', PluginName, _), Input, 'time', SampleRate, Block, Plugin):-
	nonvar(SampleRate),
	Input = stream(_Data),
	vmpl_load_plugin(PluginName, SampleRate, Block, Plugin).	

/** Check the input:
	So far we just accept signals. Audio signals are defined by the functor 'Signal'. Alternatively we can define signals in Freq domain
	
		signal/4: time domain audio signal. Channels, sample rate, length and data blobs
		frequencySignal/3: frequency domain signal. Which are the attributes??
**/
check_input(signal(_Ch, _Sr, _L, [_FirstBlob|_RestBlobs]), 'time').
check_input(frequencySignal(_Ch, _L, [_FirstBlob|_RestBlobs]), 'frequency').
check_input(stream(_Data), 'time').

check_engine(Type, engine(API, _PlugName, _Version)):-
	Type = 'feature', API = 'vamp'.
check_engine(Type, engine(API, _PlugName, _Version)):-
	Type = 'effect', API = 'ladspa'.

/**plugin-outputs: if there is no selected output we select all for the plugin. Also non-deterministic**/
check_plugin(engine('vamp', PlugName, _), Outputs, Index):-
	var(Outputs),
	vamp_plugin_system(PlugName),
	findall(Output, vamp_plugin_for(PlugName, Output, _), Outputs),
	member(O, Outputs),
	vamp_plugin_for(PlugName, O, Index).

/**This is a bit tricky, should i restrict to pass outputs from the same plugin or not (or just one output). Non deterministic way*/
check_plugin(engine('vamp', PlugName, _), Outputs, Index):-
	nonvar(Outputs),
	member(Output, Outputs), 
	vamp_plugin_for(PlugName, Output, Index).

/*same for ladspa*/
check_plugin(engine('ladspa', PluginName, _), Outputs, Index):-
	var(Outputs),
	ldpl_plugin_system(PluginName),
	ladspa_plugin_effects(PluginName, Outputs),
	ldpl_port_description(PluginName, 'LadspaPort'(_, Index, _)).

check_plugin(engine('ladspa', PlugName, _), Outputs, Index):-
	nonvar(Outputs),
	member(Output, Outputs),
	ladspa_plugin_for(PlugName, Output, Index).

check_sample_rate(_).
%- check_sample_rate(Sr) useful for streams

/**** DESCRIBING THE ENGINE ****/

/**Querying the possible engines**/
engine(engine(API, PluginName, Version)):-
	(vamp_plugin_system(PluginName), API ='vamp', vamp_plugin_version(PluginName, Version));
	(ldpl_plugin_system(PluginName), API='ladspa', Version = 'unknown').

/**Querying stuff (this may be enough with the specific modules and the rdf description) **/


/**** ENGINE CONFIGURATION ****/
/** api-specific routines for parameters setup**/
vamp_set_parameters(_PluginName, _Plugin, _).
vamp_set_parameters(_PluginName, Plugin, [H|T]):-
	H = parameter(Name, Value),
	vmpl_pluginParameters(Plugin, List),
	member(Name, List),
	vmpl_set_parameter(Plugin, Name, Value),
	vamp_set_parameters(Plugin, T).

ladspa_set_parameters(_PluginName, _Plugin, _).
ladspa_set_parameters(PluginName, Plugin, [H|T]):-
	H = parameter(Name, Value),
	ldpl_port_description(PluginName, 'LadspaPort'(T, I, N)),
	Name = N,
	((T = 'InputControl', ldpl_set_parameter(Plugin, I, Value));
	(T = 'OutputControl', ldpl_set_output_control(Plugin, I, Value))).
	
	
	






