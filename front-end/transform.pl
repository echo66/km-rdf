/**
	General view of signal transforms (so far LADSPA effects and VAMP features over audio input)
	David Pastor Escuredo, c4dm, Queen Mary University of Lonfon
	May 2008

	Contains hardcoded rules for interpretation of a transform predicate. These rules allow the user to call the transform in *many* ways without crashing pointing to a declarative meaning of such a predicate. 


SPECIFICATION:

	:-apply_transform(Signal, Transform, Output).

where T = transform(Type, Engine, SampleRate, StepSize, BlockSize, Parameters, Configuration, Program, WindowType, QueriedOutputsList)
*/
:-module(transform, [api/1
		,	transform_type/1
		, 	transform/2
		,	apply_transform/3
		,	apply_store_transform/4
		,	my_transform/2
		,	engine/1	
		]).


:-use_module('../feature-extractor/fe').
:-use_module('../swivamp/vamp').
:-use_module('../swiladspa/ladspa').
:-use_module('../swiaudiosource/audiosource').
:-style_check(-discontiguous).


/*************************************************************
	  QUERYING PREVIOUS INFORMATION
*************************************************************/


/** Hardcoded information **/
transform_type('feature').
transform_type('effect').
api('vamp').
api('ladspa').

/** running time database of transforms transform_id(ID, TransformFunctor) **/
:- dynamic transform_id/2.
:- dynamic computation_id/4.

/**
	AVAILABLE TRANSFORMS (=outputs from plugins), returns the engine for it. New APIs should be added here.
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

/**Querying the possible engines**/
engine(engine(API, PluginName, Version)):-
	(vamp_plugin_system(PluginName), API ='vamp', vamp_plugin_version(PluginName, Version));
	(ldpl_plugin_system(PluginName), API='ladspa', Version = 'unknown').

/***********************************************************
	               APPLY TRANSFORM
***********************************************************/


/** In session tabling **/
apply_store_transform(ID, Input, Transform, Output):-
	apply_transform(Input, Transform, Output),
	assert(ID, Input, Transform, Output).

my_transform(ID, Transform):-
	transform_id(ID, Transform); assert(transform_id(ID, Transform)).

/** Predicate that launches the transform computation. The functor transform must be set up (allows great flexibility!) **/
apply_transform(Input, 
transform(Type, engine(API, PluginName, Version), Sr, Step, Block, Parameters, Configuration, Program, _Window, [QueriedOutput|Others]), 
Outputs):-
	check_input(Input, Domain),
	check_engine(Type, engine(API, _, _)),
	check_plugin(engine(API, PluginName,_), [QueriedOutput|Others], ListOfIndexes),
	%check_sample_rate(Sr),
	load_engine(engine(API, PluginName, Version), Input, Domain, Sr, Block, Plugin),
	set_engine(API, PluginName, Plugin, Parameters, Configuration, Program),
	restrict_output(Type, Outputs),
	engine_process(API, Input, Plugin, Step, Block, ListOfIndexes, Outputs).
	

/************************************************************************************************
	INTERPRETATION RULES AND CONSTRAINTS

	They allow calling the transform in a declarative way and avoiding possible missuses.
*************************************************************************************************/


/** Constraining output syntax **/
restrict_output('feature', Output):-
	Output = [['Feature'(_Type, _Timestamp, _Data)|_SameType]|_MoreOutputsOfThePlugin].
restrict_output('effect', Output):-
	Output = signal(_Ch, _Sr , _L, _Data); Output=frequencySignal(_Cha, _Length, _Data2).

/**
	VAMP Processing+framing. (using feature_extractor module)
*/
engine_process('vamp', Input, Plugin, Step, Block, Indexes, Outputs):-
	blockSize(Plugin, Block, BlockSize),
	stepSize(Plugin, Step, StepSize),
	input_channels(Input, Channels),
	vmpl_initialize_plugin(Plugin, Channels, StepSize, BlockSize),
	Input = signal(Ch, Sr, L, Data),
	Signal = 'Signal'(Ch, Sr, L, Data),
	vamp_compute_feature2(Signal, StepSize, BlockSize, Indexes, Plugin, Outputs).
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
input_channels(Input, Channels):-
	Input = signal(Channels, _, _, _); Input = frequencySignal(Channels, _, _).

/**
	LADSPA PROCESSING (I need to implement effects first
**/

/** setting up **/
set_engine('vamp', PluginName, Plugin, Params, _, Program):-
	var(Program),
	nonvar(Params),
	vamp_set_parameters(PluginName, Plugin, Params).
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

/** load plugin **/
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

/** 
	Check the input:
	check_input(+Input, -Domain). 
	So far we just accept signals. Audio signals are defined by the functor 'Signal'. Alternatively we can define signals in Freq domain
	
		signal/4: time domain audio signal. Channels, sample rate, length and data blobs
		frequencySignal/3: frequency domain signal. Which are the attributes??
**/
check_input(signal(_Ch, _Sr, _L, [_FirstBlob|_RestBlobs]), 'time').
check_input(frequencySignal(_Ch, _L, [_FirstBlob|_RestBlobs]), 'frequency').
check_input(stream(_Data), 'time').

/** checking engine against the type of transform **/
check_engine(Type, engine(API, _PlugName, _Version)):-
	Type = 'feature', API = 'vamp'.
check_engine(Type, engine(API, _PlugName, _Version)):-
	Type = 'effect', API = 'ladspa'.

/**

	CHECKING PLUGIN: check_plugin(engine(?API, ?PluginName, _Version), ?Outputs, -Indexes)

	This predicate scans the instantation patter of transform and reasons about it matching the desired outputs with the plugin names and providing indexes. Note if u pass a set of outputs, the system will run a plugin for each output even when several outputs can be calculated with the same plugin.
This provides huge flexibility to call the predicate transform!!!
**/

/** it works well for vamp thanks to vamp_plugin_for/3 **/
check_plugin(engine(API, PlugName, _), Outputs, Indexes):-
	API = 'vamp',
	var(Outputs),
	var(Indexes),
	nonvar(PlugName),
	findall(I, vamp_plugin_for(PlugName, _, I), Indexes),
	findall(O, (member(In, Indexes), vamp_plugin_for(PlugName, O, In)), Outputs).
check_plugin(engine(API, PluginName, _), Outputs, [Index]):-
	API = 'vamp',
	nonvar(Outputs),
	member(Output, Outputs),
	vamp_plugin_for(PluginName, Output, Index).

/** same for ladspa (FIXME) **/
check_plugin(engine('ladspa', PluginName, _), Outputs, Indexes):-
	var(Outputs),
	ldpl_plugin_system(PluginName),
	ladspa_plugin_effects(PluginName, Outputs),
	findall(Index, ldpl_port_description(PluginName, 'LadspaPort'(_, Index, _)), Indexes).
check_plugin(engine('ladspa', PlugName, _), Outputs, [Index]):-
	nonvar(Outputs),
	member(Output, Outputs),
	ladspa_plugin_for(PlugName, Output, Index).

/** sample rate is pending **/
check_sample_rate(_).
%- check_sample_rate(Sr) useful for streams


/****************************************** END ***********************************/

	
	






