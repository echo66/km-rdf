/**
	General view of signal transforms
	David Pastor Escuredo, c4dm
	May 2008

	Contains hardcoded rules for interpretation of a transform predicate
*/
:-module(transform, [api/1
		,	plugin_engine/1
			]).


:-use_module(../feature-extractor/fe).
:-use_module(../swivamp/vamp).
:-use_module(../swiladspa/ladspa).
//load effects
:-style_check(-discontiguous).

/*'Transform'(TransformType, 'Engine'(API, PluginName, Version), SampleRate, StepSize, BlockSize, ListOfParameters, Program).*/


/* Hardcoded information */
transform_type('feature_extraction').
transform_type('effect').
api('vamp').
api('ladspa').

/**Querying the possible engines**/
plugin_engine('Engine'(API, PluginName, _Version)):-
	(vamp_plugin_system(PluginName), API ='vamp', vamp_plugin_version(PluginName, Version));
	(lpdl_plugin_system(PluginName), API='ladspa').

/**read_parameters**/
set_parameters(Engine, Plugin, ListOfParameters):-
	Engine = 'Engine'(A, P, V),
	(A = 'vamp', vamp_set_parameters(Plugin, ListOfParameters));
	(A = 'ladspa', ladspa_ser_parameters(Plugin, ListOfParameters)).

/** api-specific routines for parameters setup**/
vamp_set_parameters(PluginName, Plugin, _).
vamp_set_parameters(PluginName, Plugin, [H|T]):-
	H = 'Parameter'(Name, Value),
	vmpl_pluginParameters(Plugin, List),
	member(Name, List),
	vmpl_set_parameter(Plugin, Name, Value),
	vamp_set_parameters(Plugin, T).

ladspa_set_parameters(PluginName, Plugin, _).
ladspa_set_parameters(PluginName, Plugin, [H|T]).
	H = 'Parameter'(Name, Value),
	ldpl_port_description(PluginName, 'LadspaPort'(T, I, N)),
	Name = N,
	((T = 'InputControl', ldpl_set_parameter(Plugin, I, Value));
	(T = 'OutputControl', ldpl_set_output_control(Plugin, I, Value))).
	
/** program selection for vamp */
set_program(API, Plugin, Program):-
	API = 'vamp', vmpl_select_program(Plugin, Program).	
	
	






