/**
	This is a prolog program that uses the "vamp" module for simple feature extraction as testing.
	*/

:-use_module(vamp).
:- style_check(-discontiguous).

vamp_plugins(PluginsList):-
	vamp:vmpl_plugins(PluginsList).


/**
	Available features
	*/

feature('tempo').
feature('beats').
feature('chromagram').
feature('key').

/**
	TEMPO EXTRACTION
	*/

tempo(AudioFilePath, Tempo):-
	Tool = 'vamp plugin: qm-tempotracker',
	tempo(AudioFilePath, Tempo, Tool).

tempo(AudioFilePath, Tempo, Tool):-
	Tool = 'vamp plugin: qm-tempotracker',
	vamp_run_plugin_for_audioFile(qm-vamp-plugins:qm-tempotracker, AudioFilePath, 2, Tempo).

feature_extractor_of('tempo','vamp plugin: qm-tempotracker').

/**
	Possible rules:

	compare_performance_tempo_extraction(Tool1, Tool2, Statistics):-
		tempo(File, T1, Tool1),
		tempo(File, T2, Tool2),
		compare_algorithmX(T1, T2, Statistics).   //Weka API

	compare_tempo(File1, File2, Tool, Statistics):-
		tempo(File1, T1, Tool),
		tempo(File2, T2, Tool),
		compare_algorithmX(T1, T2, Statistics).

	tempo(AudioFilePath, Tempo, Tool, Specifications).

	Instead of writing tempo variable: unification so we could extract the audio file given the feature 

	
*/


/**
	Beats
*/

beats(AudioFilePath, Beats):-
	Tool = 'vamp plugin: qm-tempotracker',
	beats(AudioFilePath, Beats, Tool).

beats(AudioFilePath, Beats, Tool):-
	Tool = 'vamp plugin: qm-tempotracker',
	vamp_run_plugin_for_audioFile(qm-vamp-plugins:qm-tempotracker, AudioFilePath, 0, Beats).

feature_extractor_of('beats','vamp plugin: qm-tempotracker').

/**
	Chromagram
*/

chromagram(AudioFilePath, Chromagram):-
	Tool = 'vamp plugin: qm-chromagram',
	chromagram(AudioFilePath, Chromagram, Tool).

chromagram(AudioFilePath, Chromagram, Tool):-
	Tool = 'vamp plugin: qm-chromagram',
	vamp_run_plugin_for_audioFile(qm-vamp-plugins:qm-chromagram, AudioFilePath, 0, Chromagram).

feature_extractor_of('chromagram','vamp plugin: qm-chromagram').

/**
	Key detector
*/

key(AudioFilePath, Key):-
	Tool = 'vamp plugin: qm-keydetector',
	key(AudioFilePath, Key, Tool).

key(AudioFilePath, Key, Tool):-
	Tool = 'vamp plugin: qm-keydetector',
	vamp_run_plugin_for_audioFile(qm-vamp-plugins:qm-keydetector, AudioFilePath, 2, Key).

feature_extractor_of('key','vamp plugin: qm-keydetector').

/**
	A calling to feature_of is non-deterministic, so we can retrieve all features we are able to extract 
*/

feature_of(Feature, AudioFilePath):-
	tempo(AudioFilePath, Feature).

feature_of(Feature, AudioFilePath):-
	beats(AudioFilePath, Feature).

feature_of(Feature, AudioFilePath):-
	chromagram(AudioFilePath, Feature).	

feature_of(Feature, AudioFilePath):-
	key(AudioFilePath, Feature).	
	
feature_extractor('vamp plugin: qm-chromagram').
feature_extractor('vamp plugin: qm-tempotracker').
feature_extractor('vamp-plugin: qm-keydetector').



	
