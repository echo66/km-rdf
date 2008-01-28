/**
	This module defines SoundBite built on top the KM. It is aimed to perform the same functions but with two improvements:
		-multiple feature extracte based system
		-rdf talking output
	
	David Pastor 2008, c4dm
	*/


module(soundbite, [sbkm_similarity/3]).

:- style_check(-discontiguous).
use_module('home/david/km-rdf/feature-extractor/fe').


sbkm_similarity(Track1, Track2, Distance):-
	feature_of('mfccmean', Track1, Mean1),
	feature_of('mfccvar', Track1, Var1),
	feature_of('mfccmean', Track2, Mean2),
	feature_of('mfccmean', Track2, Var2),
	smpl_mfcckldiv([Mean1, Var1], [Mean2, Var2], Distance).

/*
	Si funciona flipo
	*/
	
