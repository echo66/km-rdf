/**
	This module defines SoundBite built on top the KM. It is aimed to perform the same functions but with two improvements:
		-multiple feature extracte based system
		-rdf talking output
	
	Actually there are 2 ways of calculating the similarity between 2 audio files. We can use directly the similarity plugin retrieving one of the
	distance outputs or we can just get the lower level features (mean and variance) and use the similarity module.
	It may result that the first option is faster but in the long run is expected the second to be better as we don't need to compute the features
	each time and we gain in flexibility and logical inference capabilities.
	David Pastor 2008, c4dm, Queen Mary.
	*/


module(soundbite, [sbkm_similarity/3]).

:- style_check(-discontiguous).
use_module('home/david/km-rdf/feature-extractor/fe').
use_module('home/david/km-rdf/swiaudiodata/audiodata').
use_module('home/david/km-rdf/swiaudiosoruce/audiosource').
use_module('home/david/km-rdf/swivamp/vamp').

sbkm_similarity(Track1, Track2, Distance):-
	aspl_decode(Track1, Stereo1),
	aspl_decode(Track2, Stereo2),
	mix_stereo(Stereo1, Mono1),
	mix_stereo(Stereo2, Mono2),

	/**be careful here**/
	vamp_output('mfccmean', Mono1, Mean1),
	feature_of('mfccvar', Track1, Var1),
	feature_of('mfccmean', Track2, Mean2),
	feature_of('mfccmean', Track2, Var2),
	smpl_mfcckldiv([Mean1, Var1], [Mean2, Var2], Distance).

/*
	Si funciona flipo
	*/
	
