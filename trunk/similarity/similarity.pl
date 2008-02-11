/**
	KM module to define similarity among features for audio similariy and comparison
	David Pastor Escuredo 2008, c4dm, Queen Mary.
	*/

:-module(similarity, [		smpl_mfcckldiv/5
			]).

:- style_check(-discontiguous).
:- load_foreign_library(similarity).

/**
	smpl_mfcckldiv(+MeansEvent1, +VariancesEvent1, +MeansEvent2, +VariancesEvent2, -Distance):
	
	SoundBite timbral similarity. This is the classical SB similarity approach based on a statistical model of the MFCC (means and variances) and the 
	KL divergence as metric.
*/

