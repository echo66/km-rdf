/**
	KM module to define similarity among features for audio similariy and comparison
	David Pastor Escuredo 2008, c4dm, Queen Mary.
	*/

:-module(similarity, [		mfcc_kldiv/5
			,	mfcc_gaussian_parameters/2 /**differs from similarity plugin mostly in C0**/
			, 	chroma_kldiv/3 /**no tested**/
			]).

:- style_check(-discontiguous).
:- load_foreign_library(similarity).

/**
	smpl_mfcckldiv(+MeansEvent1, +VariancesEvent1, +MeansEvent2, +VariancesEvent2, -Distance):
	
	SoundBite timbral similarity. This is the classical SB similarity approach based on a statistical model of the MFCC (means and variances) and the 
	KL divergence as metric.
*/

