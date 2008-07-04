/**
	KM module to define similarity among features for audio similariy and comparison
	David Pastor Escuredo 2008, c4dm, Queen Mary.
	*/

:-module(similarity, [		smpl_mfcc_kldiv/5
			,	smpl_mfcc_gaussian_parameters/2 /**differs from similarity plugin mostly in C0**/
			, 	smpl_chroma_kldiv/3 /**no tested**/
			,	smpl_cosine_distance/3
			]).

:- style_check(-discontiguous).
:- load_foreign_library(similarity).

%%	smpl_mfcckldiv(+MeansEvent1, +VariancesEvent1, +MeansEvent2, +VariancesEvent2, -Distance) is det
% SoundBite timbral similarity. This is the classical SB similarity approach based on a statistical model of the MFCC (means and variances) and the KL divergence as metric. The input are BLOBIDs and the output a literal

%% smpl_cosine_distance(+BeatSpectra1, +BeatSpectra2, -Distance)
% Cosine distsance of the beat spectra. Input are BLOBIDs and output a literal
