/**
	KM module to define similarity among features for audio similariy and comparison
	David Pastor Escuredo 2008, c4dm, Queen Mary.
	*/

:-module(similarity, [smpl_mfcckldiv/3]).

:- style_check(-discontiguous).
:- load_foreign_library(similarity).

/**
	smpl_mfcckldiv(+Input1, +Input2, -Distance): This predicate returns a SBSimilarity distance using as input the mean and the variance of the mfcc.
	This is the sort of metric used in SoundBite.
	The Input should look like:
		[['Feature'(mfccmean, timestamp, __data_id), ...], ['Feature'(mfccvar, timestamp, __data_id), ...]]
	*/
