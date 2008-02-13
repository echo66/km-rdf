/**
example_sb.pl
It implements 2 ways of soundbite on top of the KM. It is still useless as long we don't keep the results to avoid recomputation. Should		
be sorted using a n3 version of it.
David Pastor 2008.
*/

:-[km].
:-use_module('/home/david/km-rdf/swiaudiodata/audiodata').
:-use_module('/home/david/km-rdf/swivamp/vamp').
:-use_module('/home/david/km-rdf/swiaudiosource/audiosource').
:-use_module('/home/david/km-rdf/similarity/similarity').

soundbite2(Track1, Track2, Distance):-
	decode(Track1, L), 
	decode(Track2, K), 
	vamp_feature_of(mfccmeans, L, ['Feature'(_, _, M1)]),
	vamp_feature_of(mfccmeans, K, ['Feature'(_, _, M2)]),
	vamp_feature_of(mfccvariances, L, ['Feature'(_, _, V1)]),
	vamp_feature_of(mfccvariances, K, ['Feature'(_, _, V2)]),
	mfcc_kldiv(M1, V1, M2, V2, Distance).

soundbite(Track1, Track2, Distance):-
	decode(Track1, L),
	decode(Track2, K),
	vamp_feature_of(coefficients, L, Mfcc1),
	vamp_feature_of(coefficients, K, Mfcc2),
	mfcc_gaussian_parameters(Mfcc1, [M1, V1]),
	mfcc_gaussian_parameters(Mfcc2, [M2, V2]),
	mfcc_kldiv(M1, V1, M2, V2, Distance).	

run(Distance):-
	decode('/home/david/Repository/OggTest.ogg', X), mix_stereo(X, L),
	decode('/home/david/Repository/Mp3Test.mp3', Y), mix_stereo(Y, K),
	feature_of(means, L, ['Feature'(_, _, M1)]),
	data_out(M1, '/home/david/data/m1'), clean_data(M1),
	feature_of(means, K, ['Feature'(_, _, M2)]),
	data_out(M2, '/home/david/data/m2'), clean_data(M2),
	feature_of(variances, L, ['Feature'(_, _, V1)]),
	data_out(V1, '/home/david/data/v1'), clean_data(V1),
	feature_of(variances, K, ['Feature'(_, _, V2)]),
	data_in('/home/david/data/m1', M1),
	data_in('/home/david/data/m2', M2),
	data_in('/home/david/data/v1', V1),
	mfcc_kldiv(M1, V1, M2, V2, Distance).

