/** SOUNDBITE-KM

	Demo Version 1.0
	This sb-km is based on swi-dsp using the similarity vamp plugin (Chris Cannam, Mark Levy, Kur Jacobosn, Chris Sutton)
	It only keeps playlists as prolog lists within a session
	David Pastor Escuredo March 2008, c4dm
*/

:-use_module('/home/david/km-rdf/swiaudiodata/audiodata').
:-use_module('/home/david/km-rdf/swivamp/vamp').
:-use_module('/home/david/km-rdf/swiaudiosource/audiosource').

:-use_module('/home/david/km-rdf/similarity/similarity').
:-use_module('/home/david/km-rdf/feature-extractor/fe').

:-dynamic test/1.

test('/home/david/Repository/Test1.ogg').
test('/home/david/Repository/Test2.mp3').
test('/home/david/Repository/Test3.wav').
test('/home/david/Repository/Test4.ogg').
test('/home/david/Repository/Test5.mp3').


/** Feature extraction and playlist creation for a single track Timbre mode **/
soundbite_timbre(Audio1, List):-
	findall(_,similar_timbre(Audio1),_), get_playlist(Audio1,t, List).

sb_mode(t, 'Timbre').
sb_mode(tr, 'Timbre and Rhythm').
sb_mode(r, 'Rhythm').

/**Calculates the distance of Audio1 to the whole Database including itself **/
/**You can add another file to the database giving an A1 out of it**/
sb(t, A1, A2, D):-
	sb_t(A1, A2, D).

sb(tr, A1, A2, D):-
	sb_tr(A1, A2, _, _, D).

sb(r, A1, A2, D):-
	sb_r(A1, A2, D).

sb_t(Audio1, Audio2, Distance):-
	nonvar(Audio1),
	test(Audio2),
	computation(Audio1, M1, V1, _B1),
	computation(Audio2, M2, V2, _B2),
	smpl_mfcc_kldiv(M1, V1, M2, V2, Distance).

sb_tr(Audio1, Audio2, TD, RD, Distance):-
	nonvar(Audio1),
	test(Audio2),
	computation(Audio1, M1, V1, B1),
	computation(Audio2, M2, V2, B2),
	smpl_mfcc_kldiv(M1, V1, M2, V2, TD),
	smpl_cosine_distance(B1, B2, RD),
	Distance is TD*RD.

sb_r(Audio1, Audio2, Distance):-
	nonvar(Audio1),
	test(Audio2),
	computation(Audio1, _M1, _V1, B1),
	computation(Audio2, _M2, _V2, B2),
	smpl_cosine_distance(B1, B2, Distance).

:-dynamic playlist/3.

/**Similar tracks just in the database**/
similar_rhythm(Audio1):-
	sb_r(Audio1, X, D),
	to_playlist(Audio1, X, D, 0.02, r).

similar_timbre(Audio1):-
	sb_t(Audio1, X, D),
	to_playlist(Audio1, X, D, 20, t).

%They may be stored twice with the altered order
to_playlist(A, B, D, T, M):-
	D < T, !, assert(playlist(A, B, M)).

to_playlist(A, B, D, T, M):-
	D = T, !, assert(playlist(A, B, M)).

to_playlist(_A, _B, D, T, _M):-
	D > T.

/** select the playlist for a file can be in different pairs, we just get one **/
get_playlist(Audio1, Mode, PlayList):-
	findall(X, playlist(Audio1, X, Mode), List1),
	findall(Y, playlist(Y, Audio1, Mode), List2),
	append(List1, List2, List),
	flatten(List, L),
	list_to_set(L, PlayList).

:-dynamic sb_features/4.

computation(Audio, Mean, Var, Beat):-
	sb_features(Audio, Mean, Var, Beat), !.

computation(Audio, Mean, Var, Beat):-
	decode(Audio, Signal),
	fe:vamp_similarity_features(Signal, Mean, Var, Beat),
	assert(sb_features(Audio, Mean, Var, Beat)),
	test(Audio), !, true; assert(test(Audio)).

