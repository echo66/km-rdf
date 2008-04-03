/**
	Some cheap utils
	David Pastor 2008
	**/

:- module(to_builtins,[to_key/2]).

:- use_module(library('semweb/rdf_db')).

:- use_module('../builtins').

:- rdf_register_ns(mo,'http://purl.org/ontology/mo/').
:- rdf_register_ns(tl,'http://purl.org/NET/c4dm/timeline.owl#').
:- rdf_register_ns(to,'http://purl.org/ontology/tonality/').


builtins:builtin('http://purl.org/ontology/dsp/pitch_interval', to_builtins:pitch_interval).

pitchClass('C', 0.0).
pitchClass('Cs', 1.0).
pitchClass('D', 2.0).
pitchClass('Ds', 3.0).
pitchClass('E', 4.0).
pitchClass('F', 5.0).
pitchClass('Fs', 6.0).
pitchClass('G', 7.0).
pitchClass('Gs', 8.0).
pitchClass('A', 9.0).
pitchClass('As', 10.0).
pitchClass('B', 11.0).

enharmonic('Cs', 'Db').
enharmonic('Ds', 'Eb').
enharmonic('Fs', 'Gb').
enharmonic('As', 'Bb').
enharmonic('F', 'Es').
enharmonic('C', 'Bs').

pitch_interval(Pitch, Semitones, NewPitch):-
	pitchClass(Pitch, Class),
	Absolute is Class+Semitones,
	(Absolute>11, NewClass is Absolute-7; NewClass = Absolute),
	pitchClass(NewPitch, NewClass).
	

pitch_interval(Pitch, Semitones, NewPitch):-
	enharmonic(Pitch2, Pitch),
	pitchClass(Pitch2, Class),
	Absolute is Class+Semitones,
	(Absolute>11, NewClass is Absolute-7; NewClass = Absolute),
	pitchClass(NewPitch, NewClass).


key_id(1.0, 'http://purl.org/ontology/tonality/key/Cmajor').
key_id(2.0, 'http://purl.org/ontology/tonality/key/Csmajor').
key_id(3.0, 'http://purl.org/ontology/tonality/key/Dmajor').
key_id(4.0, 'http://purl.org/ontology/tonality/key/Dsmajor').
key_id(5.0, 'http://purl.org/ontology/tonality/key/Emajor').
key_id(6.0, 'http://purl.org/ontology/tonality/key/Fmajor').
key_id(7.0, 'http://purl.org/ontology/tonality/key/Fsmajor').
key_id(8.0, 'http://purl.org/ontology/tonality/key/Gmajor').
key_id(9.0, 'http://purl.org/ontology/tonality/key/Gsmajor').
key_id(10.0, 'http://purl.org/ontology/tonality/key/Amajor').
key_id(11.0, 'http://purl.org/ontology/tonality/key/Asmajor').
key_id(12.0, 'http://purl.org/ontology/tonality/key/Bmajor').

key_id(13.0, 'http://purl.org/ontology/tonality/key/Cminor').
key_id(14.0, 'http://purl.org/ontology/tonality/key/Csminor').
key_id(15.0, 'http://purl.org/ontology/tonality/key/Dminor').
key_id(16.0, 'http://purl.org/ontology/tonality/key/Dsminor').
key_id(17.0, 'http://purl.org/ontology/tonality/key/Eminor').
key_id(18.0, 'http://purl.org/ontology/tonality/key/Fminor').
key_id(19.0, 'http://purl.org/ontology/tonality/key/Fsminor').
key_id(20.0, 'http://purl.org/ontology/tonality/key/Gminor').
key_id(21.0, 'http://purl.org/ontology/tonality/key/Gsminor').
key_id(22.0, 'http://purl.org/ontology/tonality/key/Aminor').
key_id(23.0, 'http://purl.org/ontology/tonality/key/Asminor').
key_id(24.0, 'http://purl.org/ontology/tonality/key/Bminor').

to_key(ID, Key):-
	key_id(ID, Key).



	
