/**
	Audio Source is a module that provides SWI-Prolog with a set of predicates for the decoding of different audio file formats:
		-mp3
		-ogg (with Vorbis coding)
		-wav
		-aiff
		-aif
	
	The module allows to work with several files and formats at the same time as the decoded audio data is wrapped into a common functor:

							signal(SampleRate, DataPCM)

	This functor wraps the digital representation of an audio signal and the variable which maps both representation timelines (sample rate)

	This module interfaces some libraries: libmad, liboggz, libfishsound and libsndfile.
	David Pastor Escuredo 2007 for the c4dm, Queen Mary University of London (modified June 2008)
	
	This file contains:

		1. Prolog module predicates: The module "audiosource" defines the public predicates for decoding wrapping open source libraries.

		2. Declaration of rules to hide decoding selection 	
	
	
	Module declaration: Public predicates exported bundled within the audiosource module!

	NOTE: These are the public predicates of the module "audiosource", more predicates may be accessed by using audiosource:predicate sentences, but
	the good working of the module is not guaranteed in that case. For more information, please go to the source code of the different interfaces 
	implemented on the foreign language interface of SWI-Prolog and the SWI-Prolog/C++ interface.
		
		swimad.cpp
		swioggvorbis.cpp
		swisndfile.cpp
		asutils.cpp
*/

:- module(audiosource,[aspl_supported_file/1,
			aspl_decode/2]).

:- style_check(-discontiguous).
:- load_foreign_library(swiaudiosource). /** swiaudiosource.so: Library containing interfaces wrapping the C++/C files mentioned above */
:- use_module(library(pldoc)).

						/**************************************
						********** MODULE PREDICATES **********
						**************************************/
					
%% aspl_supported_file(?Extension). 
% We query about supported files or check if one of our selection is one of them

aspl_supported_file('mp3').
aspl_supported_file('ogg').
aspl_supported_file('wav').
aspl_supported_file('aif').
aspl_supported_file('aiff').
aspl_supported_file('m4a').

%%	aspl_decode(+AudioFilePath, -Signal). 
% Main predicate and the only one that should be called. Thus, we hide which interface and library we are using for decoding just having one entry for every file supported:
%		+AudioFilePath is just a path the file to decode
%		-Signal is a MO:signal. Check /swilib/ doc out for details.

aspl_decode(AudioFile, Signal):-
	aspl_file_extension(AudioFile, Extension),
	aspl_decode(Extension, AudioFile, Signal).

%% aspl_clean_signal_inmemory(+Signal)
% This predicate deletes the audio data in memory when longer needed. Check out aspl.cpp for the source code

						/**********************************************
						********** PROLOG RULES FOR DECODING **********
						**********************************************/

%% aspl_decode(+Extension, +AudioFilePath, -Signal)

% These are rules to select the correct library to decode the file. By checking the file extension only the correct one will be used. Then each rule calls to the specific predicates of each library interface for the correct decoding and signal extraction. 

aspl_decode(Extension, AudioFile, signal(Sr, Data)):-
	Extension = 'mp3',
	mdpl_decode(AudioFile),
	mdpl_get_decoded_signal(Data, Sr).

aspl_decode(Extension, AudioFile, signal(Sr, Data)):-
	Extension = 'ogg',
	ovpl_decode(AudioFile),
	ovpl_get_decoded_signal(Data, Sr).

aspl_decode(Extension, AudioFile, signal(Sr, Data)):-
	Extension = 'wav',
	sfpl_decode(AudioFile),
	sfpl_get_decoded_signal(Data, Sr).

aspl_decode(Extension, AudioFile, signal(Sr, Data)):-
	Extension = 'aiff',
	sfpl_decode(AudioFile),
	sfpl_get_decoded_signal(Data, Sr).

aspl_decode(Extension, AudioFile, signal(Sr, Data)):-
	Extension = 'aif',
	sfpl_decode(AudioFile),
	sfpl_get_decoded_signal(Data, Sr).

aspl_decode(Extension, AudioFile, signal(Sr, Data)):-
	Extension = 'm4a',
	fdpl_decode(AudioFile),
	fdpl_get_decoded_signal(Data, Sr).



