/**
	<module> audio for SWI-Prolog. This module wraps decoding libraries to decode several file formats under the same predicate decode/2. The resto of the module defines operations over a signal term (see README).

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2007 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
*/

:- module(audio,[	supported_file/1
			,	decode/2
			,	get_sample_rate/2
			,	get_channels/2
			,	get_samples_per_channel/2
			,	get_frame/4
			,	get_frame_timestamp/2
			,	set_limit_framing/3
			,	set_framing/4
			,	frame_signal/4
			,	clean/1
			,	mix_stereo/2
			]).

:- style_check(-discontiguous).
:- use_module('../swidata/data').
:- load_foreign_library(swiaudio). 
%:- use_module(library(pldoc)).

						/**************************************
						********** MODULE PREDICATES **********
						**************************************/
					
%% supported_file(?Extension) is nondet
% We query about supported files or check if one of our selection is one of them

supported_file('mp3').
supported_file('ogg').
supported_file('wav').
supported_file('aif').
supported_file('aiff').
supported_file('m4a').

%% decode(+AudioFilePath, -Signal) is semidet
% Main predicate that hides decoding of an audio file given the path and returns a compound term: signal (SampleRate, Data) where Data is a list of blobs id (check swilib).

decode(AudioFile, Signal):-
	aspl_extension(AudioFile, Extension),
	aspl_decode(Extension, AudioFile, Signal).

%% get_sample_rate(+Signal, -SampleRate) is semidet
% Returns back the sample rate of a signal or frame

get_sample_rate(signal(Sr, _), Sr).
get_sample_rate(frame(Sr, _, _), Sr).

%% get_channels(+Signal, -Channels) is semidet
% Returns back the number of channels of the signal or frame

get_channels(signal(_, Data), Ch):-
	is_list(Data),
	length(Data, Ch).
get_channels(frame(_, _, Data), Ch):-
	is_list(Data),
	length(Data, Ch).

%% get_samples_per_channel(+Signal, -SamplesPerChannel) is semidet
% Returns back the number of samples per channels (assuming all channels have the same length)

get_samples_per_channel(signal(_, Data), L):-
	is_list(Data),
	Data = [H|_T],
	data_size(H, L).
get_samples_per_channel(frame(_, _, Data), L):-
	is_list(Data),
	Data = [H|_T],
	data_size(H, L).

%% get_frame(+Signal, +Init, +Block, -Frame) is semidet
% Frames a signal(Sr, Data) object returning frame(Sr, Init, Data) object according to the parameters 

get_frame(signal(Sr, Data), Init, Block, frame(Sr, Init, Data2)):-
	frame_channels(Data, Init, Block, Data2).

frame_channels([], _, _, []).
frame_channels([H|T], Init, Block, [H2|T2]):-
	blob_frame(H, Init, Block, H2),
	frame_channels(T, Init, Block, T2).

%% get_frame_timestamp(+FraCentre for Digital Music, Queen Mary, University of London.
% Returns the timestamp (time values) of a frame related to the owner signal timeline

get_frame_timestamp(Frame, timestamp(Start, Duration)):-
	Frame = frame(Sr, Init, _),
	get_samples_per_channel(Frame, L),
	Start is Init/Sr,
	Duration is L/Sr.
	
%% set_framing(+StepSize, +SignalLength, -Limit, -Start) is nondet
% This predicate returns iteratively the Start of the following frame in a framing process. This framing predicate relies on the procedural meaning and is subject of substitution.

set_framing(StepSize, Samples, Limit, Start):-
	set_limit_framing(Samples, StepSize, Limit),
	between(0, Limit, N),
	Start is StepSize * N.
		
%% clean(+DataTerm) is det
% Removes the data from a Frame or a Signal where data is in the type of '__data_id'. This is necessary to relief memory

clean(frame(_, _, ListPcm)):-
	clean(ListPcm).
clean(signal(_, ListPcm)):-
	clean(ListPcm).
clean([]).
clean([H|T]):-
	clean_blob(H),
	clean(T).	

%% frame_signal(+Signal, +StepSize, +BlockSize, -FramedSignal) is det
% Returns a framed signal (list of Frames for the given signal) for the passed parameters.
	
frame_signal(Signal, StepSize, BlockSize, FramedSignal):-
	get_samples_per_channel(Signal, N),
	findall([Frame], retrieve_frame(Signal, StepSize, BlockSize, N, Frame), FramedSignal).

retrieve_frame(Signal, StepSize, BlockSize, N, Frame):-
	set_framing(StepSize, N, _, Start),
	get_frame(Signal, Start, BlockSize, Frame).

%% mix_stereo(+StereoSignal, -MonoSignal) is det
% Mixes a stereo signal and returns the mono signal

mix_stereo(signal(Sr, [Ch1, Ch2]), signal(Sr, [Ch])):-
	blobs_mean(Ch1, Ch2, Ch).
	

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

%% aspl_extension(+AudioFile, -Extension) is semidet
% Returns the file extension

aspl_extension(AudioFile, Extension):-
	concat_atom([_Name, Extension], '.', AudioFile).

%% set_limit_framing(+Samples, +StepSize, -Limit) is semidet
% Limit of framing

set_limit_framing(A, B, C):-
	C is A//B.
