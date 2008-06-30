/**
	Prolog module to work with audiodata extracted from the audiosource module. 
	It handles the data using ID and with the Blobs when it is specifically queried
	David Pastor 2007, c4dm, Queen Mary, University of London. 

	Modified June 2008.

	These functos must be managed by any km-rdf client:

		frame(sample rate,  initpos, [listOfPcm])
		signal(samplerate, [listOfPcm])
		timestamp(start, duration)	
		feature(type, Timestamp, Event)
*/

:- module(audiodata,[	get_sample_rate/2
			,	get_channels/2
			,	get_samples_per_channel/2
			,	get_frame/4
			,	get_frame_timestamp/2
			,	set_limit_framing/3
			,	set_framing/4
			,	frame_signal/4
			,	clean_/1
			,	mix_stereo/2					
			]).

:- style_check(-discontiguous).
:- load_foreign_library(swiaudiodata).
:- use_module('../swidata/data').
:- use_module(library(pldoc)).

%% get_sample_rate(+Signal, -SampleRate) is det
% Returns back the sample rate of a signal or frame

get_sample_rate(signal(Sr, _), Sr).
get_sample_rate(frame(Sr, _, _), Sr).

%% get_channels(+Signal, -Channels) is det
% Returns back the number of channels of the signal or frame

get_channels(signal(_, Data), Ch):-
	is_list(Data),
	length(Data, Ch).
get_channels(frame(_, _, Data), Ch):-
	is_list(Data),
	length(Data, Ch).

%% get_samples_per_channel(+Signal, -SamplesPerChannel) is det
% Returns back the number of samples per channels (assuming all channels have the same length)

get_samples_per_channel(signal(_, Data), L):-
	is_list(Data),
	data_size(Data, L).
get_samples_per_channel(frame(_, _, Data), L):-
	is_list(Data),
	data_size(Data, L).

%% get_frame(+Signal, +Init, +Block, -Frame) is det
% Frames a signal(Sr, Data) object returning frame(Sr, Init, Data) object according to the parameters 

get_frame(signal(Sr, Data), Init, Block, frame(Sr, Init, Data2)):-
	frame_for_signal(Data, Init, Block, Data2).

%% get_frame_timestamp(+Frame, -Timestamp) is det
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
		
%% clean(+DataTerm) is nondet
% Removes the data from a Frame or a Signal where data is in the type of '__data_id'. This is necessary to relief memory

clean(frame(_, _, ListPcm)):-
	clean(ListPcm).
clean(signal(_, ListPcm)):-
	clean(ListPcm).
clean([]).
clean([H|T]):-
	clean_data(H),
	clean(T).	

%% frame_signal(+Signal, +StepSize, +BlockSize, -FramedSignal) is nondet
% Returns a framed signal (list of Frames for the given signal) for the passed parameters.
	
frame_signal(Signal, StepSize, BlockSize, FramedSignal):-
	get_samples_per_channel(Signal, N),
	findall([Frame], retrieve_frame(Signal, StepSize, BlockSize, N, Frame), FramedSignal).

retrieve_frame(Signal, StepSize, BlockSize, N, Frame):-
	set_framing(StepSize, N, _, Start),
	get_frame(Signal, Start, BlockSize, Frame).

% mix_stereo(+StereoSignal, -MonoSignal) is nondet
% Mixes a stereo signal and returns the mono signal

mix_stereo(signal(Sr, [Ch1, Ch2]), signal(Sr, [Ch])):-
	data_mean(Ch1, Ch2, Ch).
	
/************************* FIXME ****************/

/** put frames to signal. not sure it works */

frames_to_signal(ListFrames, Signal):-
	ListFrames = [H|Rest],
	H = 'Frame'(C, Sr, _Start, PCMs),
	add_frames(Rest, PCMs, PCMsF),
	put_to_id(PCMsF, Sigs),
	Signal = 'Signal'(C, Sr, _, Sigs).

put_to_id([], _).
put_to_id([H|T], [L|M]):-
	register_blob(H, L),
	put_to_id(T, M).

add_frames([], _, _).
add_frames([H|T], PCMs, List):-
	H = 'Frame'(_, _, _, PCMs2),
	concat_frames(PCMs, PCMs2, A),
	add_frames(T, A, List). 
concat_frames([], [], _).
concat_frames([H1|T1], [H2|T2], [H|T]):-
	data_concat(H1, H2, H),
	concat_frames(T1, T2, T).
	
	


