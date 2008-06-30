/**
	Prolog module to work with audiodata extracted from the audiosource module. 
	It handles the data using ID and with the Blobs when it is specifically queried
	David Pastor 2007, c4dm, Queen Mary, University of London. 

	Modified June 2008.

	These functos must be managed by any km-rdf client:

		frame(sample rate,  initpos, [listOfPcm])
		signal(samplerate, [listOfPcm])
		timestamp(start, duration)	
		feature(type, MO::timestamp, Event)
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

			/* Handling binary data. This might me in some other place */
			pointerBlob_to_list/2,
			vectorBlob_to_list/2,
			list_to_pointerBlob/2,
			clean_pointedVector/1,
			is_audio_blob/1,

			/* data id stuff*/
			blob_id/2,
			id_blob/2,
			is_data_id/2,
			active_id/1,
			reserve_id/1,
			is_data_id/1,	
			register_blob/2, /*reserves a new id and stores the blob*/
			register_data_list/2,

			/*db status*/
			current_id/1,
			next_id/1,
			ids_in_db/1,
			id_db_status/3,

			/*i/o data operations*/	
			data/2,
			load_data_list/2,
			blob_to_file/2,
			file_to_blob/2,
			clean_data/1,
			data_in/2,
			data_out/2,

			/*other*/			
			concat_blob/3,
			equal_blob/2,
			data_concat/3, /*id wrapped version*/
			data_compare/2			
			]).

:- style_check(-discontiguous).
:- load_foreign_library(swiaudiodata).
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
	

/************************************** STUFF TO MOVE AWAY ********************************/

/**
	is_data_id(+DataID): True if this is an id for data stored in the system at the running session. The id may be active or not (with actual data 	
	or just reserved id)
*/
is_data_id(Id):-
	is_data_id(Id, _).

/**
	data(+ID, -ListData): This predicate gets the blob wrapping the data identified by the id and decodes it in a prolog list. 
	Fails in case the ID is not registered or desactived.
*/
:- multifile data/2.
data(ID, ListData):-
	id_blob(ID, Blob),
	pointerBlob_to_list(Blob, ListData).

/**
	data_load(+ListData, +ID): We get the raw data in the blob and store it in memory assigning the id given in the predicate .
	IMPORTANT: The ID must be already reserved and desactive!!
*/
load_data_list(ListData, ID):-
	list_to_pointerBlob(ListData, Blob),
	blob_id(ID, Blob).

/**
	data_out(+ID, +FilePath): outputs the data dumping it in a binary file to be stored in disk freeing space in memory. 
*/
data_out(ID, FilePath):-
	id_blob(ID, Blob),
	blob_to_file(Blob, FilePath).

/**
	data_in(+FilePath, +ID): gets the data from an external file using blobs and stores the data assigning the ID.
	IMPORTANT: The ID must be already reserved and must be desactive as well (data cleaned)!!!
*/
data_in(FilePath, ID):-
	file_to_blob(FilePath, Blob),
	blob_id(ID, Blob).

/**Short explanation of imported predicates**/

/**
	get_sample_rate(+Signal or Frame, -SampleRate). Check mopl.cpp

*/

/**
	get_channels(+Signal or Frame, -Channels). Check mopl.cpp

*/

/**
	get_samples_per_channel(+Signal, -SamplesPerChannels). Check mopl.cpp

*/

/**
	set_limit_framing(+SamplesChannel, +StepSize, -Limit)

*/

/**
	pointerBlob_to_list(+Blob, -PrologList). Check audioblobpl.cpp. It basically gets a <#..> blob term and decodes it to get the data in memory
	pointed by the blob and returns a prolog list. It can be also done for blob containing vectors with real raw data.
*/

/**
	list_to_pointerVector(+List, -Blob): Creates a blob from the list and unifies it with the argument. There is no definition to return a blob 
	containing a real vector with the raw data
*/

/**
	get_frame(+Signal, +Start, +BlockSize, -Frame). Signal is a MO:signal from which this predicate extracts a MO:frame for the specific start and
	end samples. This should be called iterativelly to get the frames for further processing.
	Check out framepl.cpp for the source code
*/

/**
	get_timestamp(+Frame, -Timestamp). Gets the timestamp of a frame
	Check out framepl.cpp for the source code
*/

/**
	clean_pointedVector(+Blob). Cleans the data pointed by the vector freeing space in memory (we really want this). This is not very useful
	with the new implementation. We should clean_data(+ID) instead. The memory is far better managed right now
*/

/**
	is_audio_blob(+Blob). Low level way to check if a blob is actually an AudioBlob containing data. We are unlikely to use this as we will handle 
	BlobIds instead and we just have to check if they are in the database or not (we are interested in this high level view). Anyway this stays...
*/

/**
	blob_id(+Id, +Blob). Unifies the blob with the given id and stores the data in the blob in the database. The id must exist and be stored
	already
*/

/**
	id_blob(+Id, -Blob). Gives back the blob storing the data given by the id. Fails if the id is non active
*/

/**
	is_data_id(+Id, -Index). Says if the Id exists or not
*/

/**
	reserve_id(+Id). We reserve a record in the database for this id. This is designed for flexibility and output/input operations. 
	IF WE WANT TO RESERVE SOME ID FOR BLOBS WE MUST DO IT BEFORE CREATING NEW BLOBS TO AVOID CRASHES
*/

/**
	active_id(+Id). Just tells if the id is in the database and if it is active (there is one real blob pointing data associated at that moment)
*/

/**
	clean_data(+ID): cleans the data of the id keeping the id in the database!!!!!!
*/

/**
	blob_to_file(+Blob, +FilePath): dumps the binary data pointed by the blob into a file given its path
*/

/**
	file_to_blob(+FilePath, +Blob): reads the file and stores the data pointing it by the blob
*/

/**
	Checking the status of the database
*/
id_db_status(Size, CurrentID, NextID):-
	ids_in_db(Size),
	current_id(CurrentID),
	next_id(NextID).

/**
	Register blob register_blob(+Blob, -Id)
*/
register_blob(Blob, Id):-
	next_id(Id),
	reserve_id(Id),
	blob_id(Id, Blob).

/**
	register_data_list(+List, -Id)
*/
register_data_list(List, ID):-
	list_to_pointerBlob(List, Blob),
	register_blob(Blob, ID).

/**
	Concat and compare
*/
data_concat(Id1, Id2, Blob):-
	id_blob(Id1, Blob1),
	id_blob(Id2, Blob2),
	concat_blob(Blob1, Blob2, Blob).

data_compare(Id1, Id2):-
	id_blob(Id1, Blob1),
	id_blob(Id2, Blob2),
	equal_blob(Blob1, Blob2).

/************************* black box workflow stuff ****************/

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
	
	


