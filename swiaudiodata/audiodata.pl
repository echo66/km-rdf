/**
	Prolog module to work with audiodata extracted from the audiosource module. 
	It handles the data using ID and with the Blobs when it is specifically queried
	David Pastor 2007, c4dm, Queen Mary, University of London

	ToDo: this module puts a lot of things together that may be useful to define somewhere else...
*/

:- module(audiodata,[
			/* Querying audio data */
			get_sample_rate/2,
			get_channels/2,
			get_samples_per_channel/2,
			get_frame/4,
			get_frame_timestamp/2,
			set_limit_framing/3,
			set_framing/4,
			framed_signal/4,
			framed_signal_bis/4, 
			clean_frame/1,
			clean_pcm/1,
			clean_signal/1,

			/* mixing */
			mix_stereo/2,			

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
			register_blob/1, /*reserves a new id and stores the blob*/
			register_data_list/1,

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
 
/** EXPLANATION
*
* This module is aimed to deal with binary audio data extracted from audio file as MO::signals.
*
* 1. The way we handle the data:
*
* This module allows us to store binary data (normally arbitrary size) and deal with it by means of IDs like __data_id
* The binary data can be input in several ways:
* 	Through a binary file containing a binary representation of a std::vector<float> (data_in/2)
*	Through a prolog list of floats (load_data_list/2)
*
* In both cases we must have reserved the id for the data to store in memory previously using reserve_id/1	
* The way we use import data to the database from prolog is by means of BLOBS (blob_id/2 does the job).
*
* We can store the data externally in files by using data_out/2. Then the data in memory is cleaned when we use this predicate (clean_data/1)
* We can also see the data (if it is not arbitrary long) as prolog list using data/2
* We export the data from the database as prolog BLOBS (id_blob/2 does this)
* 
* There is a bunch of lower level predicates that can be also used to deal with the ids and blobs separately.
*
* 2. Querying audio data
*
*	There is another bunch of predicates to query frames, channels and significant signal details. We obtain signals from the module swiaudiosource
*/


/** RETURNED FUNCTORS REPRESENTING AUDIO DATA 
*
* These functos must be managed by any km-rdf client:

		'Frame'(channels, sample rate,  initpos, [listOfPcm])
		'Signal'(channels, samplerate, samples/channel, [listOfPcm])
		'Timestamp'(start, duration)	
		'Feature'(type, MO::timestamp, Event)
*/


/** PREDICATES */

/**
	framed_signal(+Signal, +StepSize, +BlockSize, -FramedSignal): Returns a framed signal (list of Frames for the given signal) 
	for the passed parameters.
	We have to version. One is based on between and findall and the other in recursive access to lists (last one sounds better)
	*/
framed_signal_bis(Signal, StepSize, BlockSize, FramedSignal):-
	get_samples_per_channel(Signal, N),
	findall([Frame], retrieve_frame(Signal, StepSize, BlockSize, N, Frame), FramedSignal).

retrieve_frame(Signal, StepSize, BlockSize, N, Frame):-
	set_framing(StepSize, N, _, Start),
	get_frame(Signal, Start, BlockSize, Frame).
	
framed_signal(Signal, StepSize, BlockSize, FramedSignal):-
	get_samples_per_channel(Signal, N),
	set_limit_framing(N, StepSize, Limit),
	numlist(0, Limit, List),
	FramedSignal = [],
	create_frames(Signal, StepSize, BlockSize, List, FramedSignal).

create_frames(_, _, _, [], _).
create_frames(Signal, StepSize, BlockSize, [H|T], FramedSignal):-
	Start is StepSize*H,
	get_frame(Signal, Start, BlockSize, Frame),
	append(FramedSignal, [Frame], NewList),
	create_frames(Signal, StepSize, BlockSize, T, NewList).

/**
	Sets the framing. Sets the Start of each frame. It's an iterative way to go through all the frames using between/3. We then findall the frames.
	*/
set_framing(StepSize, Samples, Limit, Start):-
	set_limit_framing(Samples, StepSize, Limit),
	between(0, Limit, N),
	Start is StepSize * N.

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
	get_frame(+Signal, +Start, +End, -Frame). Signal is a MO:signal from which this predicate extracts a MO:frame for the specific start and
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
	Cleans the binary data of a frame from memory, we don't really to keep this intermediate result I guess

		clean_frame(+Frame)

	Remember the pcm data is '__data_id'
*/
clean_frame('Frame'(_Channels, _SampleRate, _Start, ListPcm)):-
	clean_pcm(ListPcm).

/**
	clean_pcm(ListOFPcmChannel).
*/
clean_pcm([]).
clean_pcm([H|T]):-
	clean_data(H),
	clean_pcm(T).	
	
/**
	Same for signal
*/
clean_signal('Signal'(_Channels, _SampleRate, _Samples, ListPcm)):-
	clean_pcm(ListPcm).

/**
	Checking the status of the database
*/
id_db_status(Size, CurrentID, NextID):-
	ids_in_db(Size),
	current_id(CurrentID),
	next_id(NextID).

/**
	Register blob register_blob(+Blob)
*/
register_blob(Blob):-
	next_id(Id),
	reserve_id(Id),
	blob_id(Id, Blob).

/**
	register_data_list(+List)
*/
register_data_list(List):-
	list_to_pointerBlob(List, Blob),
	register_blob(Blob).

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



