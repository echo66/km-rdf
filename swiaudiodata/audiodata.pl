/**
	Prolog module to work with audiodata extracted from the audiosource module. 
	It handles the data using ID and with the Blobs when it is specifically queried
	David Pastor 2007, c4dm, Queen Mary, University of London
*/

:- module(audiodata,[
			/* Querying audio data */
			get_sample_rate/2,
			get_channels/2,
			get_samples_per_channel/2,
			get_frame/4,
			get_frame_timestamp/2,
			set_limit_framing/3,

			/* Handling binary data */
			pointerBlob_to_list/2,
			vectorBlob_to_list/2,
			list_to_pointerBlob/2,
			clean_pointedVector/1,
			is_audio_blob/1,
			blob_id/2,
			id_blob/2,
			is_data_id/2,
			active_id/1,
			reserve_id/1,
			is_data_id/1,		
			data/2,
			load_data_list/2,
			blob_to_file/2,
			file_to_blob/2,
			clean_data/1,
			data_in/2,
			data_out/2
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

/** PREDICATES **/

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
	data_out(+ID, +FilePath): outputs the data dumping it in a binary file to be stored in disk freeing space in memory. The data ID is
	automatically cleaned!!
*/
data_out(ID, FilePath):-
	id_blob(ID, Blob),
	blob_to_file(Blob, FilePath),
	clean_data(ID).	

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

