/**
	Prolog module to work with audiodata extracted from the audiosource module. 
	It handles the data using ID and with the Blobs when it is specifically queried
	David Pastor 2007, c4dm, Queen Mary, University of London
*/

:- module(audiodata,[get_sample_rate/2,
			get_channels/2,
			get_samples_per_channel/2,
			get_frame/4,
			pointerBlob_to_list/2,
			vectorBlob_to_list/2,
			list_to_pointerBlob/2,
			get_frame_timestamp/2,
			set_limit_framing/3,
			clean_pointedVector/1,
			is_audio_blob/1,
			blob_id/2,
			id_blob/2,
			is_data_id/2,
			active_id/1,
			reserve_id/1,
			is_data_id/1,		
			data/2,
			data_load/2
			]).

:- style_check(-discontiguous).
:- load_foreign_library(swiaudiodata).
 
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
	data_load(+ListData, +ID): We get the raw data in the blob and store it in memory assigning the id given in the predicate 
*/
data_load(ListData, ID):-
	list_to_pointerBlob(ListData, Blob),
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


