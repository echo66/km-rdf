/**
	Prolog module to work with audiodata extracted from the audiosource module. 
	It handles the data as blobs, and the blobs are handled by BlobIDs that make transparent the <#...> invalid terms.
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
			/*we introduce now, the BlobIDs to hide the ugly <#..> stuff. It's basically a new layer than can be combined with the old
			code*/
			blob_id/2,
			id_blob/2,
			is_blob_id/2,
			active_id/1,
			desactive_entry/1,
			desactive_id/1,
			reserve_id/1,
			/*and the combination of audioblobpl and blobidpl gives us a new way to handle data that fits better to sem web/n3 structure*/
			blob/1,		
			audio_blob/1,	
			blob_data/2,
			]).

:- style_check(-discontiguous).
:- load_foreign_library(swiaudiodata).
 
/**
	blob(+BlobID): True if this is a blob (the interpretation is that we have it stored in our blob_id_db, check out swilib/). It just checks
	if the blob is registered but may be active or not!!!
*/
blob(BlobID):-
	is_blob_id(BlobID, _).

/**
	audio_blob(+BlobID): This is a different and more specific way to look at blob/1. We get the real blob and checks it against the audioblob 		library
*/
audio_blob(BlobID):-
	id_blob(BlobID, Blob),
	is_audio_blob(Blob).

/**
	blob_data(+BlobID, -ListData): This predicate gets the blob identified by the id and decodes it in a prolog list. Fails in case the ID is not
	registered or desactived. (no need to call blob/1)
*/
blob_data(BlobID, ListData):-
	id_blob(BlobID, Blob),
	pointerBlob_to_list(Blob, ListData).

/**
	data_blob(+ListData, +BlobID): For the given prolog list with raw data and id to be unified, we retrieve a blob (pointing to raw data in memory 	named by its ID. The ID must be desactived before and activated from here on.
*/
data_blob(ListData, BlobID):-
	list_to_pointerVector(ListData, Blob),
	blob_id(Blob, BlobID).	

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
	clean_pointedVector(+Blob). Cleans the data pointed by the vector freeing space in memory (we really want this). This predicate is the low level
	one, we should clean_blob(+BlobId) instead. 
	

	IMPORTANT NOTE!!!!!!!!!!!!
	We're missing what to do with the blob itself and its term reference. I'm almost assuming not doing anything with them as we
	don't waste much data. Blob garbage collection is still something to improve
*/

/**
	is_audio_blob(+Blob). Low level way to check if a blob is actually an AudioBlob containing data. We are unlikely to use this as we will handle 
	BlobIds instead and we just have to check if they are in the database or not (we are interested in this high level view). Anyway this stays...
*/

/**
	blob_id(+Blob, +BlobId). Unifies the blob with the given id and stores the blob in the database. The id must exist and be stored already
*/

/**
	id_blob(+BlobId, -Blob). Gives back the blob stored given its id. Fails if the id is non active
*/

/**
	is_blob_id(+BlobId, -Index). New conception of blob(Blob). If the id is stored in the database we do know that this id hides a blob and it's all
	that matters. Anyway we can check if it is certainly a blob or not, by using is_audio_blob/2. Gives back the index for practical issues
*/

/**
	reserve_id(+BlobId). We reserve a record in the database for this id. This is designed for flexibility and output/input operations. 
	IF WE WANT TO RESERVE SOME ID FOR BLOBS WE MUST DO IT BEFORE CREATING NEW BLOBS TO AVOID CRASHES
*/

/**
	active_id(+BlobId). Just tells if the id is in the database and if it is active (there is one real blob pointing data associated at that moment)
*/

/**
	desactive_id(+BlobId). After having deleted a blob (THIS THING IS NOT DONE YET) we desactive the id, we don't delete it as we may want the id
	to get the blob back. we can use the entry of the database to do the same with desactive_entry/1
*/
