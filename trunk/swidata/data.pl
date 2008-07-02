/**
	Unified way to handle data
  
	Blobs, ids, datalists
*/

:- module(data, [/* Handling binary data. This might me in some other place */
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
:- load_foreign_library(swidata).
:- use_module(library(pldoc)).


/************************************** FIXME ********************************/

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
	pointerBlob_to_list(+Blob, -PrologList). Check audioblobpl.cpp. It basically gets a <#..> blob term and decodes it to get the data in memory
	pointed by the blob and returns a prolog list. It can be also done for blob containing vectors with real raw data.
*/

/**
	list_to_pointerVector(+List, -Blob): Creates a blob from the list and unifies it with the argument. There is no definition to return a blob 
	containing a real vector with the raw data
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











