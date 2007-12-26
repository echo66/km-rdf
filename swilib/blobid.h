/*
	This source is aimed to hide the raw data using IDs, a database and blobs to wrap the data when query from prolog
	It defines the DataID namespace
	David Pastor 2007, c4dm, Queen Mary, University of London
*/

#include <swiaudioblob.h>
#ifndef _FLI_H_INCLUDED
#include <SWI-cpp.h>
#endif
#include <qstring.h>
#include <vector>

#define MAX_AUDIO_ID_DB 1000000

/*
 * Prototypes to access the database of BlobID records
 */
namespace DataID{

/**
	This function give us an id of the form __data_x as QString. This function is in charged of not generating id that alreay in the database.
*/
QString
generate_id();

QString
generate_random_id();//this is finally incremental

/**
	Initial function of the database. Given a pointer to raw data, we generate and id and store the record in the database setting it active.
	We return the id to be handled. It's user's responsability the correct use of this.
	From here on we use the id to name the raw data and a blob to return it to prolog if required as a pointerBlob (check swiaudioblob)
*/
const char*
assign_data_id(std::vector<float> *);

/**
	New predicate for the version 2. Internal C++ code we don't need to wrap the data into a blob, so we just directly extract the pointer to the
	vector. Error if it returns < 0
*/
int
get_data_for_id(const char*, std::vector<float>* &);


/**
	Extracts the data from the blob and stores it in the given id record. This is is thought for the case when the id is reserved and we use a
	blob to store the data for that id
		
		-3 the input blob in incorrect
		-1 if no id in the database. The term is not unified
		-2 if there was some data in the id
		 0 if not active and the blob is stored
*/
int 
set_blob_for_id(const char*, term_t );

/**
	Returns the blob for the given id unifying with the term. Returns:
		
		-2 error with the blob
		-1 if no id in the database. The term is not unified
		0  if the id is non active. The term is not unified
		1 if active and the term is unified with the blob

	Here now, we combine the library for swiaudioblob to wrap the data when it is identified.
*/
int 
get_blob_from_id(const char*, term_t );

/**
	Checks if the id given in the string is already in the database. Useful in many ways. Returns
	
	        -1 no exists
	 	index in the table if exists
*/
double
existing_id(const char* );

/**
	Checks the status of the id. Returns:
	
	        -1 no exists
		0 exists but there is no data associated in memory
		1 exists and the id is actively naming a data in memory
*/
int
active_id(const char* );

/**
	This function reserves an id in the database even when there is no data for it, setting active to 0. Returns 0 if success and negative if fails
*/
int
reserve_id(const char* );

/**
	Delete the data for the id and desactive it. This is the way we are going to use to free space in memory!!! IMPORTANT!!!
	
	Just with a prolog version of this we do clean_data(ID), we delete the raw data in memory. The blobs used to wrap this data will be
	garbage-collected at some point. This new implementation manages far better the memory resources
*/
int
clean_data_for_id(const char*);

/**
	Returns the number of id in the database
*/
size_t
ids_in_db();

}



