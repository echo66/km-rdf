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
#include <limits>

#define MAX_AUDIO_ID_DB 100000000 //I'm not allowed to make it bigger

/*
 * Prototypes to access the database of BlobID records
 */
namespace DataID{

/**
	This function just creates the id atom using a given value as numerical identification for the id.
*/
QString
generate_id();

/**
	This function give us an id of the form __data_x as QString. This function is in charged of
	not generating ids that alreay in the database.
	
	IMPORTANT NOTE: The method doesn't not reassign ids that are desactived because they could be probably retrieved from external storage and the
	id would clash. We only assign ids that are not reserved or taken in one session, so IT IS USER'S RESONSABILITY to restore the session with the 
	ids that will be necessary to keep in the KM.
	Following this algorithm:
		-Starts the count in lastID.
		-Check if the corresponding ID is in the database if not return it.
		-If it is in the database we increment the id which doesn't mean that it would be free as it could be reserved
		independently of the lastID count we have
		-If it is free create the ID and lastID is now updated to the last id which has been assigned.

	This should work properly to avoid ids clashing and to reuse ids as we don't need some of them, BUT IS VERY IMPORTANT TO RESTORE THE
	DATABASE RESERVING IDs EACH TIME. Which is sort of limitation. It may be the case we need to implement some algorithm to read files from a
	folder and reserve every id associated with data
	
*/

QString
generate_free_id();

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

long long
existing_id(const char* );

/**
	Checks the status of the id. Returns:
	
	        -1 no exists
		0 exists but there is no data associated in memory
		1 exists and the id is actively naming data in memory
*/

int
active_id(const char* );

/**
	This function reserves an id in the database even when there is no data for it, setting active to 0. Returns 0 if success and negative if fails
	This should be done at the startup, otherwise the system can not ensure that the id is not taken by another new block of data.
	Note that his method doesn't increment lastID and therefore can be more ids that lastID count.
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

/**
	Returns the last id assigned in the system
*/
const char*
current_id();

/**
	Next id to assign which may be not lastID + 1 as it could be taken
*/
const char*
next_id_to_assign();

}



