/*
	This source is aimed to hide the prolog terms referring the blobs with specific ids
	It defines the BlobID namespace
	David Pastor 2007, c4dm
	
	ToDo: we delete specific blobs types from here adding libraries or from prolog???? what to do exactly with the blob terms and their references!
*/

#include <SWI-cpp.h>
#include <qstring.h>

#define MAX_BLOBS 10000

/*
 * Prototypes to access the database of BlobID records
 */
namespace BlobID{

/**
	This function give us an id of the form __blob_x as QString. The x may vary randomly between 0 and MAX_BLOBS-1. This function is in charged of
	not generating id that alreay in the database.
*/
QString
generate_id();

QString
generate_random_id();

/**
	Initial function of the database. Given a blob, we generate and id and store the record in the database setting it active.
	We return the id to be handled. It's user's responsability the correct use of this.
	If we need some other id to be reserved and not used by this blob. WE MUST RESERVE THEM FIRST!!!
*/
const char*
assign_blob_id(term_t );

/**
	Unifies the blob for the given id unifying with the term. This is thought for the case when the id is reserved and we want then to 
	set the blob for it. Returns:
		
		-1 if no id in the database. The term is not unified
		-2 if the blob was areadly active
		 0 if non active and the blob is stored
*/
int 
set_blob_for_id(const char*, term_t );

/**
	Returns the blob for the given id unifying with the term. Returns:
		
		-1 if no id in the database. The term is not unified
		 0 if the id is non active. The term is not unified
		 1 if active and the term is unified with the blob
*/
int 
get_blob_from_id(const char*, term_t );

/**
	Checks if the id given in the string is already in the database. Useful in many ways. Returns
	
	        -1 no exists
	 	index in the table if exists
*/
int
existing_id(const char* );

/**
	Checks the status of the id. Returns:
	
	        -1 no exists
		0 exists but there is no blob associated in memory
		1 exists and the id is actively naming a blob in memory
*/
int
active_id(const char* );

/**
	This function reserves an id in the database even when there is no blob for it setting active to 0. Returns 0 if success and negative if fails
*/
int
reserve_id(const char* );

/**
	Desactive an id when the blob is deleted from memory. The data itself must be deleted from the specific blob library!!!!
	We would have to do this in prolog:
	clean_blob(ID):-
		blob_for_id(ID, BLOB),
		audioblob_delete(BLOB),
		desactive(ID).

	NOTE: what do we have to do with BLOB_ID.blob term reference????
*/
int
desactive_id(const char* );

/**
	Another way to do the same but passing the index as argument
*/
int
desactive_id(size_t pos);

/**
	Returns the number of id in the database
*/
size_t
ids_in_db();

}



