/*
 * Source for the data ids database. These ids wrap BLOB (binarly large objects of data) so we can deal with them easily from a prolog
 * David Pastor 2007, c4dm, Queen Mary, University of London
 *
 * Define some operations over blob ids (May 2008)
 *
 * Note: that library only is valid during a session of the km, loosing the id in the database afterwards.
 */

#include <blobid.h>
#include <iostream>
#include <cstdlib>
#include <vector>

using namespace std;
using namespace AudioDataConversion;
/***** Variables ******/

/*
 * This structure associates an id to each raw data
 */
static struct AudioDataRecord{

	/* id: __data_id */
	QString id;
	/* pointer to the raw data in memory */
	vector<float> *data;
	/* flag 
		0 the id has been created but the data has been delted, we keep the id to use if afterwars
		1 id with associated data in memory
	*/
	int active;
}

/*
 * Database with all the entries id/data. There is a maximum of ids in the system. This may be a significant limitation...
 */

audio_data_db[MAX_AUDIO_ID_DB];//The maximum number of ids in the database is the maximum value that an unsigned long can reach 4 294 967 295

/*
 * This is the last ID created and assigned from the system. This doesn't mean this is the number of IDs on the database as we may have reserved
 * some ids in higher positions of the database. This count is only meaningful to assing the lowest id possible to the new data. This will be useful
 * to reuse ids in different sessions when we do not longer need certain ids.
 */

size_t lastID = 0; 

/*
 * This is the count of ids in the system. This is necessary to keep order in the database and put the ids in the lower positions. Therefore there is
 * no necessary correspondence between the id and its position in the database
 */
size_t ids_in_system = 0;

namespace BLOBID{
/**** Implementation of functions *****/

/**
	This function just creates the id atom using a given value as numerical identification for the id.
*/
QString 
generate_id(size_t value){

	QString head("__data_");
	QString var;
	var = QString("%1")
		.arg((long)value);
	head.append(var);
	return head;
}

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
generate_free_id(){

	QString new_id;
	new_id = generate_id(lastID);//with lastID variable

	//if the ID exists it measn we can not use even it is active or not. The desactive ID's are reserved for a block of data within a session.
	while(existing_id(new_id.toLocal8Bit().data())>=0){
		lastID++;
		new_id = generate_id(lastID);
	}
	return new_id;
} 

/**
	Initial function of the database. Given a pointer to raw data, we generate and id and store the record in the database setting it active.
	We return the id to be handled. It's user's responsability the correct use of this.
	From here on we use the id to name the raw data and a blob to return it to prolog if required as a pointerBlob (check swiaudioblob)
*/
const char * 
assign_data_id(vector<float> *data)
{
	QString ident;
	ident = generate_free_id();
	audio_data_db[ids_in_system].id = ident;
	audio_data_db[ids_in_system].active = 1;
	audio_data_db[ids_in_system].data = data;
	ids_in_system++;
	return ident.toLocal8Bit().data();
}

/**
	New predicate for the version 2. Internal C++ code we don't need to wrap the data into a blob, so we just directly extract the pointer to the
	vector. Error if it returns < 0
*/
int
get_data_for_id(const char* ident, std::vector<float>* &m_data)
{
	long long pos = 0;
	pos = existing_id(ident);
	if(pos < 0){
		 return -1;//no id. -1
	}
	else if(audio_data_db[(size_t)pos].active == 0){
		return 0; //not active
	}
	else{		
		m_data = audio_data_db[(size_t)pos].data;//gets the data.	
		return 1; //was active	
	}
}

/**
	Extracts the data from the blob and stores it in the given id record. This is is thought for the case when the id is reserved and we use a
	blob to store the data for that id
		
		-3 the input blob in incorrect
		-1 if no id in the database. The term is not unified
		-2 if there was some data in the id
		 0 if not active and the blob is stored
*/
int 
set_blob_for_id(const char *ident, term_t blob)
{
	long long pos = 0;
	pos = existing_id(ident);
	if(pos<0){
		cerr<<"The Id doesn't exist"<<endl;
		return -2;
	}
	
	if(pos<0) return -1; //no id.
	
	if(audio_data_db[(size_t)pos].active == 1) return -2; //the id was already taken
	audio_data_db[(size_t)pos].active = 1;
	if(is_audio_blob(blob)==0){
		audio_data_db[(size_t)pos].data = audio_blob_to_pointer(blob);
		return 0;
	}else{
		return -3;
	}
}

/**
	Returns the blob for the given id unifying with the term. Returns:
		
		-2 error with the blob
		-1 if no id in the database. The term is not unified
		0  if the id is non active. The term is not unified
		1 if active and the term is unified with the blob

	Here now, we combine the library for swiaudioblob to wrap the data when it is identified.
*/
int 
get_blob_from_id(const char *ident, term_t blob)
{
	long long pos = 0;
	pos = existing_id(ident);
	if(pos < 0){
		 return -1;//no id. -1
	}
	else if(audio_data_db[(size_t)pos].active == 0){
		return 0; //not active
	}
	else{		
		pointer_to_audio_blob(audio_data_db[(size_t)pos].data, blob);//unifies blob. 
		if(is_audio_blob(blob)==1){
			return -2;
		}		
		return 1; //was active
	
	}
}

/**
	Checks if the id given in the string is already in the database. Useful in many ways. Returns
	
	        -1 no exists
	 	index in the table if exists

	we need to return a long long to cover the long and the negative value. I could have change this really...
*/
long long
existing_id(const char* ident)
{
	int flag = -1;
	size_t pos = 0;//usigned long which is the size of the database
	QString input(ident);
	for(size_t i=0; i<ids_in_system; i++){//this was a bug
		
		if(input.compare(audio_data_db[i].id)==0) {//this check should be correctly defined
			flag = 0;
			pos = i;
		}	
	}
	if(flag == 0){
		return pos;
	}
	return flag;
}

/**
	Checks the status of the id. Returns:
	
	        -1 no exists
		0 exists but there is no data associated in memory
		1 exists and the id is actively naming a data in memory
*/
int
active_id(const char *ident)
{
	int flag = -1;
	int pos = 0;
	QString input(ident);
	for(size_t i=0; i<ids_in_system; i++){
		
		if(input.compare(audio_data_db[i].id)==0) {//this check should be correctly defined
			flag = 0;
			pos = i;
		}	
	}
	if(flag == 0){
		return audio_data_db[pos].active;
	}
	return flag;
}

/**
	This function reserves an id in the database even when there is no data for it, setting active to 0. Returns 0 if success and negative if fails
	This should be done at the startup, otherwise the system can not ensure that the id is not taken by another new block of data.
	Note that his method doesn't increment lastID and therefore can be more ids that lastID count.
*/
int
reserve_id(const char *ident)
{
	if(existing_id(ident)<0){

		if(lastID == MAX_AUDIO_ID_DB){
			std::cerr<<"more blobs than possible"<<std::endl;
			return -2;
		}		
		audio_data_db[ids_in_system].id = ident;
		audio_data_db[ids_in_system].active = 0;
		ids_in_system++;//we increment the ids in system but not the lastID count
		//std::cerr<<ident<<" reserved"<<std::endl;
		return 0;

	}else{
		return -1;	
	}

}

/**
	Delete the data for the id and desactive it. This is the way we are going to use to free space in memory!!! IMPORTANT!!!
	We don't remove the id and we don't reuse ids during the session.

	WE MAY NEED TO CHANGE THIS BEHAVIOUR LATER ON
	
	Just with a prolog version of this we do clean_data(ID), we delete the raw data in memory. The blobs used to wrap this data will be
	garbage-collected at some point. This new implementation manages far better the memory resources
*/
int
clean_data_for_id(const char* ident){

	long long pos = 0;
	pos = existing_id(ident);

	if(pos<0) return -1; //no id.
	
	if(audio_data_db[(size_t)pos].active == 0) return -2;//already desactived

	audio_data_db[(size_t)pos].active = 0;
	
	delete audio_data_db[(size_t)pos].data;
	audio_data_db[(size_t)pos].data = 0;//reset the pointer

	//What happens to the reference to the blob once we have deleted the data?????
	//std::cerr<< ident <<" free"<<std::endl;
	return 0;
}

/**
	Returns the number of ids in the database
*/
size_t
ids_in_db()
{
	return ids_in_system;
}

/**
	Returns the last id assigned in the system
*/
const char*
current_id()
{

	return generate_id(lastID).toLocal8Bit().data();
}

/**
	Next id to assign which may be not lastID + 1 as it could be taken
*/
const char*
next_id_to_assign()
{
	//This is temporary so we save the context
	size_t oldLast = lastID;
	const char *nextID = generate_free_id().toLocal8Bit().data();
	lastID = oldLast;//restoring the context
	
	return nextID;
}

}//end of BLOBID
