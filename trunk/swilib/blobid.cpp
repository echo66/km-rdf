/*
 * Source to deal raw data identifiers. We assign an id to each pointer of raw data stored in memory, so this identifier is our prolog id for the data.
 * This changes respect to the previous version because we used to handle blobs to represent the data at every stage, now we just use blobs to wrap data
 * once it is queries specifically.
 * David Pastor 2007, c4dm, Queen Mary, University of London
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
 * Database with all the entries id/data
 */

audio_data_db[MAX_AUDIO_ID_DB];

/*
 * The number of ids created and stored in the database. Note that once an id is created it won't be deleted.
 * Note that may be id only reserved but without data associated
 */
size_t ids_in_system;

namespace DataID{
/**** Implementation of functions *****/

/**
	We split this up from generate_free_id to make the code clearer
*/
QString 
generate_random_id(){

	QString head("__data_");

	//srand((unsigned)time(0)); 
    	//size_t random; 
   	//size_t lowest=1, highest=MAX_BLOBS; 
   	//size_t range=(highest-lowest)+1; 
   	//random = lowest+size_t(range*rand()/(RAND_MAX + 1.0)); 
	//QString var((long)random, 10);
	
	//incremental id for blobs	
	QString var;
	var = QString("%1")
		.arg((long)ids_in_system);
	head.append(var);
	return head;
}

/**
	This function give us an id of the form __data_x as QString. This function is in charged of
	not generating id that alreay in the database.
*/
QString
generate_id(){

	QString new_id;
	new_id = generate_random_id();//actually it is incremental so far
	while(existing_id(new_id.toLocal8Bit().data())>=0){
		new_id = generate_random_id();
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
	ident = generate_id();//this is already in charge of giving as an unusued or reserved id
	audio_data_db[ids_in_system].id = ident;
	audio_data_db[ids_in_system].active = 1;
	audio_data_db[ids_in_system].data = data;
	ids_in_system++;
	return ident.toLocal8Bit().data();
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
	int pos = 0;
	pos = existing_id(ident);

	if(pos<0) return -1; //no id.
	
	if(audio_data_db[pos].active == 1) return -2; //the id was already taken
	audio_data_db[pos].active = 0;
	if(is_audio_blob(blob)==0){
		audio_data_db[pos].data = audio_blob_to_pointer(blob);
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
	int pos = 0;
	pos = existing_id(ident);
	if(pos < 0){
		 return pos;//no id. -1
	}
	else if(audio_data_db[pos].active == 0){
		return 0; //not active
	}
	else{		
		pointer_to_audio_blob(audio_data_db[pos].data, blob);//unifies blob. 
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
*/
int 
existing_id(const char* ident)
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
*/
int
reserve_id(const char *ident)
{
	if(existing_id(ident)<0){

		if(ids_in_system == MAX_AUDIO_ID_DB){
			std::cerr<<"more blobs than possible"<<std::endl;
			return -2;
		}		
		audio_data_db[ids_in_system].id = ident;
		audio_data_db[ids_in_system].active = 0;
		ids_in_system++;
		std::cerr<<ident<<" reserved"<<std::endl;
		return 0;

	}else{
		return -1;	
	}

}

/**
	Delete the data for the id and desactive it. This is the way we are going to use to free space in memory!!! IMPORTANT!!!
	
	Just with a prolog version of this we do clean_data(ID), we delete the raw data in memory. The blobs used to wrap this data will be
	garbage-collected at some point. This new implementation manages far better the memory resources
*/
int
clean_data_for_id(const char* ident){

	int pos = 0;
	pos = existing_id(ident);

	if(pos<0) return -1; //no id.
	
	if(audio_data_db[pos].active == 0) return -2;//already desactived

	audio_data_db[pos].active = 0;
	
	delete audio_data_db[pos].data;
	audio_data_db[pos].data = 0;//reset the pointer

	//What happens to the reference to the blob once we have deleted the data?????
	std::cerr<< ident <<" free"<<std::endl;
	return 0;
}

/**
	Returns the number of id in the database
*/
size_t
ids_in_db()
{
	return ids_in_system;
}

}




