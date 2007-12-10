/*
 * Source to deal with the blobs and their ids in order to hide <#...> prolog terms.
 * David Pastor 2007, c4dm, Queen Mary, University of London
 *
 * Note: that library only is valid during a session of the km, loosing the id in the database afterwards.
 */

#include <blobid.h>
#include <iostream>
#include <cstdlib>

/***** Variables ******/

/*
 * This structure associates an id to each blob in the system (a pointer to a vector of data)
 */
static struct BlobIdRecord{

	/* id: __blob_id */
	QString id;
	/* AudioBlob containing a pointer to raw data in memory */
	term_t blob;
	/* flag 
		0 the id has been created but the blob has been delted, we keep the id to use if afterwars
		1 id with associated blob in memory
	*/
	int active;
}

/*
 * Database with all the entries id/blob
 */

blob_id_db[MAX_BLOBS];

/*
 * The number of ids created and stored in the database. useful to not check till MAX_BLOBS. Note that once an id is created it won't be deleted.
 * Note that may be id only reserved but without a blob
 */
size_t blobs_id_in_system;


/**** Implementation of functions *****/


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
	for(size_t i=0; i<blobs_id_in_system; i++){
		
		if(input.compare(blob_id_db[i].id)==0) {//this check should be correctly defined
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
	We split this up from generate_free_id to make the code clearer
*/
QString 
generate_random_id(){

	QString head("__blob_");

	srand((unsigned)time(0)); 
    	size_t random; 
   	size_t lowest=1, highest=MAX_BLOBS; 
   	size_t range=(highest-lowest)+1; 
   	random = lowest+size_t(range*rand()/(RAND_MAX + 1.0)); 
	QString var((long)random, 10);
	head.append(var);
	return head;
}

/**
	This function give us an id of the form __blob_x as QString. The x may vary randomly between 1 and MAX_BLOBS. This function is in charged of
	not generating id that alreay in the database.
*/
QString
generate_id(){

	QString new_id;
	new_id = generate_random_id();
	while(existing_id(new_id.toLocal8Bit().data())>=0){
		new_id = generate_random_id();
	}
	return new_id;
} 

/**
	Checks the status of the id. Returns:
	
	        -1 no exists
		0 exists but there is no blob associated in memory
		1 exists and the id is actively naming a blob in memory
*/
int
active_id(const char *ident)
{
	int flag = -1;
	int pos = 0;
	QString input(ident);
	for(size_t i=0; i<blobs_id_in_system; i++){
		
		if(input.compare(blob_id_db[i].id)==0) {//this check should be correctly defined
			flag = 0;
			pos = i;
		}	
	}
	if(flag == 0){
		return blob_id_db[pos].active;
	}
	return flag;
}

/**
	Returns the blob for the given id unifying with the term. Returns:
		
		-1 if no id in the database. The term is not unified
		0  if the id is non active. The term is not unified
		1 if active and the term is unified with the blob
*/
int 
get_blob_from_id(const char *ident, term_t blob)
{
	int pos = 0;
	pos = existing_id(ident);
	
	if(pos < 0){
		 return pos;//no id. -1
	}
	else if(blob_id_db[pos].active == 0){
		return 0; //not active
	}
	else{
		blob = blob_id_db[pos].blob;//unifies blob
		return 1; //was active
	}
}

/**
	This function reserves an id in the database even when there is no blob for it, setting active to 0. Returns 0 if success and negative if fails
*/
int
reserve_id(const char *ident)
{
	if(existing_id(ident)<0){

		if(blobs_id_in_system == MAX_BLOBS){
			std::cerr<<"more blobs than possible"<<std::endl;
			return -2;
		}		
		blob_id_db[blobs_id_in_system].id = ident;
		blob_id_db[blobs_id_in_system].active = 0;
		blobs_id_in_system++;
		std::cerr<<ident<<" reserved"<<std::endl;
		return 0;

	}else{
		return -1;	
	}

}

/**
	Unifies the blob for the given id unifying with the term. This is thought for the case when the id is reserved and we want then to 
	set the blob for it. You really don't set the blob and the id at the same time as it is generated randomly. Returns:
		
		-1 if no id in the database. The term is not unified
		-2 if the blob was areadly active
		 0 if active and the blob is stored
*/
int 
set_blob_for_id(const char *ident, term_t tblob)
{
	int pos = 0;
	pos = existing_id(ident);

	if(pos<0) return -1; //no id.
	
	if(blob_id_db[pos].active == 1) return -2; //the id was already taken for a blob!!

	blob_id_db[pos].active = 0;
	blob_id_db[pos].blob = tblob;
	return 0;
}

/**
	Initial function of the database. Given a blob, we generate and id and store the record in the database setting it active.
	We return the id to be handled. It's user's responsability the correct use of this.
	If we need some other id to be reserved and not used by this blob. WE MUST RESERVE THEM FIRST!!!
	It doesn't really check if we are out of the database range (user's problem)
*/
const char * 
assign_blob_id(term_t tblob)
{
	QString ident;
	ident = generate_id();//this is already in charge of giving as an unusued or reserved id
	blob_id_db[blobs_id_in_system].id = ident;
	blob_id_db[blobs_id_in_system].active = 1;
	blob_id_db[blobs_id_in_system].blob = tblob;
	blobs_id_in_system++;
	return ident.toLocal8Bit().data();
}

/**
	Returns the number of id in the database
*/
size_t
ids_in_db()
{
	return blobs_id_in_system;
}

/**
	Desactive an id when the blob is deleted from memory. The data itself must be deleted from the specific blob library!!!!
	We would have to do this in prolog:
	clean_blob(ID):-
		blob_for_id(ID, BLOB),
		delete(BLOB),
		desactive(ID).

	NOTE: what do we have to do with BLOB_ID.blob term reference????
*/
int
desactive_id(const char* ident){

	int pos = 0;
	pos = existing_id(ident);

	if(pos<0) return -1; //no id.
	
	if(blob_id_db[pos].active == 0) return -2;//already desactived

	blob_id_db[pos].active = 0;

	//What happens to the reference to the blob once we have deleted the data?????
	std::cerr<< ident <<" free"<<std::endl;
	return 0;
}

/**
	Another way to do the same but passing the index as argument
*/
int
desactive_id(size_t pos){

	if(pos<0){
		return -1; //incorrect position
	}
	else if(blob_id_db[pos].active == 0) {
		return -2;//already desactived
	}
	else{
		blob_id_db[pos].active = 0;

		//What happens to the reference to the blob once we have deleted the data?????
		std::cerr<< pos <<" record free"<<std::endl;
		return 0;
	}
}






