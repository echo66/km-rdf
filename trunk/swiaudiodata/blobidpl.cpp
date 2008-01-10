/**
	This source defines an interface to the blobid library in /swilib/ to handle blob with ids and manage the database storing the records 
	(id, pointerToData, active).
	The revision of this source has been done to wrap the data referred by the IDs when it is query from prolog wrapping some of the functions
	of blobid.
	David Pastor 2007 for c4dm, Queen Mary, University of London
*/

#include <blobid.h>
#include <iostream>

/*
 *	This predicate SETS a blob in the database for the specific id given. The id must previously exists and not be already active!! otherwise it 
 *      fails. This is interesting when we have desactived an id previously and we load the blob again assigning it its old id.
 *	Now, we don't store the blob but the pointer to the raw data in it.
 */

PREDICATE(blob_id, 2){

	//+id in the database
	//+blob to unify

	//getting both arguments
	term_t blob = PL_copy_term_ref(term_t(PlTerm(A2)));
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	if(DataID::set_blob_for_id((const char*)id, blob)<0){
		return false;
	}else{
		return true;
	}
}
/*
 *	This predicate works giving us the blob wrapping the data reprenseted by the id. Success if id exists and is active and fails otherwise 
 */

PREDICATE(id_blob, 2){

	//+id
	//-data in blob (pointer to raw data in memory)
	
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	term_t blob = PL_new_term_ref();
	if(DataID::get_blob_from_id((const char *)id, blob)>0){
	
		return A2 = PlTerm(blob);
	}else{
		return false;
	}
}

/*
 *	This predicate reserves an id in the database for a known data object (that is not stored yet) and sets it to non active
 */

PREDICATE(reserve_id, 1){

	//+id to reserve
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);
	
	if(DataID::reserve_id(id)<0){
		return false;
	}else{
		return true;
	}
}

/*
 *	This is not related to existing blobs anymore and therefore it's less useful. Just checks if an id is taken or not
 */

PREDICATE(is_data_id, 2){

	//+id
	//-index
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	long index = DataID::existing_id((const char *)id);
	if(index<0) return false;
	
	return A2 =PlTerm((long)index);
}

/*
 *	If the id exists and is active the predicate returns true. The activation of an id can not be done manually.
 */

PREDICATE(active_id, 1){

	//+id
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	if(DataID::active_id((const char *)id)>0) return true;

	return false;
}

/*
 *	This predicate cleans the space in memory of the raw data specified by the ID. We may use it later on, so the ID stays
 */

PREDICATE(clean_data, 1){

	//+id
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	if(DataID::clean_data_for_id((const char *)id)<0) return false;

	return true;
}


