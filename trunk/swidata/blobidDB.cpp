/**
	This source defines an interface to the blobid library in /swilib/ to handle blob with ids and manage the database storing the records 
	(id, pointerToData, active).
	The revision of this source has been done to wrap the data referred by the IDs when it is query from prolog wrapping some of the functions
	of blobid.
	
	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
*/

#include <blobid.h>
#include <iostream>

/*
 *	This predicate reserves an id in the database for a known data object (that is not stored yet) and sets it to non active
 */

PREDICATE(reserve_data_id, 1){

	//+id to reserve
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);
	
	if(BLOBID::reserve_id(id)<0){
		return false;
	}else{
		return true;
	}
}

/*
 *	An id is a BLOBID if it exists in the running database of BLOBIDs
 */

PREDICATE(is_blobid, 2){

	//+id
	//-index
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	long index = BLOBID::existing_id((const char *)id);
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

	if(BLOBID::active_id((const char *)id)>0) return true;

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

	if(BLOBID::clean_data_for_id((const char *)id)<0) return false;

	return true;
}

/*
 * Checking the status
 */

PREDICATE(ids_in_db, 1){

	return A1 = PlTerm((long)BLOBID::ids_in_db());

}

PREDICATE(current_blob_id, 1){

	return A1 = PlTerm(PlAtom(BLOBID::current_id()));

}

PREDICATE(next_blob_id, 1){

	return A1 = PlTerm(PlAtom(BLOBID::next_id_to_assign()));

}

/**********************************************************
********************** DEPRECATED *************************
**********************************************************/

/*
 *	This predicate SETS a PL_blob_t in the BLOBID's database giving the id and the blob for the unification. We do not longer use Pl blobs, but we 
 * 	keep this.
 */

PREDICATE(plblob_to_blob, 2){

	//+id in the database
	//+blob to unify

	//getting both arguments
	term_t blob = PL_copy_term_ref(term_t(PlTerm(A2)));
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	if(BLOBID::set_plblob_for_id((const char*)id, blob)<0){
		return false;
	}else{
		return true;
	}
}

/*
 *	This predicate works giving us the blob wrapping the data reprenseted by the id. Success if id exists and is active and fails otherwise 
 */

PREDICATE(blob_to_plblob, 2){

	//+id
	//-data in blob (pointer to raw data in memory)
	
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	term_t blob = PL_new_term_ref();
	if(BLOBID::get_plblob_from_id((const char *)id, blob)>0){
	
		return A2 = PlTerm(blob);
	}else{
		return false;
	}
}

