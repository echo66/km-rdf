/**
	This source defines an interface to the blobid library in /swilib/ to handle blob with ids and manage the database storing the records 
	(id, blob, active).
	Thus, we get rid of <#...> blob terms in querying processes and we can represent each blob containing data as a node __blob_x.
	We basically wrap blobid functions that may be needed from prolog
	David Pastor 2007 for c4dm, Queen Mary, University of London
*/

#include <blobid.h>

/*
 *	This predicate assigns an id for the blob for the first time and stores the data in the blob_id_db.
 *	WE DON'T RETRIEVE IDs FROM BLOBS EVER!! WE GENERATE THE ID AND FROM THERE ON IS THE ACCURATE WAY TO HANDLE THE BLOB!!!
 *	Note that is unlikely to use this predicate in prolog as the blob should be created in some other module creating and returning its id.
 */

PREDICATE(new_blob_id, 2){

	//+New blob
	//-generated id

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));

	const char *id;
	id = BlobID::assign_blob_id(blob);
	//for here one we have an entry in the database for this id that won't be deleted till the end of the session and may be active or not

	return A2 = PlAtom(id);//we handle id in prolog as atoms
}

/*
 *	This predicate SETS a blob in the database for the specific id given. The id must previously exists and not be already active!! otherwise it 
 *      fails. This is interesting when we have desactived an id previously and we load the blob again assigning it its old id.
 */

PREDICATE(blob_id, 2){

	//+id in the database
	//+blob to unify

	//getting both arguments
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A2));
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	if(BlobID::set_blob_for_id((const char*)id, blob)<0){
		return false;
	}else{
		return true;
	}
}
/*
 *	This predicate works giving us the blob represented by the id. Success if id exists and is active and fails otherwise 
 */

PREDICATE(id_blob, 2){

	//+id
	//-data in blob (pointer to raw data in memory)
	
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	term_t blob = PL_new_term_ref();
	if(BlobID::get_blob_from_id((const char *)id, blob)>0){
	
		return A2 = PlTerm(blob);
	}else{
		return false;
	}
}

/*
 *	This predicate reserves an id in the database for a known blob (that is not stored yet) and sets it to non active
 */

PREDICATE(reserve_id, 1){

	//+id to reserve

	if(BlobID::reserve_id<0){
		return false;
	}else{
		return true;
	}
}

/*
 *	This predicate tells us if an id is a blob or not. It doesn't matter if the id is active or not. We just want to know if our identifier is
 *	is used for a blob. In our new management of blobs, this mainly means blob(BLOB) as we just need to check the database for that. It's users
 *	responsability and program correct working not to store in the database things that are not blobs!!!
 *	However we always can do check the real blob <#...> using a specific predicate of the library where it was created.
 *	It also outputs the index in the database in case it needs to be handled
 */

PREDICATE(is_blob_id, 2){

	//+id
	//-index
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	int index = BlobID::existing_id((const char *)id);
	if(index<0) return false;
	
	return A2 =PlTerm((long)index);
}

/*
 *	If the id exists and is active the predicate returns true. The activation of an id can not be done manually, it is automatic when we 
 *	re-unify a new blob in the database for a specific id
 */

PREDICATE(active_id, 1){

	//+id
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	if(BlobID::active_id((const char *)id)>0) return true;

	return false;
}

/*
 *	This predicate desactives an id once the blob referenced has been deleted. Fails if it didn' exist or it was already desactived
 */

PREDICATE(desactive_id, 1){

	//+id
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	if(BlobID::desactive_id((const char *)id)<0){

		return false;
	}else{
		return true;
	}
}

/*
 *	The same using the index of the record in the database
 */

PREDICATE(desactive_entry, 1){

	//+index of the record

	if(BlobID::desactive_id((long)A1)<0){

		return false;
	}else{
		return true;
	}
}


