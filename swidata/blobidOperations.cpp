/**
	Definition of functions for BLOBIDs. The input of these predicates must be a blobid as well as the output.
	The predicates retrieve the data pointed by the blobid's and operate with it. Results are also assigned an id if output.

	David Pastor 2008 for c4dm, Queen Mary, University of London
*/


#include <swiaudioblob.h>
#include <blobid.h>

#include <vector>
#include <iostream>

using namespace std;


/*******************************
****** FOREIGN PREDICATES ******
*******************************/

/**	
	blobs_mean(+Blobid1, +Blobid2, -MeanBlobid). Given 2 blobids
	*/

PREDICATE(blobs_mean, 3){

	//+blob1
	//+blob2
	//-mean

	//getting input
	PlTerm ch1_id(A1);
	PlTerm ch2_id(A2);

	char *id1;//atom to const char *
	char *id2;
	PL_get_atom_chars(ch1_id, &id1);
	PL_get_atom_chars(ch2_id, &id2);

	vector<float> *ch1;
	vector<float> *ch2;
	if(BLOBID::get_data_for_id((const char *)id1, ch1)<=0){//this check is not really important if the rest of the system works well.
		return false;
	}

	if(BLOBID::get_data_for_id((const char *)id2, ch2)<=0){//we only check
			return false;
	}
		
	//MIXING
	std::cerr<<"mixing"<<std::endl;
	vector<float> *mono;
	mono = new vector<float>;
	for(size_t j=0; j < ch1->size(); j++){

		float mean = (float)ch1->at(j)+ch2->at(j);
		mean = mean /2.0;
		mono->push_back(mean);
			//cerr<<mean<<endl;
			//cerr<<ch1->at(j)<<endl;
			//cerr<<ch2->at(j)<<endl;
	}
	
	return A3 = PlTerm(PlAtom(BLOBID::assign_data_id(mono)));

}

/**
	blob_size(+Blobid, -Size). Returns the size of the blob
	*/

PREDICATE(blob_size, 2)
{

	PlTerm ch1_id(A1);

	char *id1;//atom to const char *

	PL_get_atom_chars(ch1_id, &id1);

	vector<float> *ch1;
	if(BLOBID::get_data_for_id((const char *)id1, ch1)<=0){//this check is not really important if the rest of the system works well.
		return false;
	}

	return A2 = PlTerm(double(ch1->size()));		
}

/**
	equal_blobs(+Blobid1, +Blobid2): Compares 2 blobs.
	*/

PREDICATE(equal_blobs, 3){

	//+blob1
	//+blob2

	//getting input
	PlTerm ch1_id(A1);
	PlTerm ch2_id(A2);

	char *id1;//atom to const char *
	char *id2;
	PL_get_atom_chars(ch1_id, &id1);
	PL_get_atom_chars(ch2_id, &id2);

	vector<float> *ch1;
	vector<float> *ch2;
	if(BLOBID::get_data_for_id((const char *)id1, ch1)<=0){//this check is not really important if the rest of the system works well.
		return false;
	}

	if(BLOBID::get_data_for_id((const char *)id2, ch2)<=0){//we only check
			return false;
	}
		
	if(*ch1==*ch2){
		return true;
	}
	return false;
}

/**
	concat_blobs(+Blobid1, +Blobid2, -Blobid3)
	*/

PREDICATE(concat_blobs, 3)
{
	//+blobid1
	//+blobid2
	//-blobid3

	//getting input
	PlTerm ch1_id(A1);
	PlTerm ch2_id(A2);

	char *id1;//atom to const char *
	char *id2;
	PL_get_atom_chars(ch1_id, &id1);
	PL_get_atom_chars(ch2_id, &id2);

	vector<float> *ch1;
	vector<float> *ch2;
	if(BLOBID::get_data_for_id((const char *)id1, ch1)<=0){//this check is not really important if the rest of the system works well.
		return false;
	}

	if(BLOBID::get_data_for_id((const char *)id2, ch2)<=0){//we only check
			return false;
	}
		
	vector<float> *data;
	data = new vector<float>();
	
	for(unsigned int j = 0; j<ch1->size(); j++){
		
		data->push_back(ch1->at(j));
	}

	for(unsigned int r = 0; r<ch2->size(); r++){
		
		data->push_back(ch2->at(r));
	}

	return A3 = PlTerm(PlAtom(BLOBID::assign_data_id(data)));

}

