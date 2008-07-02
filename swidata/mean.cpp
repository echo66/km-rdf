/**
	Definition of functions for blobids. The input of these predicates must be a blobid as well as the output
	David Pastor 2008 for c4dm, Queen Mary, University of London
*/


#include <swiaudioblob.h>
#include <swimo.h>
#include <blobid.h>

#include <vector>

using namespace std;


/*******************************
****** FOREIGN PREDICATES ******
*******************************/

/**	
	blobs_mean(+Blob1, +Blob2, -MeanBlob). Given 2 blobids
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
	if(DataID::get_data_for_id((const char *)id1, ch1)<=0){//this check is not really important if the rest of the system works well.
		return false;
	}

	if(DataID::get_data_for_id((const char *)id2, ch2)<=0){//we only check
			return false;
	}
		
	//MIXING
	cerr<<"mixing stereo"<<endl;
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
	
	return A3 = PlTerm(PlAtom(DataID::assign_data_id(mono))));

}
