/**
	Definition of functions for BLOBIDs. The input of these predicates must be a blobid as well as the output.
	The predicates retrieve the data pointed by the blobid's and operate with it. Results are also assigned an id if output.

	David Pastor 2008 for c4dm, Queen Mary, University of London

	TODO: put the functions in a proper C++ library?
*/


#include <swiaudioblob.h>
#include <blobid.h>

#include <vector>
#include <iostream>

using namespace std;

/* Prototypes */
vector<float> *
select_frame(size_t, size_t, vector<float> *);

/*******************************
****** FOREIGN PREDICATES ******
*******************************/

/**	
	blobs_mean(+Blobid1, +Blobid2, -MeanBlobid). Given 2 blobids
	*/

PREDICATE(mean_of_blobs, 3){

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

PREDICATE(get_blob_size, 2)
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

PREDICATE(are_equal_blobs, 3){

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
	blob_frame(+Blobid, +Start, +Size, -BlobidFrame)
	*/

PREDICATE(get_frame_of_blob, 4)
{
	//+blobid
	//+start
	//+size
	//-blobframe

	//getting input
	PlTerm ch1_id(A1);
	size_t start = (size_t)(long)A2;
	size_t size = (size_t)(long)A3;

	char *id1;//atom to const char *
	PL_get_atom_chars(ch1_id, &id1);

	vector<float> *ch1;
	if(BLOBID::get_data_for_id((const char *)id1, ch1)<=0){//this check is not really important if the rest of the system works well.
		return false;
	}
		
	return A4 = PlTerm(PlAtom(BLOBID::assign_data_id(select_frame(start, start+size-1, ch1))));
}

/**
	concat_blobs(+Blobid1, +Blobid2, -Blobid3)
	*/

PREDICATE(concat_of_blobs, 3)
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

/**
	blob_from_file(+FILEPATH, -BLOBID): saves the data (float * binary data) into a file given its path. The system provides a new id for the blob an returns it to the user.

	PROBLEM: what about if the user wants to specify the name of the blob? Should accept both ways


PREDICATE(blob_from_file, 2)
{

	//+File Path
	//+BlodId
	
	term_t blob = PL_copy_term_ref(term_t(PlTerm(A1)));
	//+id
	char *path;//A2
	term_t path_t = PL_new_term_ref();
	path_t = term_t(PlTerm(A2));
	PL_get_atom_chars(path_t,&path);

	//open the file to read
	FILE *fileData;
	fileData = fopen(path, "r");//binary file to read
		
	//retrieved data vector in memory
	vector<float> *data;
	data = new vector<float>();	

	char bin_datum[4];//size of a float in chars

	if(fileData!=NULL){
		int char_count = 0;
		char c;
		float *datum = 0;
		size_t test2 = 0;
		fseek(fileData, 0L, SEEK_END );
 		long final = ftell(fileData);
		//cerr<<"eof at "<<final<<endl;

		fseek(fileData, 0L, SEEK_SET);
		//read characters till the end of the file is reached getting the length of the data and a temporary buffer
		
		for(size_t r=0;r<final;r++){
			c=getc(fileData);
		 	bin_datum[char_count] = c;
			test2++;
			
			if(ferror(fileData)){
				cerr<<"error while reading"<<endl;
				cerr<<(const char*)c<<endl;
			}
			if(char_count==3){//when we read a whole float, we store it in the vector
				
				char *pointer = 0;
				pointer = &bin_datum[0];			
				datum = (float *)pointer;
				data -> push_back(*datum);
				char_count=0;
				
			}else{
				char_count++;
			}
	     	}
		//cerr<<test2<<endl;
		fclose(fileData); //we don't need to read the file anymore
	
		//returning the blob
		return A2 = PlTerm(PlAtom(BLOBID::assign_data_id(data)));
	}
	return false;
}


	blob_to_file(+BlodID, +FilePath)


PREDICATE(file_to_blob, 2)
{
	//+File Path
	//+Blob
	
	term_t blob = PL_copy_term_ref(term_t(PlTerm(A2)));
	//+id
	char *path;//A2
	term_t path_t = PL_new_term_ref();
	path_t = term_t(PlTerm(A1));
	PL_get_atom_chars(path_t,&path);
	
	//getting the data from the blob
	AudioVector *data;
	data = audio_blob_to_pointer(blob);//pointer to vector = data

	//open a file and write the data in it as stream of data
	//we don't store the real values, but the binary data as it is in memory which should make the progress faster

	size_t data_size;
	data_size = data -> size();//size of the vector

	//cerr<<data_size<<endl;

	FILE *fileData;
	fileData = fopen(filePath, "w");//binary file to write
		
	if(fileData!=NULL){
		//writing each float of the vector
		
		float *datum=0;
		char *binary_datum=0;
		size_t test = 0;
	
		for(size_t r=0; r<data_size; r++){

			datum = &(data -> at(r));//getting each datum of the vector and putting it in a pointer
			binary_datum = (char *)datum;//view as binary. 

			for(unsigned int l=0; l<sizeof(*datum); l++){//writing each float in binary

				fputc(*binary_datum, fileData);
				binary_datum++;
			}
			test = r;
		}
		fseek(fileData, 0L, SEEK_END );
 		long final = ftell(fileData);
		//cerr<<"eof at "<<final<<endl;
		//cerr<<test<<endl;
		fclose(fileData);
		return 0;
	}else{
		return -2;
	}
}
*/

//NEED TO MAKE LISTS...

/***********************************
******* C++  functions *************
***********************************/

/*
 * Returns an vector of floats that is a subvector of the pcm vector pointed by the third argument for the specified start and end samples
 * There are 2 basic constraints:
			-The frame can not start beyond the size of the signal. Checked before
			-The length of the frame is obtained with adding zeros if the final point is beyond the size of the signal 
 */

vector<float> *
select_frame(size_t start, size_t end, vector<float> *channel){	

	vector<float> *frame;
	frame = new vector<float>();
	size_t limit = channel-> size();
	for(size_t i=start; i<(end+1); i++){
		if(i < limit){
			
			frame->push_back(channel->at(i));			
		}else{
			frame->push_back(0.0f);//complete with 0 till fill the size of the frame queried
		}
	}
	
	return frame;
}

