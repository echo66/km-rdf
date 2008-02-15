/**
	Audio data type conversion: AudioVector/AudioBlob
	David Pastor 2007
*/

#include "swiaudioblob.h"
#include <stdio.h>
#include <iostream>

namespace AudioDataConversion{
using namespace std;

						/***************************
						******** Variables *********
					        ***************************/
/**
	This blob is a general BLOB type for audio data like PCM, features...
	Write callbacks...
	PROBLEMS IF ATOMIC PREDICATES
*/
static PL_blob_t audio_blob = 
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
					/* unique representation for audio data*/
  "audio_blob",
  NULL,					/* release */
  NULL,					/* compare */
  NULL,					/* write */
  NULL,					/* acquire */
  NULL,					/* save load to/from .qlf files */
  NULL,
};

/**
	Sizes
*/
size_t size_AudioVector = 0;//number of elements of the vector
size_t size_AudioBlob = 0;//bytes in memory of the blob


						/***************************
						******** Functions *********
					        ***************************/

/**
	Initialization: Don't know if necessary
	-Register Blob type (all the times¿?)
	-set sizes to 0
*/
void
init_audio_blob(){

	size_AudioVector = 0;
	size_AudioBlob = 0; 
	//PL_register_blob_type(&audio_blob);//ref count ++?
}

/**
	Returns the size of the current AudioVector object 
*/
size_t
get_AudioVector_size(){
	
	return size_AudioVector;
}

/**
	Returns the size of the current AudioBlob object 
*/
size_t
get_AudioBlob_size(){

	return size_AudioBlob;

}

/**
	Unifies the new blob containing the data of AudioVector to the term AudioBlob.
	Returns the result of the unification
*/
int
vector_to_audio_blob(AudioVector data, AudioBlob blob){
	
	//it_audio_blob();//ref count ++?
	size_AudioVector = data.size();
	float data_array[size_AudioVector];
	for(size_t j=0; j<size_AudioVector; j++){
		data_array[j] = data[j];
	}	
	size_AudioBlob = sizeof(data_array);
	return PL_unify_blob(blob, &data_array, size_AudioBlob, &audio_blob);//ref count ++?? Can be collected¿¿??
}

/**
	Returns a vector containing the audio data from a term reference. NO TESTED!!
*/
AudioVector 
term_to_audio_vector(term_t t){

	PL_blob_t *type;        //type
	void *data;		//void pointer to data
	AudioVector vector;
	PL_get_blob(t, &data, &size_AudioBlob, &type);
	if(type != &audio_blob){
		throw PlException("Not Audio Data Blob");
	}
	size_AudioVector = size_AudioBlob/4;
	float* floats =	(float *)data;//hope it works
	for(size_t i=0; i<size_AudioVector; i++){
		vector.push_back(floats[i]);
	}
	return vector;
}

/**
	The same for a prolog atom
*/
AudioVector 
atom_to_audio_vector(atom_t a) { 
	
	return term_to_audio_vector(PlTerm(PlAtom(a)));
}

/**
	This time the AudioBlob represents a blob containing a pointer to the AudioVector. Sizes are not important in here....
*/
int
pointer_to_audio_blob(AudioVector *pointer, AudioBlob blob){

	//cerr<<"creating blob"<<endl;
	return PL_unify_blob(blob, (void **)&pointer, sizeof(pointer), &audio_blob);	
}	

/**
	Gets the pointer to the data. 
*/
AudioVector *
audio_blob_to_pointer(AudioBlob blob){

	PL_blob_t *type;        //type
	void *data;		//void pointer to data
	PL_get_blob(blob, &data, &size_AudioBlob, &type);
	if(type != &audio_blob){
		throw PlException("Not Audio Data Blob");
	}
	return *(AudioVector **)(data);
}

/**
	Checks if the blob is containing audio data or not
*/
int
is_audio_blob(term_t blob){

	PL_blob_t *type;  
	void *data;
	size_t size;
	PL_get_blob(blob, &data, &size, &type);
	if(type != &audio_blob){
		return 1;
	}
	return 0;
}

/**
	Dumps the data pointed by the blob into an external plain file. Returns 0 if suecceeds and negative if fails:
		-1 no audio blob
		-2 error to open file
	ToDo: Use buffering????
*/
int
dump_blob_data(term_t blob, const char* filePath){

	if(is_audio_blob(blob)==1) return -1;//no audio blob
	
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

/**
	Reads the data in the plain file, loads it in memory and stores the pointer in a blob
		
		 0 success
		-1 error

	NOTE: it has a limitation which i can't understand. The maximum data size we can in/out without truncation is 40000 samples. We can not store 
	therefore the whole signal without losses.
*/	
int
load_file_data(term_t blob, const char* filePath){

	//open the file to read
	FILE *fileData;
	fileData = fopen(filePath, "r");//binary file to read

		
	//retrieved data vector in memory
	AudioVector *data;
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
		if(pointer_to_audio_blob(data, blob)){ return 0;}
	}
	return -1;
}

}









