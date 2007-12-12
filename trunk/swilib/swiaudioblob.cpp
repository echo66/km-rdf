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

}









