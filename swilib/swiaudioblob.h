/**
	This is a library for general type conversion of vector<float> to PL_blob_t. It has been created in order to store arbitrary large audio data 		objects defined as AudioVector and AudioBlob. We combine it with the namespace AudioID to get the raw data pointed by an ID as prolog blob
	David Pastor 2007 for c4dm, Queen Mary, University of London
*/

#ifndef _AUDIO_BLOB_H_
#define _AUDIO_BLOB_H_

#ifndef _FLI_H_INCLUDED
#include <SWI-cpp.h>
#endif
#include <vector>

/** 
    New namespace: AudioDataConversion
*/
namespace AudioDataConversion{
using namespace std;

						/**********************
						*******	Types *********
						**********************/

/**
	This is a vector containing general audio data: PCM, features...
	This library should support arbitrary large vectors.
*/
typedef vector<float> AudioVector;// we like C++ vectors because we can get the size easily

/**
	Term reference to an audio blob. This AudioBlob may wrap
		--binary data as it is stored in memory
		--pointers to data
*/
typedef term_t AudioBlob; //we like PL_blob_t because we can manage large binary data

						/**************************
						******* Prototypes ********
						**************************/

/**
	Wraps the binary audio data into a Blob. Returns the result of PL_unify_blob
*/
int
vector_to_audio_blob(AudioVector, AudioBlob);

/**
	Gets the audio data from the Blob (passed as term)
*/
AudioVector 
term_to_audio_vector(AudioBlob);

/**
	Gets the audio data from the Blob passed as atom
*/
AudioVector 
atom_to_audio_vector(atom_t);

/**
	Wraps a pointer to an AudioVector into an AudioBlob so we can deal with massive large vectors in memory without memory problems
*/
int
pointer_to_audio_blob(AudioVector *, AudioBlob);

/**
	Retrieves a pointer to an AudioVector stored in an AudioBlob
*/
AudioVector *
audio_blob_to_pointer(AudioBlob);

/**
	Retrieves the size of the AudioVector: number of samples, feature values... This is only meaninful when we wrap binary data itself 
*/
size_t
get_AudioVector_size();

/**
	Retrieves the size of the AudioBlob: number of bytes in memory of the float* representing the data. Only meaninful when storing binary data 		itself
*/
size_t
get_AudioBlob_size();

/**
	Initialization of the audio_blob. CHECK THIS OUT FOR BETTER GARBAGE-COLLECTION BEHAVOUR!!
	This hasn't been used
*/
void
init_audio_blob();

/**
	Checks if it is audio blob (0 yes, 1 not)
*/
int 
is_audio_blob(term_t);

/**
	Dumps the data pointed by the blob into an external plain file. The name of the file is the ID for the data (see BlobID)
*/
int
dump_blob_data(term_t, const char*);

/**
	Reads the data in the plain file, loads it in memory and stores the pointer in a blob
*/
int
load_file_data(term_t, const char*);

}

#endif

