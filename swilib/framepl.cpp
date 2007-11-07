/**
	This is a library to deal with framing from Prolog. Can be used from many modules as long as the load the foreign library framepl.so
	David Pastor 2007 for c4dm, Queen Mary, University of London
	ToDo: arrange select frame for a proper selection of the frame
*/

#include <swiaudioblob.h>
#include <swimo.h>

#include <vector>

using namespace std;

/** * Prototype of a function to select a frame. This function should have constraints for general frames.
 */

vector<float>
select_frame(size_t , size_t , vector<float> *);//selects a frame of the pcm of the channel passed given the starting and ending samples 


					/****************************************************
					 * Prolog predicates to perform framing from prolog *
					 ***************************************************/

/*
 * This one reads the MO:signal element and from the pcm pointed by the pointers of that element, it extracts a frame a returns an MO::frame element
 * which contains actual audio data.
 * Returning the name without path?? check that works fine for mono as well
 */

PREDICATE(get_frame, 4){

	//+MO::signal
	//+start
	//+end
	//-MO::frame

	//getting input
	term_t signal = PL_new_term_ref();
	signal = term_t(PlTerm(A1));
	size_t start = (size_t)(long)A2;
	size_t end = (size_t)(long)A3;

	//setting variables of signal to be read
	term_t sample_rate = PL_new_term_ref();
	term_t channel_count = PL_new_term_ref();
	term_t samples_channel = PL_new_term_ref();
	term_t ch1 = PL_new_term_ref();//blobs containing the pointer to the data in memory
	term_t ch2 = PL_new_term_ref();
	MO::signal(channel_count, sample_rate, samples_channel, ch1, ch2, signal);//gets the parameters for signal (swimo.h)
	
	//setting frame for writing
	term_t frame = PL_new_term_ref(); //the new MO::frame to return	
	term_t initpos = PL_new_term_ref();//we store the position of the first sample in the whole decoded signal  						   //(length can be extracted from the blob)
	PL_put_integer(initpos, start);
	term_t frame_ch1 = PL_new_term_ref();//blobs for the pcm frame selected for both channels
	term_t frame_ch2 = PL_new_term_ref();	
	AudioDataConversion::vector_to_audio_blob(select_frame(start, end, AudioDataConversion::audio_blob_to_pointer(ch1)), frame_ch1);
	//now the blob is not a pointer but the data itself
	int channels;
	PL_get_integer(channel_count, &channels);
	if(channels == 2){	
		AudioDataConversion::vector_to_audio_blob(select_frame(start, end, AudioDataConversion::audio_blob_to_pointer(ch2)), frame_ch2);
	}
	MO::frame(channel_count, sample_rate, initpos, frame_ch1, frame_ch2, frame);//swimo.h
	
	return A4 = PlTerm(frame);
}

						/***************************************************						 * Implementation of the foreign interface functions
						 **************************************************/

/*
 * Returns an vector of floats that is a subvector of the pcm vector pointed by the third argument for the specified start and end samples
 * ToDO!!!!!! ADD SOME CONSTRAINTS TO THE END and BEGINNING OF THE FRAME: ZERO PADDING AND SO ON
 */

vector<float>
select_frame(size_t start, size_t end, vector<float> *channel){	
	vector<float> frame;
	for(size_t i=start; i<(end+1); i++){
		frame.push_back(channel->at(i));
	}
	return frame;
}






















