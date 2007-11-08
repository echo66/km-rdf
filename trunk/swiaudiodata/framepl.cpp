/**
	This is a library to deal with framing from Prolog. Can be used from many modules as long as the load the foreign library framepl.so
	Basically, this library defines a main predicate to retrieve a frame specifying the starting point of the frame and the size of it. Thus, it 
	can be called using StepSize and BlockSize arguments from Prolog to perform the necessary framing over the signal	
	David Pastor 2007 for c4dm, Queen Mary, University of London
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
/* * Gets a MO::frame given the start and size of it. Doesnt allow frames where start is out of the limit of the channel. The timestamp is implicity 
 * stored in the MO::frame as the term contains info about samples and not about time.
 */

PREDICATE(get_frame, 4){

	//+MO::signal
	//+start (=+StepSize)
	//+size (BlockSize)
	//-MO::frame
	//getting input

	term_t signal = PL_new_term_ref();
	signal = term_t(PlTerm(A1));
	size_t start = (size_t)(long)A2;
	size_t size = (size_t)(long)A3;

	//setting variables of signal to be read
	term_t sample_rate = PL_new_term_ref();
	term_t channel_count = PL_new_term_ref();
	term_t samples_channel = PL_new_term_ref();
	term_t ch1 = PL_new_term_ref();//blobs containing the pointer to the data in memory
	term_t ch2 = PL_new_term_ref();
	MO::signal(channel_count, sample_rate, samples_channel, ch1, ch2, signal);//gets the parameters for signal (swimo.h)

	//Checks that the frame requested is within the signal passed
	long limit;
	PL_get_long(samples_channel, &limit);
	if(start >= (size_t)limit){
		return false;
	}
	
	//setting frame for writing
	term_t frame = PL_new_term_ref(); //the new MO::frame to return	
	term_t initpos = PL_new_term_ref();//we store the position of the first sample in the whole decoded signal  						   //(length can be extracted from the blob)
	PL_put_integer(initpos, start);
	term_t frame_ch1 = PL_new_term_ref();//blobs for the pcm frame selected for both channels
	term_t frame_ch2 = PL_new_term_ref();	
	AudioDataConversion::vector_to_audio_blob(select_frame(start, start+size-1, AudioDataConversion::audio_blob_to_pointer(ch1)), frame_ch1);
	
	//now the blob is not a pointer but the data itself
	int channels;
	PL_get_integer(channel_count, &channels);
	if(channels == 2){	
		AudioDataConversion::vector_to_audio_blob(select_frame(start, start+size-1, AudioDataConversion::audio_blob_to_pointer(ch2)), frame_ch2);
	}
	MO::frame(channel_count, sample_rate, initpos, frame_ch1, frame_ch2, frame);//swimo.h
	
	return A4 = PlTerm(frame);
}

/*
 * Gets the implicit MO::timestamp of a MO::frame. This is not a direct retrieval from a MO::frame term, so it is not placed in mopl.cpp (may change)
 * The timestamp obtained is a more detailed description than the vamp timestamp as this contains also the duration of the frame. Vamp does not c
 * contain duration as it may vary from features of the same frame
 *
 */
PREDICATE(get_frame_timestamp, 2){
	
	//+MO::frame
	//-MO::timestamp
	
	//Getting data
	term_t frame = PL_new_term_ref();
	frame = term_t(PlTerm(A1));
	term_t sample_rate = PL_new_term_ref();
	term_t channel_count = PL_new_term_ref();
	term_t initpos = PL_new_term_ref();
	term_t ch1 = PL_new_term_ref();//blobs containing the data of the frame
	term_t ch2 = PL_new_term_ref();
	MO::frame(channel_count, sample_rate, initpos, ch1, ch2, frame);//gets the parameters for MO::frame (swimo.h)
	
	long sr;
	PL_get_long(sample_rate, &sr);
	long init;
	PL_get_long(initpos, &init);
	
	term_t start = PL_new_term_ref();//beginning
	PL_put_float(start, (float)init/(float)sr);//just seconds

	//gets the number of samples for one channel (size of the frame).
	term_t duration = PL_new_term_ref();
	size_t size = AudioDataConversion::term_to_audio_vector(ch1).size();
	PL_put_float(duration, (float)size/float(sr));		
	
	term_t timestamp_term = PL_new_term_ref();
	MO::timestamp(start, duration, timestamp_term);
	
	return A2 = PlTerm(timestamp_term);
}

/*
 * This predicate calculates the number of frames that will be retrieved in the framing for the specific signal and the StepSize of the framing process
 */

PREDICATE(set_limit_framing, 3)
{
	//+samples per channel
	//+StepSize
	//-Limit
	
	long limit = long((long)A1/(long)A2);
	return A3 = PlTerm(limit);

}

						/***************************************************						 * Implementation of the foreign interface functions
						 **************************************************/

/*
 * Returns an vector of floats that is a subvector of the pcm vector pointed by the third argument for the specified start and end samples
 * There are 2 basic constraints:
			-The frame can not start beyond the size of the signal. Checked before
			-The length of the frame is obtained with adding zeros if the final point is beyond the size of the signal 
 */

vector<float>
select_frame(size_t start, size_t end, vector<float> *channel){	
	
	vector<float> frame;
	size_t limit = channel-> size();
	for(size_t i=start; i<(end+1); i++){
		if(i < limit){
			frame.push_back(channel->at(i));
		}else{
			frame.push_back(0.0f);//complete with 0 till fill the size of the frame queried
		}
	}
	return frame;
}






















