/**
	This is a library to deal with framing from Prolog. Can be used from many modules as long as the load the foreign library framepl.so
	Basically, this library defines a main predicate to retrieve a frame specifying the starting point of the frame and the size of it. Thus, it 
	can be called using StepSize and BlockSize arguments from Prolog to perform the necessary framing over the signal	
	David Pastor 2007 for c4dm, Queen Mary, University of London

	Modification: we only pass data. The functor analysis is done in Prolog code. Support for n channels
*/

#include <swiaudioblob.h>
#include <swimo.h>
#include <blobid.h>

#include <vector>

using namespace std;

/** * Prototype of a function to select a frame. This function should have constraints for general frames.
 */

vector<float> *
select_frame(size_t , size_t , vector<float> *);//selects a frame of the pcm of the channel passed given the starting and ending samples 


					/****************************************************
					 * Prolog predicates to perform framing from prolog *
					 ***************************************************/
/* * Gets a list of data pcm and frames it according to the parameters provided returning a list of n frames. I can even do this from Prolog
 */

PREDICATE(frame_for_signal, 4){

	//+signal data
	//+start (=+StepSize)
	//+size (BlockSize)
	//-frame data

	//getting input. We accept a list of n data elements

	PlTerm signal(A1);
	size_t start = (size_t)(long)A2;
	size_t size = (size_t)(long)A3;

	PlTail list(signal);
	PlTerm data;

	PlTerm frames;
	PlTail flist(frames);

	//data is sort of '__data_x'
	while(list.next(data)){

		char *id;
		PL_get_atom_chars(data, &id);

		//Now we retrieve the pointers to the raw data in memory
		vector<float> *ch;
		if(DataID::get_data_for_id((const char *)id, ch)<=0) return false;

		term_t f_id = PL_new_term_ref();//new ids for frame data
		f_id = term_t(PlTerm(PlAtom(DataID::assign_data_id(select_frame(start, start+size-1, ch)))));	

		flist.append(PlAtom(PlTerm(f_id)));
	}
	flist.close();
	return A4 = PlTerm(frames);
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






















