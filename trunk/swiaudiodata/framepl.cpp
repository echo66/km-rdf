/**
	This is a library to deal with framing from Prolog. Can be used from many modules as long as the load the foreign library framepl.so
	Basically, this library defines a main predicate to retrieve a frame specifying the starting point of the frame and the size of it. Thus, it 
	can be called using StepSize and BlockSize arguments from Prolog to perform the necessary framing over the signal	
	David Pastor 2007 for c4dm, Queen Mary, University of London

	Modification: we only pass data. The functor analysis is done in Prolog code. Support for n channels
*/

#include <swiaudioblob.h>
#include <blobid.h>

#include <vector>

using namespace std;

					/****************************************************
					 * Prolog predicates to perform framing from prolog *
					 ***************************************************/
/
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

						





















