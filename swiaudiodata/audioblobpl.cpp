/**
	David Pastor 2007 for c4dm, Queen Mary, University of London
*/

#include <swiaudioblob.h>

/*
	Gets an audioblob containing the data itself (not point to data!!!!) and writes each value on a prolog list
	Do the other way around
*/ 

PREDICATE(audioblob_to_list, 2)
{
	//+ reference to blob
	//- list

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	std::vector<float> data = AudioDataConversion::term_to_audio_vector(blob);
	
	PlTerm list;
	PlTail tail(list);

	for(size_t j=0; j<data.size(); j++){

		tail.append((double)data[j]);
	}
	tail.close();	
	return A2 = list;

}
