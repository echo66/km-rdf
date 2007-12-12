/**
	This source defines type conversion between blobs (pointers and raw data) and prolog lists and some other stuff about AudioBlob (see swilib)
	This source handles the AudioBlob with prolog, but should be hidden and used through and id reference of the blobs instead
	The id should be __blob_x form and the assignment is done by blobid stuff in this swiaudiodata
	David Pastor 2007 for c4dm, Queen Mary, University of London
*/

#include <swiaudioblob.h>

/*
	Gets an audioblob containing the data itself (not point to data!!!!) and writes each value on a prolog list
	Do the other way around
*/ 

PREDICATE(vectorBlob_to_list, 2)
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

/*

   Gets the data pointed by the blob and writes it in a prolog list

*/

PREDICATE(pointerBlob_to_list, 2)
{
	//+ reference to blob
	//- list
	
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	std::vector<float> *data;
	data = AudioDataConversion::audio_blob_to_pointer(blob);

	PlTerm list;
	PlTail tail(list);

	for(size_t j=0; j<data->size(); j++){

		tail.append((double)data->at(j));
	}
	tail.close();	
	return A2 = list;

}

/*
	Reads a prolog list and stores the elements in a vector which will be pointed by the blob
	NOTE: there is no predicate to read a prolog list a put in a raw data blob
*/

PREDICATE(list_to_pointerBlob, 2)	
{

	//+list
	//-Blob

	PlTerm e;
	PlTail tail(A1);

	std::vector<float> *data;
	data = new std::vector<float>;

	while(tail.next(e)){

		data->push_back((float)(double)e);
        }

	term_t blob = PL_new_term_ref();
        AudioDataConversion::pointer_to_audio_blob(data, blob);//unifies the pointer and the data

	return A2 = PlTerm(blob);//should check errors	
}

/*
	Deletes the vector inmemory pointed by the blob. We use clean_data/1 instead. This is not very useful
*/

PREDICATE(clean_pointedVector, 1)
{
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	std::vector<float> *data;
	data = AudioDataConversion::audio_blob_to_pointer(blob);
	delete data;//What about cleaning the blob itself
	
	return true;
}

/*
	Checks if the given prolog term is a blob. This doesn't work with ids so seems useless, but keeping it for rich functionality
*/

PREDICATE(is_audio_blob, 1)
{
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));

	int result = AudioDataConversion::is_audio_blob(blob);

	if(result == 0){
		return true;
	}
	else if(result == 1){
		return false;
	}else{
		return false;
	}
}


