/**
	This is a library to mix down stereo signals to mono ones.	
	David Pastor 2008 for c4dm, Queen Mary, University of London
*/


#include <swiaudioblob.h>
#include <swimo.h>
#include <blobid.h>

#include <vector>

using namespace std;


/*******************************
****** FOREIGN PREDICATES ******
*******************************/

/**
	adpl_mix_stereo(+StereoSignal, -MonoSignal).
	*/

PREDICATE(adpl_mix_stereo, 2){

	//+stereo
	//-mono

	//getting input

	term_t signal = PL_new_term_ref();
	signal = term_t(PlTerm(A1));

	//setting variables of signal to be read
	term_t sample_rate = PL_new_term_ref();
	term_t channel_count = PL_new_term_ref();
	term_t samples_channel = PL_new_term_ref();
	term_t ch1_id = PL_new_term_ref();//gets the id for the channel raw data
	term_t ch2_id = PL_new_term_ref();

	MO::signal(channel_count, sample_rate, samples_channel, ch1_id, ch2_id, signal);//gets the parameters for signal (swimo.h)

	char *id1;//atom to const char *
	char *id2;
	PL_get_atom_chars(ch1_id, &id1);
	PL_get_atom_chars(ch2_id, &id2);

	vector<float> *ch1;
	vector<float> *ch2;
	if(DataID::get_data_for_id((const char *)id1, ch1)<=0){
		return false;
	}

	int channels;
	PL_get_integer(channel_count, &channels);

	if(channels == 1){ return true;} //not necessary to support more channels
	else if(channels == 2){
		if(DataID::get_data_for_id((const char *)id2, ch2)<=0){
			return false;
		}
		
		//MIXING
		cerr<<"mixing stereo"<<endl;
		vector<float> *mono;
		mono = new vector<float>;
		for(size_t j=0; j < ch1->size(); j++){

			mono->push_back((float)(ch1->at(j)+ch2->at(j))/(float)2);//test if it works.
		}
	
		//new signal
		term_t mono_t = term_t(PlTerm(PlAtom(DataID::assign_data_id(mono))));
		term_t empty = term_t(PlTerm(PlAtom("")));

		term_t mono_channel = PL_new_term_ref();
		int c = 1;
		PL_unify_integer(mono_channel, (long)c);

		term_t signalmono = PL_new_term_ref();
		MO::signal(mono_channel, sample_rate, samples_channel, mono_t, empty, signalmono);

		return A2 = PlTerm(signalmono);

	}else{
		return false; //we don't support >2 channels
	}
}
