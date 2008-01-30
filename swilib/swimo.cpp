/**
	This is a foreign library for prolog that basically defines and analyzes prolog functors as representations of the 
	Music Ontlogy objects/class.
	So the prolog interface matches semantically with the OWL definition of the Music Ontology (Yves Raimond, c4dm).
	David Pastor 2007 for c4dm at Queen Mary, University of London.
	
	ToDo: This works for mono and stereo signals, I should extend it to support n probably using an array of pcm ids.
*/ 

#include "swimo.h"

			/****************************************************************************************
			*******	Defining functors represeting music ontology entities or relationships **********
			****************************************************************************************/
namespace MO{

/**
	Functors 
*/

functor_t frame_t = PL_new_functor(PL_new_atom("Frame"), 4);//MO::frame
functor_t signal_t = PL_new_functor(PL_new_atom("Signal"), 4);//MO::signal
functor_t timestamp_t = PL_new_functor(PL_new_atom("Timestamp"), 2);//MO::timestamp
functor_t feature_t = PL_new_functor(PL_new_atom("Feature"), 3);//MO::feature

/**
	MO::frame Both ways are supported by this function:

	1. frame(+channels, +samplerate, +initpos, +pcm_ch1, +pcm_ch2, -frame_term)
	If frame_term is not a compound term, it creates a "Frame" compound term given: 	
        	-number of channels (look out as it has to be passed as term)
		-sample rate
		-position of first samples in the original sequence: This is the 
		-term referencing the pcm for channel 1
		-term referencing the pcm for channel 2 (may be empty)
	
	and unifies to frame_term obtaining:
										
					'Frame'(channels, sample rate,  initpos, [listOfPcm])

	For mono we just have one element in the list adn we can't have more than 3.

	NOTE: The length of the frame is not stored in here as it can be retrieved using the swiaudioblob library¡

	2. frame(-channels, -samplerate, -initpos, -pcm_ch1, -pcm_ch2, +frame_term) 
	This way it analyzes and extracts the data from the term if frame_term does refer to a compound term 
	(cheking that it is the correct name)
*/
void
frame(term_t channel_count, term_t sample_rate, term_t first_sample_pos, term_t pcm_ch1, term_t pcm_ch2, term_t frame_term){
	
	//now, frame_term is checked out. If the results !=0, it means that there is no compound term, so the term is created and unified to it
	if(PL_is_functor(frame_term, frame_t)==0){
		
		//list of pcm 
		term_t pcm = PL_new_term_ref();
		PL_put_nil(pcm);
		int channels;
		PL_get_integer(channel_count, &channels);
		if(channels==2){
			PL_cons_list(pcm, pcm_ch2, pcm);//we'd introduce first the ch2 to get (ch1, ch2) in the correct order
		}	
		PL_cons_list(pcm, pcm_ch1, pcm);	
		//Construct and unifies the term
		PL_cons_functor(frame_term, frame_t, channel_count, sample_rate, first_sample_pos, pcm);
	
	//otherwise the term is read
	}else{
		
		//should check name and arity for security
		PL_get_arg(1, frame_term, channel_count);//channels is read
		PL_get_arg(2, frame_term, sample_rate);
		PL_get_arg(3, frame_term, first_sample_pos);//initpos is read
		//reads the list of pcm blobs		
		term_t pcm = PL_new_term_ref();
		PL_get_arg(4, frame_term, pcm);
		PL_get_list(pcm, pcm_ch1, pcm);
		int channels;
		PL_get_integer(channel_count, &channels);
		if(channels==2){
			PL_get_list(pcm, pcm_ch2, pcm);//if there is one just channel, the term is not unified.
		}	
	}
}

/**
	MO:signal Both ways are supported by this function:

	1. signal(+channels, +samplerate, +samples/channel, +pcm_ch1, +pcm_ch2, -frame_term)
	If signal_term is not a compound term, it creates a "Signal" compound term given: 	
        	-number of channels (look out as it has to be passed as term)
		-term reference to sample rate
		-term reference to samples/channel
		-term reference to pcm for channel 1
		-term reference to pcm for channel 2 (may be empty)
	
	and unifies to signal_term obtaining:
										
					'Signal'(channels, samplerate, samples/channel, [listOfPcm])

	NOTE: The length of the frame is not stored in here as it can be retrieved using the swiaudioblob library¡

	2. frame(-channels, -samplerate, -samples/channel, -pcm_ch1, -pcm_ch2, +frame_term) 
	This way it analyzes and extracts the data from the term if signal_term does refer to a compound term 
	(cheking that it is the correct name)
*/
void
signal(term_t channel_count, term_t sample_rate, term_t samples_channel, term_t pcm_ch1, term_t pcm_ch2, term_t signal_term){

	//now, signal_term is checked out. If the results ==0, it means that there is no compound term, so the term is created and unified to it
	if(PL_is_functor(signal_term, signal_t)==0){

		//list of pcm 
		term_t pcm = PL_new_term_ref();
		PL_put_nil(pcm);
		int channels;
		
		PL_get_integer(channel_count, &channels);
		//std::cerr<<channels<<std::endl;
		if(channels==2){
			PL_cons_list(pcm, pcm_ch2, pcm);
		}	
		PL_cons_list(pcm, pcm_ch1, pcm);
			
		//Construct and unifies the term
		PL_cons_functor(signal_term, signal_t, channel_count, sample_rate, samples_channel, pcm);
	
	//otherwise the term is read
	}else{
		//should check name and arity for security
		PL_get_arg(1, signal_term, channel_count);//channels is read
		PL_get_arg(2, signal_term, sample_rate);//samplerate is read
		PL_get_arg(3, signal_term, samples_channel);//samples/channel read
		term_t pcm = PL_new_term_ref();
		PL_get_arg(4, signal_term, pcm);
		PL_get_list(pcm, pcm_ch1, pcm);
		int channels;
		PL_get_integer(channel_count, &channels);
		if(channels==2){
			PL_get_list(pcm, pcm_ch2, pcm);
		}
	}
}

/**
	MO:timestamp both ways are supported by this function:

	1. timestamp(+start, +duration, -timestamp_term)
	If timestamp_term is not a compound term, it creates a "timestamp" compound term given: 	
        	-starting time point (sec)
		-duration (sec)
		
	and unifies to timestamp_term obtaining:
										
					'timestamp'(start, duration)

	2. timestamp(-start, -duration, +timestamp_term) 
	This way it analyzes and extracts the data from the term if timestamp_term does refer to a compound term 
	(cheking that it is the correct name)
*/
void
timestamp(term_t start, term_t duration, term_t timestamp_term){

	//now, signal_term is checked out. If the results ==0, it means that there is no compound term, so the term is created and unified to it
	if(PL_is_functor(timestamp_term, timestamp_t)==0){

		//Construct and unifies the term
		PL_cons_functor(timestamp_term, timestamp_t, start, duration);
	
	//otherwise the term is read
	}else{
		//should check name and arity for security
		PL_get_arg(1, timestamp_term, start);		//start is read
		PL_get_arg(2, timestamp_term, duration);		//duration
	}
}

/**
	MO:feature both ways are supported by this function:
	
	1. feature(+type, +timestamp, +featureEvent, -feature)
	 	
        	-feature type
		-MO::timestamp. Can be the one for the frame or a specific one!!!
		-featureEvent: Encapsulates the values of the feature!!
		
	and unifies to feature_term obtaining:
										
					'feature'(type, MO::timestamp, Event)

	2. feature(-type, -timestamp, -featureEvent, +feature)
	This way it analyzes and extracts the data from the term if feature_term does refer to a compound term 
	(cheking that it is the correct name)
*/
void
feature(term_t type, term_t timestamp, term_t featureEvent, term_t feature_term){

	//now, signal_term is checked out. If the results ==0, it means that there is no compound term, so the term is created and unified to it
	if(PL_is_functor(feature_term, feature_t)==0){
		
		//Construct and unifies the term
		PL_cons_functor(feature_term, feature_t, type, timestamp, featureEvent);
		
	//otherwise the term is read
	}else{
		//should check name and arity for security
		PL_get_arg(1, feature_term, type);		
		PL_get_arg(2, feature_term, timestamp);	
		PL_get_arg(3, feature_term, featureEvent);			
	}
}


namespace GET{
/*
 * This function returns the sample rate given a term reference to a MO::frame or MO::signal
 */
float
sample_rate(term_t data){

	//Tests which term we have as input
	if(PL_is_functor(data, signal_t) !=0){
		term_t c = PL_new_term_ref(); 
		term_t sr = PL_new_term_ref();
		term_t samples = PL_new_term_ref();
		term_t ch1 = PL_new_term_ref();
		term_t ch2 = PL_new_term_ref();
		MO::signal(c, sr, samples, ch1, ch2, data);
		double isr;
		PL_get_float(sr, &isr);
		return (float)isr;
		
	}
	else if(PL_is_functor(data, frame_t) !=0){
		term_t c = PL_new_term_ref(); 
		term_t sr= PL_new_term_ref();
		term_t samples = PL_new_term_ref();
		term_t ch1 = PL_new_term_ref();
		term_t ch2 = PL_new_term_ref();
		MO::frame(c, sr, samples, ch1, ch2, data);
		double isr;
		PL_get_float(sr, &isr);
		return (float)isr;
	}
	else{
		std::cerr<<"Not expected input data"<<std::endl;
		return -1;
	}
}

/*
 * This function returns the channels of the audio data given a term reference to a MO::frame or MO::signal
 */
int
channels_count(term_t data){

	//Tests which term we have as input
	if(PL_is_functor(data, signal_t) !=0){
		term_t c = PL_new_term_ref(); 
		term_t sr = PL_new_term_ref();
		term_t samples = PL_new_term_ref();
		term_t ch1 = PL_new_term_ref();
		term_t ch2 = PL_new_term_ref();
		MO::signal(c, sr, samples, ch1, ch2, data);
		int ch;
		PL_get_integer(c, &ch);
		return ch;
		
	}
	else if(PL_is_functor(data, frame_t) !=0){
		term_t c = PL_new_term_ref(); 
		term_t sr = PL_new_term_ref();
		term_t initpos = PL_new_term_ref();
		term_t ch1 = PL_new_term_ref();
		term_t ch2 = PL_new_term_ref();
		MO::frame(c, sr, initpos, ch1, ch2, data);
		int ch;
		PL_get_integer(c, &ch);
		return ch;
	}
	else{
		std::cerr<<"Not expected input data"<<std::endl;
		return -1;
	}
}

/*
 * This function returns the samples_channel of the audio data given a term reference to a MO::signal  
 * This value should be interpreted as the length of a PCM vector of a frame or signal
 */
long
samples(term_t data){

	//Tests which term we have as input
	if(PL_is_functor(data, signal_t) !=0){
		term_t c = PL_new_term_ref(); 
		term_t sr = PL_new_term_ref();
		term_t samples = PL_new_term_ref();
		term_t ch1 = PL_new_term_ref();
		term_t ch2 = PL_new_term_ref();
		MO::signal(c, sr, samples, ch1, ch2, data);
		long sc;
		PL_get_long(samples, &sc);
		return (size_t)sc;		
	}
	else{
		std::cerr<<"Not expected input data"<<std::endl;
		return -1;
	}
}



/*
 * Gets the first_sample position from MO::frame. This is a very important parameter to get the timestamp of the frame.
 */
long
first_sample(term_t data){

	if(PL_is_functor(data, frame_t) !=0){
		term_t c = PL_new_term_ref(); 
		term_t sr = PL_new_term_ref();
		term_t initpos = PL_new_term_ref();
		term_t ch1 = PL_new_term_ref();
		term_t ch2 = PL_new_term_ref();
		MO::frame(c, sr, initpos, ch1, ch2, data);
		long first_sample;
		PL_get_long(initpos, &first_sample);
		return (size_t)first_sample;
	}
	else{
		std::cerr<<"Not expected input data"<<std::endl;
		return -1;
	}
}

/**
	Gets the start point time of a MO::timestamp
*/
float 
start(term_t timestamp){

	if(PL_is_functor(timestamp, timestamp_t) !=0){

		term_t start = PL_new_term_ref();
		term_t duration = PL_new_term_ref();
		MO::timestamp(start, duration, timestamp);
		double start_point;
		PL_get_float(start, &start_point);
		return (float)start_point;
	}else{
		std::cerr<<"Not expected input data"<<std::endl;
		return -1;
	}
}

/**
	Gets the duration in time of a MO::timestamp
*/
float 
duration(term_t timestamp){

	if(PL_is_functor(timestamp, timestamp_t) !=0){

		term_t start = PL_new_term_ref();
		term_t duration = PL_new_term_ref();
		MO::timestamp(start, duration, timestamp);
		double d;
		PL_get_float(duration, &d);
		return (float)d;
	}else{
		std::cerr<<"Not expected input data"<<std::endl;
		return -1;
	}
}

}
}





