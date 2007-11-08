/**
	This is a library that defines prolog predicates to deal with swimo.h functions and therefore with MO:: and MO::GET:: elements.
	It has been defined apart of swimo.h to get a tidier code.
	This library can be found at /swilib/mopl.so so can be used by other interfaces or any prolog module loading this foreign interface. 
	David Pastor 2007 for c4dm, Queen Mary, University of London
*/

#include <swiaudioblob.h>
#include <swimo.h>

/*
	Extracts the sample rate from a MO::signal or MO::frame
*/
PREDICATE(get_sample_rate, 2)
{
	term_t element = PL_new_term_ref();
	element = term_t(A1);
	float sr;
	sr = MO::GET::sample_rate(element);
	if(sr<0){return false;}
	else{return A2 = PlTerm((double)sr);}
}

/*
	Extracts the number of channels from a MO::signal or MO::frame
*/
PREDICATE(get_channels, 2)
{
	term_t element = PL_new_term_ref();
	element = term_t(A1);
	int channels;
	channels = MO::GET::channels(element);
	if(channels<0){return false;}
	else{return A2 = PlTerm((long)channels);}
}

/*
	Extracts the number of samples per channel of a MO::signal relying on MO::GET::samples_channel
	This is the actual length of the PCM data contained inside a signal 
*/
PREDICATE(get_samples_per_channel, 2)
{
	term_t element = PL_new_term_ref();
	element = term_t(A1);
	size_t samples_channel;
	samples_channel = MO::GET::samples_channel(element);
	if(samples_channel<0){return false;}
	else{return A2 = PlTerm((long)samples_channel);}
}

/*	TODO:
	Build a predicate for each argument in MO::elements
	Build a predicate to buld MO::elements from prolog
*/
