/**
	This is a library for definition and analysis of Semantically meaningful prolog functors representing Music Ontology elements.
	Using this library is therefore possible to define MO::element in any foreign interface used as evaluation engine for the KM, as well as
	analyze them using MO::element (check source for description)
	David Pastor 2007 for c4dm, Queen Mary, University of London
*/

#ifndef _SWIMO_H_
#define _SWIMO_H_

#ifndef _FLI_H_INCLUDED
#include <SWI-cpp.h>
#endif

#include <iostream>
#include <stdio.h>

/**
	This namespace serves to define a domain described by the Music Ontology
*/
namespace MO{

/**
	Defines and analyzes MO:frame/4 
	'Frame'(NumberOfChannels, SampleRate, PositionOfFirstSample, [ListOfPCMBlobs])
	Check the source for more details
	NOTE: SHOULD I INCLUDE THE SIZE AS EXPLICIT PARAMETER???
*/
void
frame(term_t, term_t, term_t, term_t, term_t, term_t);

/**
	Defines and analyzes MO::signal/4. 
	'Signal'(NumberOfChannels, SampleRate, SamplesPerChannel, [ListOfPCMBlobs])
	Check the source for more details
*/
void
signal(term_t, term_t, term_t, term_t, term_t, term_t);

/**
	Defines and analyzes MO::timestamp/2. 
	'timestamp'(start, duration)
	Check the source for more details
*/
void
timestamp(term_t, term_t, term_t);

/**
	Defines and analyzes MO::Feature/3
	'Feature'(featureType, timestamp, FeatureEvent)
	Check source for details
*/
void
feature(term_t, term_t, term_t, term_t);

/**
	Namespace to read terms from the MO::elements and return them as C types
*/
namespace GET{

/**
	Gets the channels from MO::signal or MO::frame just by telling MO::GET::channels
*/	
int
channels_count(term_t);

/**
	Gets the samples_channel (length of PCM data) from MO::signal just by telling MO::GET::samples_channel
	In MO::signal this is obtained as direct parameter of the term
*/	
long
samples(term_t);

/**
	Gets the sample_rate from MO::signal or MO::frame
*/
float
sample_rate(term_t);

/**
	Gets the first_sample position from MO::frame. This is a very important parameter to get the timestamp of the frame (beginning of it).
*/
long
first_sample(term_t);

}
}

#endif
