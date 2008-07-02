/** * SWI-Prolog external interface to Sound File library (sndsoundfile). Audio file formats supported:
	-wav
	-aiff
	-aif
 * This interface is part of the module swiaudiosource created for the Knowledge Machine (Yves Raimond) at the c4dm, Queen Mary Universtiy of London.
 * David Pastor Escuredo 2007
 */

#include <swiaudioblob.h>
#include <blobid.h>

#include <qstring.h>
#include <iostream>
#include <vector>

#include <sndfile.h>

using namespace std;				
				
						 /*********************************************						 * Prototypes of the foreign language interface
						 *********************************************/

QString
sfpl_get_fileName(const char *);//extracts the name as QString from the path given as input of the predicate

void
sfpl_array_to_vector(float *, vector<float> *, size_t);


				        	/********************************************      
						*** Variables: audio file, SndFile reader ***
						********************************************/

/*
 * Audio file data
 */

QString wavFile_name;
SF_INFO wavFile_Info;//only used for the file formats supported by SndFile

/* 
 * Reader: This structure contains the necessary fields of data to set, run and obtain the results of the decoding process.
 * The attributes: channel_count
		   sample_rate
		   samples_channel 
		   ch1_pcm (pointer)
		   ch2_pcm (pointer)
 * are in common for every decoding interface and are present in the PCM functor
 */

static struct SndReader{

	size_t channel_count;	//number of channels
	size_t sample_rate;	//sample rate
	size_t samples_channel;	//number of samples of each channel	
	
	SNDFILE *m_sndFile;	//pcm extractor
	
	//pointers to pcm data
	vector<float> *ch1_pcm;
	vector<float> *ch2_pcm;	
	
}snd_reader;

					/****************************************************************************					 * Prolog predicates used in the swiaudiosource module (C++/Prolog interface)
					 ***************************************************************************/


/*
 * Main predicate of the interface. This predicate runs a SndFile decoder over the given audio file path setting the data of snd_reader.
 * This predicate allows the decoding of WAV, AIFF and AIF audio files.
 * After calling this predicate we can find the extracted pcm and the data in the snd_reader
 */

PREDICATE(sfpl_decode, 1)
{
	//+path to the audio file

	//init
	snd_reader.channel_count = 0;
	snd_reader.sample_rate = 0;
	snd_reader.samples_channel = 0;

	//init vectors: They must be deleted at the end!!
	snd_reader.ch1_pcm = new vector<float>();
	snd_reader.ch2_pcm = new vector<float>();

	const char *wavFile_path = (char *)A1;
	wavFile_name = sfpl_get_fileName(wavFile_path);

	//open the wav file with sndfile 
	snd_reader.m_sndFile = sf_open(wavFile_path, SFM_READ, &wavFile_Info);
	if(!snd_reader.m_sndFile){
		cerr<<"Failed to open file"<<endl;
		return false;
	}
	//setting
	if (wavFile_Info.channels > 0) {
	        snd_reader.samples_channel = wavFile_Info.frames;//MUST BE THE NUMBER OF SAMPLES!!!
		snd_reader.channel_count = wavFile_Info.channels;
	        snd_reader.sample_rate = wavFile_Info.samplerate;
		sf_count_t readCount = 0;
		//starting in sample 0
		if (sf_seek(snd_reader.m_sndFile, 0, SEEK_SET) < 0) {
        	     	cerr << "sf_seek failed" << endl;
        	     	return false;
	        }
		//There are different procedures to get the data for 1 channel or 2 channel files:
		//just one channel. We use directly the ch1_pcm to pass it to the sf_readf_float function
		if(snd_reader.channel_count==1){
	
			float* ch1_array;//pcm data for channel 1. This array passed to the decoder to store the data in runtime
			ch1_array = new float[snd_reader.samples_channel];
			//the whole file at once
			if((readCount = sf_readf_float(snd_reader.m_sndFile, ch1_array, snd_reader.samples_channel)) < 0) {
				cerr << "Failed to read wav file" << endl;
              			return false;
			}
			sfpl_array_to_vector(ch1_array, snd_reader.ch1_pcm, snd_reader.samples_channel);//puts the data into the vector
			delete[] ch1_array; //we not longer need the float array as we have the vector
			ch1_array=0;

		//two channels (we dont do more than 2 channels, hopefully we dont need to).			
		}else{
			float *buffer;
			buffer = new float[snd_reader.samples_channel*snd_reader.channel_count]; //two channels actually (read at the same time)
			if((readCount = sf_readf_float(snd_reader.m_sndFile, buffer, snd_reader.samples_channel)) < 0) {
				cerr << "Failed to read wav file" << endl;
              			return false;
			}
			//Splitting channels
			for(size_t c = 0; c< snd_reader.channel_count; c++){
				float *ch1_array;
				float *ch2_array;
				if(c==0){
					size_t j=0;					
					ch1_array = new float[snd_reader.samples_channel];
					while(j<readCount){
						
             					ch1_array[j] = buffer[j * snd_reader.channel_count + c];
               				 	++j;
            				}			
					//once the channel 1 is splitted up, we can store it in the vector
					sfpl_array_to_vector(ch1_array, snd_reader.ch1_pcm, snd_reader.samples_channel);
					delete[] ch1_array; //we not longer need the float array as we have the vector
					ch1_array=0;
						
				}else if(c==1){
					size_t j=0;				
					ch2_array = new float[snd_reader.samples_channel];
					while(j<readCount){
						
             					ch2_array[j] = buffer[j * snd_reader.channel_count + c];
               				 	++j;
            				}
					//once the channel 2 is splitted up, we can store it in the vector
					sfpl_array_to_vector(ch2_array, snd_reader.ch2_pcm, snd_reader.samples_channel);
					delete[] ch2_array; //we not longer need the float array as we have the vector
					ch2_array=0;
				}
				else{
					cerr<<"Unexpected number of channels: "<<c+1<<endl;
					return false;
				}
			}
			delete[] buffer;//Delete temporary buffer
			buffer = 0;
		}
  	}else{
		//Close snd file
		sf_close(snd_reader.m_sndFile);
		snd_reader.m_sndFile = 0;
		return false;//no data to read
	}
	//Close snd file
	sf_close(snd_reader.m_sndFile);
	snd_reader.m_sndFile = 0;
	return TRUE;	
}

/*
 * This will be at the very end. but hopefully can be removed after the implementation is done
 */

PREDICATE(sfpl_reset, 0)
{
	
	//Reset metadata
	snd_reader.channel_count = 0;
	snd_reader.sample_rate = 0;
	snd_reader.samples_channel = 0;

	//Reset decoded data in vectors
	delete snd_reader.ch1_pcm;
	delete snd_reader.ch2_pcm;
	snd_reader.ch1_pcm=0;
	snd_reader.ch2_pcm=0;
	return true;
}

/***************************************************
 *************** DECODED AUDIO DATA ****************
 **************************************************/	

/*
 * Returns the whole decoded audio as MO::signal element (Check out swimo.h and swimo.cpp in /swilib)+
 * Returning the name without path??
 */

PREDICATE(sfpl_get_decoded_signal, 1)
{	
	//-MO::signal	
	
	term_t sr = PL_new_term_ref();
	term_t pcm1 = PL_new_term_ref();
	term_t pcm2 = PL_new_term_ref();
	
	PL_put_integer(sr, snd_reader.sample_rate);
	PlTerm pcm;
	PlTail pcm_list(pcm);

	//The pcm data is returned by a pointer to it and not the data itself as it overflows prolog memory and is not efficient.
	//The pointer is returned by its ID

	if(snd_reader.channel_count>0){
		pcm1 = term_t(PlTerm(PlAtom(BLOBID::assign_data_id(snd_reader.ch1_pcm))));
		pcm_list.append(pcm1);
		if(snd_reader.channel_count==2){			
			pcm2 = term_t(PlTerm(PlAtom(BLOBID::assign_data_id(snd_reader.ch2_pcm))));
			pcm_list.append(pcm2);
		}
		pcm_list.close();
			
		A1 = PlTerm(pcm);
		A2 = PlTerm(sr);
		return true;
	
	}else {
		cerr<<"No signal"<<endl;
		return false;
	}
}

						/***************************************************						 * Implementation of the foreign interface functions
						 **************************************************/

/*
 * Function that extracts the name from the absolute path
 */

QString
sfpl_get_fileName(const char *path){

	QString qpath(path);
	QString qname = qpath.section("/",-1);
	return qname;
}

/*
 * Stores the array with the decoded audio in the vector specifying the number of samples to store.
 */

void
sfpl_array_to_vector(float *array, vector<float> *vector, size_t samples){

	for(size_t i=0; i<samples; i++){
		vector -> push_back(array[i]);
	}
	//cerr<<vector->size()<<endl;
}

/*
 * handles the input stream
 */

void
get_input_stream(term_t){

	//getting a prolog term which is a prolog stream.


}



