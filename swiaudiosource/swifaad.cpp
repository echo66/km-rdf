/** * SWI-Prolog external interface to FAAD2 library for decoding of AAC audio files. These interface will decode the following audio types:
	-M4A
	-AAC
 * This interface is part of the module swiaudiosource created for the Knowledge Machine (Yves Raidmon) at the c4dm, Queen Mary Universtiy of London.
 * David Pastor Escuredo 2007, c4dm, Queen Mary University of London
 *
 */

#include <swiaudioblob.h>
#include <blobid.h>

#include <sys/stat.h>
#include <sys/mman.h>

#include <fcntl.h>
#include <qstring.h>
#include <vector>
#include <iostream>

#include <faadDecode.h> 

using namespace std;
						/**********************************************						 * Prototypes of the foreign language interface
						 *********************************************/

QString
fdpl_get_file_name(const char*);//extracts the name as QString from the path given as input of the predicate

unsigned char *
fdpl_get_input_frame(void *, size_t, size_t );//gets a frame of aac file data to pass it to the decoder

			
						/************************************						 * Variables: audio file, Faad reader					 			                                 ***********************************/

/*
 * Audio file data
 */

QString audioAAC_name;

/* 
 * Reader: This structure contains the necessary fields of data to set, run and obtain the results of the decoding process.
 * The attributes: channel_count
		   sample_rate
		   samples_channel 
		   ch1_pcm (vector *)
		   ch2_pcm (vector *)
 * are in common for every decoding interface and are present in the PCM functor
 */

static struct FaadReader{

	size_t channel_count;	//number of channels
	size_t sample_rate;	//sample rate
	size_t samples_channel;	//number of samples of each channel	

	//PCM
	std::vector<float> *ch1_pcm;//pcm data for both channels
	std::vector<float> *ch2_pcm;

}faad_reader;//instance of the structure used in the interface


					/****************************************************************************					 * Prolog predicates used in the swiaudiosource module (C++/Prolog interface)
					 ***************************************************************************/

/*
 * Main predicate of the interface. This predicate runs a FAAD decoder over the given audio file path setting the data of faad_reader.
 * This predicate allows the decoding of AAC audio files.
 * After calling this predicate we can find the extracted pcm and the data in the fad_reader
 */

PREDICATE(fdpl_decode, 1)
{
	//+File path
	//-MO::signal
	faad_reader.sample_rate = 0;
	faad_reader.channel_count = 0;
	faad_reader.samples_channel = 0;

	//init pcm data
	//init vectors: They must be deleted at the end!!
	faad_reader.ch1_pcm = new std::vector<float>();
	faad_reader.ch2_pcm = new std::vector<float>();

	//retrieve file and map in memory
	audioAAC_name = fdpl_get_file_name((const char *)A1); 

	//check if the file is mp4
	int result;
	int def_srate = 0;
    	int mp4file = 0;
    	unsigned char header[8];
        FILE *hMP4File;

	/* point to the specified file name */
  	hMP4File = fopen((const char *)A1 , "rb");
    	if (!hMP4File){
		
		cerr<<"Failed to open file"<<endl;
       		return false;
    	}
    	fread(header, 1, 8, hMP4File);
    	fclose(hMP4File);
    	if (header[4] == 'f' && header[5] == 't' && header[6] == 'y' && header[7] == 'p'){ mp4file = 1;}

  	if (mp4file == 1){

		std::cerr<<"mp4"<<std::endl;
       		result = fdpl_decode_MP4((const char *)A1, faad_reader.channel_count, faad_reader.sample_rate, faad_reader.samples_channel, faad_reader.ch1_pcm, faad_reader.ch2_pcm);
    	}
  	else{

		std::cerr<<"aac"<<std::endl;
     	   	result = fdpl_decode_AAC(def_srate, (const char *)A1, faad_reader.channel_count, faad_reader.sample_rate, faad_reader.samples_channel, faad_reader.ch1_pcm, faad_reader.ch2_pcm);
   	}

    	return true;	
}

/*
 * Could be called at the end (try to remove)
 */

PREDICATE(fdpl_reset, 0)
{
	faad_reader.channel_count = 0;
	faad_reader.samples_channel = 0;
	faad_reader.sample_rate = 0;
	
	//Delete pointers
	delete faad_reader.ch1_pcm;
	delete faad_reader.ch2_pcm;
	faad_reader.ch1_pcm=0;
	faad_reader.ch2_pcm=0;

	return TRUE;
}

/***************************************************
 *************** DECODED AUDIO DATA ****************
 **************************************************/	

/*
 * Returns the whole decoded audio as MO::signal element (Check out swimo.h and swimo.cpp in /swilib)+
 * Returning the name without path??
 */

PREDICATE(fdpl_get_decoded_signal, 1)
{	
	//-MO::signal	
	
	term_t sr = PL_new_term_ref();
	term_t pcm1 = PL_new_term_ref();
	term_t pcm2 = PL_new_term_ref();
	
	PL_put_integer(sr, faad_reader.sample_rate);
	PlTerm pcm;
	PlTail pcm_list(pcm);
	
	//The pcm data is returned by a pointer to it and not the data itself as it overflows prolog memory and is not efficient.
	//The pointer is returned by its ID

	//CHANGE THIS TO SUPPORT MORE CHANNELS

	if(faad_reader.channel_count>0){
		pcm1 = term_t(PlTerm(PlAtom(BLOBID::assign_data_id(faad_reader.ch1_pcm))));
		pcm_list.append(pcm1);
		if(faad_reader.channel_count==2){			
			pcm2 = term_t(PlTerm(PlAtom(BLOBID::assign_data_id(faad_reader.ch2_pcm))));
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
fdpl_get_file_name(const char *path){

	QString qpath(path);
	return qpath.section("/",-1);
}


