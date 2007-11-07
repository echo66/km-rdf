/** * ovpl: SWI-Prolog external interface to Vorbis encoded files in Ogg format files by using the FishSound and Oggz libraries. Format supported:
	-ogg audio file format containing Vorbis encoding 
 * This interface is part of the module swiaudiosource created for the Knowledge Machine (Yves Raimond) at the c4dm, Queen Mary Universtiy of London.
 * David Pastor Escuredo 2007, c4dm, Queen Mary, University of London.
 */

#include <swiaudioblob.h>
#include <swimo.h>

#include <qstring.h>
#include <iostream>
#include <vector>
#include <stdlib.h>

#include <oggz.h> 
//library for .ogg files (multimedia container, stream oriented)
#include <fishsound.h>
//library for decoding vorbis 

using namespace std;

						 /*********************************************						 * Prototypes of the foreign language interface
						 *********************************************/

QString
ovpl_get_fileName(const char *);//extracts the name from the path

static
int ovpl_store_frames(FishSound *, float **, long, void *);//sets the output of the decoding into the Vorbis Reader

static
int ovpl_read_packet(OGGZ *, ogg_packet *, long, void *);//reads the data contained into the OGG container (as ogg packets): Vorbis coded audio


						 /***************************************						 * Variables: audio file, Vorbis reader *
						 ***************************************/
/*
 * Audio file data	
 */

QString oggFile_name;
OGGZ *oggz;//oggz representation for reading

/* 
 * Reader: This structure contains the necessary fields of data to set, run and obtain the results of the decoding process.
 * The attributes: channel_count
		   sample_rate
		   samples_channel 
		   ch1_pcm (vector *)
		   ch2_pcm (vector *)
 * are in common for every decoding interface 
 */

struct VorbisReader{
	size_t channel_count;//number of channels
	size_t sample_rate;//sample rate
	size_t samples_channel;//number of samples of one channel or both?	

	FishSound *m_fishSound;//decoder

	vector<float> *ch1_pcm;//pcm data for both channels
	vector<float> *ch2_pcm;

}vorbis_reader;

				/****************************************************************************				 * Prolog predicates used in the swiaudiosource module (C++/Prolog interface)
				 ***************************************************************************/

/*
 * Main predicate of the interface. This predicate runs a OggVorbis decoder over the given audio file path setting the data of vorbis_reader.
 * This predicate allows the decoding of OGG audio files.
 * After calling this predicate we can find the extracted pcm and the data in the vorbis_reader.
 * NOTE: to process several file concurrently, this predicate MUST BE ATOMIC
 */

PREDICATE(ovpl_decode,1)
{
	//+path to audio file

	//init of ReaderData parameters. Necessary to set to 0 each time we process a new file
	vorbis_reader.channel_count = 0;
	vorbis_reader.sample_rate = 0;
	vorbis_reader.samples_channel = 0;

	//init vectors: They must be deleted at the end!!
	vorbis_reader.ch1_pcm = new vector<float>();
	vorbis_reader.ch2_pcm = new vector<float>();

	char *oggFile_path = (char *)A1;
	oggFile_name = ovpl_get_fileName(oggFile_path);

	//open the ogg file with liboggz	
  	if (!(oggz = oggz_open(oggFile_path, OGGZ_READ))) {
            	return FALSE;
     	}	

	FishSoundInfo fsinfo; //Fish sound 
	vorbis_reader.m_fishSound = fish_sound_new(FISH_SOUND_DECODE, &fsinfo);

	//preparing for decoding of Vorbis. ovpl_store_frames will manage the decoded frames extracted from each packet read
	fish_sound_set_decoded_callback(vorbis_reader.m_fishSound, ovpl_store_frames, &vorbis_reader);
	
	//preparing for reading. ovpl_read will manage the read packets from the ogg file as they are read by oggz
	oggz_set_read_callback(oggz, -1, ovpl_read_packet, &vorbis_reader);//serial number = -1 => supposing there is one logical stream
		
	//Reading .ogg file. Reads chains of 1024 bytes and call the callback function on the fly
	while (oggz_read(oggz, 1024) > 0);	

	//Stops after finishing
	fish_sound_delete(vorbis_reader.m_fishSound);
	vorbis_reader.m_fishSound = 0;
	oggz_close(oggz);

	return TRUE;
}

/*
 * This will be at the very end. but hopefully can be removed after the implementation is done
 */

PREDICATE(ovpl_reset, 0)
{
	//Reset metadata
	vorbis_reader.channel_count = 0;
	vorbis_reader.sample_rate = 0;
	vorbis_reader.samples_channel = 0;

	//Reset decoded data in vectors
	delete vorbis_reader.ch1_pcm;
	delete vorbis_reader.ch2_pcm;
	vorbis_reader.ch1_pcm=0;
	vorbis_reader.ch2_pcm=0;
			
	return TRUE;
}

/***************************************************
 *************** DECODED AUDIO DATA ****************
 **************************************************/	

/*
 * Returns the whole decoded audio as MO::signal element (Check out swimo.h and swimo.cpp in /swilib)+
 * Returning the name without path??
 */

PREDICATE(ovpl_get_decoded_signal, 1)
{	
	//-MO::signal	
	
	term_t sr = PL_new_term_ref();//all passed as terms
	term_t channels = PL_new_term_ref();
	term_t spc = PL_new_term_ref();
	term_t pcm1 = PL_new_term_ref();
	term_t pcm2 = PL_new_term_ref();
	term_t signal = PL_new_term_ref();

	PL_put_integer(sr, vorbis_reader.sample_rate);
	PL_put_integer(channels, vorbis_reader.channel_count);
	PL_put_integer(spc, vorbis_reader.samples_channel);
	
	//The pcm data is returned by a pointer to it and not the data itself as it overflows prolog memory and is not efficient.
	//The pointer is returned as AudioBlob (see swiaudioblob.h)
	if(vorbis_reader.channel_count>0){
	AudioDataConversion::pointer_to_audio_blob(vorbis_reader.ch1_pcm, pcm1);	
		if(vorbis_reader.channel_count==2){
			AudioDataConversion::pointer_to_audio_blob(vorbis_reader.ch2_pcm, pcm2);
		}
		//Creation of a signal element (swimo.h)
		MO::signal(channels, sr, spc, pcm1, pcm2, signal);	
		return A1 = PlTerm(signal);
	
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
ovpl_get_fileName(const char *path){

	QString qpath(path);
	QString qname = qpath.section("/",-1);
	return qname;
}


/*************************************************** * Specific callBacks for OGGVORBIS decoding process
 **************************************************/

/*
 * Callback that stores the data extracted at decoding into the vectors for each channel and sets the attributes of the reader
 */

int 
ovpl_store_frames(FishSound *fs, float **frames, long nframes, void *user_data){

	VorbisReader *data = (VorbisReader *)user_data;
	if(data -> channel_count==0){//initialization. Check that for the next file, the channel_count has been reset
		
		FishSoundInfo fsinfo;
	        fish_sound_command(fs, FISH_SOUND_GET_INFO, &fsinfo, sizeof(FishSoundInfo));
	        data -> channel_count = fsinfo.channels;	
		data -> sample_rate = fsinfo.samplerate;		
	}
	
	data -> samples_channel += nframes; //we may have more than one call to this callback, so we have to accumulate the frames.

	if(nframes>0){//storing the PCM for every channel
	        for (long i = 0; i < nframes; ++i) {
	        	for (size_t c = 0; c < data -> channel_count; ++c) {
				if(c==0){	//two channels max						         	
					data -> ch1_pcm -> push_back(frames[c][i]);//vector of floats. Has to be empty at destroying.
					}
				if(c==1){
					data -> ch2_pcm -> push_back(frames[c][i]);
				}
	                }
	        }
		return -1;
	}
	return 0;
}

/*
 * Callback that takes the read packets of data from the .ogg file and passes them to the vorbis decoder
 */

int
ovpl_read_packet(OGGZ *, ogg_packet *packet, long, void *user_data)
{
	VorbisReader *data= (VorbisReader *)user_data;
	FishSound *fs = data -> m_fishSound;
	if(packet->b_o_s == 1){
		cerr<<packet->bytes<<endl;//Checking the size of the bos packet with the metadata.
	}
	fish_sound_prepare_truncation(fs, packet->granulepos, packet->e_o_s);
	fish_sound_decode(fs, packet->packet, packet->bytes);
	
	return 0;
}




