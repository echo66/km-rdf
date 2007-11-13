/** * SWI-Prolog external interface to MAD library for decoding of MPEG audio files. These interface will decode the following audio types:
	MP3 - MPEG 1_layer 3
	AAC - MPEG 2
 * This interface is part of the module swiaudiosource created for the Knowledge Machine (Yves Raidmon) at the c4dm, Queen Mary Universtiy of London.
 * David Pastor Escuredo 2007
 *
 */

#include <swiaudioblob.h>
#include <swimo.h>

#include <sys/stat.h>
#include <sys/mman.h>

#include <qstring.h>
#include <fcntl.h>
#include <iostream>
#include <vector>

#include <mad.h>

using namespace std;

						/**********************************************						 * Prototypes of the foreign language interface
						 *********************************************/

QString
mdpl_get_file_name(const char*);//extracts the name as QString from the path given as input of the predicate

bool
decode(void *, size_t);//calling the decode function of MAD with a previous initialization

static
enum mad_flow input(void *, struct mad_stream *);//setting the input for the MAD decoder

static
enum mad_flow output(void *, struct mad_header const *, struct mad_pcm *);//setting the output of the MAD decoder dealing with the obtained PCM

static
enum mad_flow error(void *, struct mad_stream *, struct mad_frame *);//setting the errors in decoding process

term_t
mpeg_header_to_prolog(struct mad_header const *);//type conversion function: mp3 header -> Header prolog functor (see documentation of swiaudiosource)


						/***********************************						 * Variables: audio file, Mad reader					 			                                 **********************************/

/*
 * Audio file data	
 */

QString audioMPEG_name;
struct mad_header const *m_header = 0; //mp3 data (maybe I'm deleting it)

/* 
 * Reader: This structure contains the necessary fields of data to set, run and obtain the results of the decoding process.
 * The attributes: channel_count
		   sample_rate
		   samples_channel 
		   ch1_pcm (vector *)
		   ch2_pcm (vector *)
 * are in common for every decoding interface and are present in the PCM functor
 */

static struct MadReader{

	size_t channel_count;	//number of channels
	size_t sample_rate;	//sample rate
	size_t samples_channel;	//number of samples of each channel	

	//Structure to pass the data to the decoder
	struct buffer {
  		unsigned char const *start; 	//beginning of data	
 		unsigned long length;		//number of bytes of the data
	};

	//PCM
	vector<float> *ch1_pcm;//pcm data for both channels
	vector<float> *ch2_pcm;

}mad_reader;//instance of the structure used in the interface


					/****************************************************************************					 * Prolog predicates used in the swiaudiosource module (C++/Prolog interface)
					 ***************************************************************************/

/*
 * Main predicate of the interface. This predicate runs a MAD decoder over the given audio file path setting the data of mad_reader.
 * This predicate allows the decoding of MP3 and AAC audio files.
 * After calling this predicate we can find the extracted pcm and the data in the mad_reader
 */

PREDICATE(mdpl_decode,1)
{
	//+path to the audio file

	//init
	mad_reader.channel_count = 0;
	mad_reader.samples_channel = 0;
	mad_reader.sample_rate = 0;

	//init pcm data
	//init vectors: They must be deleted at the end!!
	mad_reader.ch1_pcm = new vector<float>();
	mad_reader.ch2_pcm = new vector<float>();

	char *audioMPEG_path = (char *)A1;
	audioMPEG_name = mdpl_get_file_name(audioMPEG_path); //extracts the name of the audio file from the absolute path 

	struct stat stat;			//status structure
	void *fdm;				//beginning of data	
	int fd = -1; 				//file descriptor

	fd = open (audioMPEG_path, O_RDONLY, 0);//open the file and get the file descriptor 
	if (fd < 0){cerr<<"Failed to open the audio file"<<endl;}
	
	if (fstat(fd, &stat) == -1 || stat.st_size == 0){
		cerr<<"Failed to check the file state"<<endl;
	}
	fdm = mmap(0, stat.st_size, PROT_READ, MAP_SHARED, fd, 0);//beginning of the data

	//Decoding of the map containing the data extracted from the audio file. Call to decode that intializes the decoder and uses the callbacks
	//input, output and error
	if (fdm == MAP_FAILED){
		cerr<<"Failed to map"<<endl;
	}else{
	
		if(!decode(fdm, stat.st_size)){
			cerr<<"Failed to decode the file"<<endl;
		}else{
			return TRUE;	//successful decoding. mad_reader is already containing the decoded data.
		}	
	}
	//fdm must be deleted??
	return FALSE;
}

/*
 * Could be called at the end (try to remove)
 */

PREDICATE(mdpl_reset, 0)
{
	mad_reader.channel_count = 0;
	mad_reader.samples_channel = 0;
	mad_reader.sample_rate = 0;
	
	//Delete pointers
	delete mad_reader.ch1_pcm;
	delete mad_reader.ch2_pcm;
	mad_reader.ch1_pcm=0;
	mad_reader.ch2_pcm=0;

	m_header = 0;
	return TRUE;
}

/***************************************************
 *************** DECODED AUDIO DATA ****************
 **************************************************/	

/*
 * Returns the whole decoded audio as MO::signal element (Check out swimo.h and swimo.cpp in /swilib)+
 * Returning the name without path??
 */

PREDICATE(mdpl_get_decoded_signal, 1)
{	
	//-MO::signal	
	
	term_t sr = PL_new_term_ref();//all passed as terms
	term_t channels = PL_new_term_ref();
	term_t spc = PL_new_term_ref();
	term_t pcm1 = PL_new_term_ref();
	term_t pcm2 = PL_new_term_ref();
	term_t signal = PL_new_term_ref();

	PL_put_integer(sr, mad_reader.sample_rate);
	PL_put_integer(channels, mad_reader.channel_count);
	PL_put_integer(spc, mad_reader.samples_channel);
	
	//The pcm data is returned by a pointer to it and not the data itself as it overflows prolog memory and is not efficient.
	//The pointer is returned as AudioBlob (see swiaudioblob.h)
	if(mad_reader.channel_count>0){
	AudioDataConversion::pointer_to_audio_blob(mad_reader.ch1_pcm, pcm1);	
		if(mad_reader.channel_count==2){
			AudioDataConversion::pointer_to_audio_blob(mad_reader.ch2_pcm, pcm2);
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
mdpl_get_file_name(const char *path){

	QString qpath(path);
	return qpath.section("/",-1);
}


/********************************************* * Specific callBacks for MAD decoding process
 ********************************************/

/*
 * Function wrapping the decoding process
 */

bool
decode(void *mm, size_t sz){

	MadReader::buffer data;		
	struct mad_decoder decoder;

	data.start = (unsigned char const *)mm;	//initialization of the buffer
	data.length = (unsigned long)sz;

	//initialization of the decoder: Data, input, output and error
	//run the decoder in Synchronous mode and quit the decoder
	mad_decoder_init(&decoder, &data, input, 0, 0, output, error, 0);
	mad_decoder_run(&decoder, MAD_DECODER_MODE_SYNC);
	mad_decoder_finish(&decoder);
	 
	return TRUE; //Errors are handled by error function
}

/*
 * INPUT
 * OUTPUT
 * ERROR
 * callbacks
 */

enum mad_flow 
input(void *dt, struct mad_stream *stream){
  
	MadReader::buffer *data = (MadReader::buffer *)dt;
 	if (!data->length) return MAD_FLOW_STOP; //stop if there is no more data to analyse.

 	mad_stream_buffer(stream, data->start, data->length);
 	data->length = 0;

 	return MAD_FLOW_CONTINUE;//Continue the decoding
}

enum mad_flow
output(void *dt, struct mad_header const *header, struct mad_pcm *pcm){
	
	
	struct mad_pcm *m_pcm;
	m_pcm = pcm; 			//just assign them to the global variables
	m_header = header;

	int channels = pcm -> channels;
	int samples = pcm -> length;	//number of samples passed in this callback

	if (samples < 1) return MAD_FLOW_CONTINUE;

	if(mad_reader.channel_count == 0) {
        	 mad_reader.channel_count = channels; //filling the data just once
        	 mad_reader.sample_rate = pcm->samplerate;
	}

	mad_reader.samples_channel += samples; //accumulating samples

	for(int i=0; i<samples; i++){
		//writing the samples of each channel in separate float vectors
		for(int ch=0; ch<channels; ch++){
			mad_fixed_t sample = 0;
          		if (ch < int(sizeof(pcm->samples) / sizeof(pcm->samples[0]))) {
               			sample = pcm->samples[ch][i];
            		}
             		float fsample = float(sample) / float(MAD_F_ONE);
			if(ch==0){
				mad_reader.ch1_pcm -> push_back(fsample);
			}else if(ch==1){
				mad_reader.ch2_pcm -> push_back(fsample);
			}else{
				cerr<<"Unexpected number of channels: "<<ch<<endl;
			}
		}			
	}

	return MAD_FLOW_CONTINUE;
}

enum mad_flow 
error(void *dt, struct mad_stream *stream, struct mad_frame *frame){
  
	MadReader::buffer *data = (MadReader::buffer *)dt;

	//fprintf(stderr, "decoding error 0x%04x (%s) at byte offset %u\n",
	//stream->error, mad_stream_errorstr(stream),
	//stream->this_frame - data->start);

  	return MAD_FLOW_CONTINUE;
}


/********************************************* * TYPE CONVERSION: MAD types/SWI-Prolog types (Specific for mad library)
 ********************************************/

/*
 * 1. HEADER-PROLOG and PROLOG-HEADER
 */

/*
 * This function extracts some information from the header and returns them as a prolog Term with the following structure:
 
			 header(name,[layer, mode, de-emphasis, bitrate (bps), samplerate (Hz), duration in seconds]);
 */

term_t
mpeg_header_to_prolog(struct mad_header const *h){

	//These are the fields extracted, but there are some other that might be of interest.
	enum mad_layer layer = h -> layer;		/* audio layer (1, 2, or 3) */
 	enum mad_mode mode = h -> mode;			/* channel mode (see above) */
	enum mad_emphasis emphasis = h -> emphasis;	/* de-emphasis to use (see above) */
  	unsigned long bitrate = h -> bitrate;		/* stream bitrate (bps) */
  	unsigned int samplerate = h -> samplerate;	/* sampling frequency (Hz) */
  	mad_timer_t duration = h -> duration;		/* audio playing time of frame  */

	//Creating prolog List
	PlTerm prologHeader;
	PlTail tail(prologHeader);

	//layer
	if(layer==1){tail.append(PlAtom("Layer I"));}
	else if(layer==2){tail.append(PlAtom("Layer II"));}
	else if(layer==3){tail.append(PlAtom("Layer III"));}
	else tail.append(PlAtom("No layer info"));

	//mode
	if(mode==0) tail.append(PlAtom("single channel"));
	else if(mode==1) tail.append(PlAtom("dual channel"));
	else if(mode==2) tail.append(PlAtom("joint (MS/intensity) stereo"));
	else if(mode==3) tail.append(PlAtom("normal LR stereo"));
	else tail.append(PlAtom("no channel info"));

	//emphasis
	if(emphasis==0) tail.append(PlAtom("no emphasis"));
	else if(emphasis==1) tail.append(PlAtom("50/15 microseconds emphasis"));
	else if(emphasis==2) tail.append(PlAtom("unkown emphasis"));
	else if(emphasis==3) tail.append(PlAtom("CCITT J.17 emphasis"));
	else tail.append(PlAtom("no emphasis info"));
	
	//bit rate, sample rate and duration in seconds
	tail.append((long)bitrate);
	tail.append((long)samplerate);
	tail.append((long)duration.seconds); //only seconds: It doesn't work.

	tail.close();
	
	PlTermv terms(PlAtom(audioMPEG_name.toLocal8Bit().data()), prologHeader);
	PlCompound mpegHeader("Header", terms);//Compound term: Header('file name', [fields of the header])

	return mpegHeader;
}


