#include <stdio.h>
#include <stdlib.h>
#include <iostream>

#include <faad.h>
#include "faadDecode.h"

#define MAX_CHANNELS 8 /* make this higher to support files with more channels */

						/******************************************* 
						********* FAAD BUFFER ROUTINES *************
						*******************************************/

/* declare buffering variables */
#define DEC_BUFF_VARS \
    int fileread, bytesconsumed, k; \
    int buffercount = 0, buffer_index = 0; \
    unsigned char *buffer; \
    unsigned int bytes_in_buffer = 0;

/* initialise buffering */
#define INIT_BUFF(file) \
    fseek(file, 0, SEEK_END); \
    fileread = ftell(file); \
    fseek(file, 0, SEEK_SET); \
    buffer = (unsigned char*)malloc(FAAD_MIN_STREAMSIZE*MAX_CHANNELS); \
    memset(buffer, 0, FAAD_MIN_STREAMSIZE*MAX_CHANNELS); \
    bytes_in_buffer = fread(buffer, 1, FAAD_MIN_STREAMSIZE*MAX_CHANNELS, file);

/* skip bytes in buffer */
#define UPDATE_BUFF_SKIP(bytes) \
    fseek(infile, bytes, SEEK_SET); \
    buffer_index += bytes; \
    buffercount = 0; \
    bytes_in_buffer = fread(buffer, 1, FAAD_MIN_STREAMSIZE*MAX_CHANNELS, infile);

/* update buffer */
#define UPDATE_BUFF_READ \
    if (bytesconsumed > 0) { \
        for (k = 0; k < (FAAD_MIN_STREAMSIZE*MAX_CHANNELS - bytesconsumed); k++) \
            buffer[k] = buffer[k + bytesconsumed]; \
        bytes_in_buffer += fread(buffer + (FAAD_MIN_STREAMSIZE*MAX_CHANNELS) - bytesconsumed, 1, bytesconsumed, infile); \
        bytesconsumed = 0; \
    }

/* update buffer indices after faacDecDecode */
#define UPDATE_BUFF_IDX(frame) \
    bytesconsumed += frame.bytesconsumed; \
    buffer_index += frame.bytesconsumed; \
    bytes_in_buffer -= frame.bytesconsumed;

/* true if decoding has to stop because of EOF */
#define IS_FILE_END buffer_index >= fileread

/* end buffering */
#define END_BUFF if (buffer) free(buffer);

/*Skipping id3 tag*/
int id3v2_tag(unsigned char *buffer)
{
    if (strncmp((const char *)buffer, "ID3", 3) == 0) {
        unsigned long tagsize;

        /* high bit is not used */
        tagsize = (buffer[6] << 21) | (buffer[7] << 14) |
            (buffer[8] <<  7) | (buffer[9] <<  0);

        tagsize += 10;

        return tagsize;
    } else {
        return 0;
    }
}
						/****************************
						****** DECODING FILES *******
						****************************/

/* DECODE AAC FILES */

int fdpl_decode_AAC(int def_srate, const char *file_path, size_t &c, size_t &sr,  size_t &samples, std::vector<float> * ch1, std::vector<float> *ch2)
{
    int tagsize;
    uint32_t samplerate;
    unsigned char channels;
    void *sample_buffer;

    FILE *infile;

    faacDecHandle hDecoder;
    faacDecFrameInfo frameInfo;
    faacDecConfigurationPtr config;

    /* declare variables for buffering */
    DEC_BUFF_VARS

    infile = fopen(file_path, "rb");
    if (infile == NULL)
    {
        /* unable to open file */
       	std::cerr<<"Error opening file"<<std::endl;
        return 1;
    }
    INIT_BUFF(infile)

    tagsize = id3v2_tag(buffer);//avoids ID3 tag (useful for mp3 as well)
    if (tagsize)
    {
        UPDATE_BUFF_SKIP(tagsize)
    }

    hDecoder = faacDecOpen();

    /* Set the default object type and samplerate */
    /* This is useful for RAW AAC files */
    config = faacDecGetCurrentConfiguration(hDecoder);
    if (def_srate)
        config->defSampleRate = def_srate;
    config->defObjectType = LC;//default
    config->outputFormat = FAAD_FMT_FLOAT;
    config -> downMatrix = 0; //set to 1

    faacDecSetConfiguration(hDecoder, config);

    if ((bytesconsumed = faacDecInit(hDecoder, buffer, bytes_in_buffer,
        &samplerate, &channels)) < 0)
    {
        /* If some error initializing occured, skip the file */
        std::cerr<<"Error initializing decoder library."<<std::endl;
        END_BUFF
        faacDecClose(hDecoder);
        fclose(infile);
        return 1;
    }
    buffer_index += bytesconsumed;

    do
    {
        /* update buffer */
        UPDATE_BUFF_READ

        sample_buffer = faacDecDecode(hDecoder, &frameInfo, buffer, bytes_in_buffer);

        /* update buffer indices */
        UPDATE_BUFF_IDX(frameInfo)

        if (frameInfo.error > 0)
        {
            std::cerr<<"Error: %s\n"<<faacDecGetErrorMessage(frameInfo.error)<<std::endl;
        }

        if ((frameInfo.error == 0) && (frameInfo.samples > 0) && (frameInfo.channels < 3))
        {
		size_t samples_channel = (size_t)frameInfo.samples/(size_t)frameInfo.channels;
       		samples += samples_channel;
		float *pcm;		
		pcm = (float *)sample_buffer;
		for(size_t i=0; i<samples_channel; i++){
		//writing the samples of each channel in separate float vectors
			for(size_t j=0; j<frameInfo.channels; j++){
			          		
				if(j==0){
					ch1 -> push_back(pcm[i*frameInfo.channels+j]);

				}else if(j==1){
					ch2 -> push_back(pcm[i*frameInfo.channels+j]);

				}else{
					std::cerr<<"Unexpected number of channels: "<<j<<std::endl;
					return -1;
				}
			}
		}		
        }
        if (buffer_index >= fileread)
            sample_buffer = NULL; /* to make sure it stops now */

    } while (sample_buffer != NULL);

    c = frameInfo.channels;
    sr = frameInfo.samplerate; 

    faacDecClose(hDecoder);

    fclose(infile);

    END_BUFF

    return frameInfo.error;
}

/* DECODE MP4 FILES */

/*
 * Find a AAC track inside the mp4 file
 */

int GetAACTrack(mp4ff_t *infile)
{
    /* find AAC track */
    int i, rc;
    int numTracks = mp4ff_total_tracks(infile);

    for (i = 0; i < numTracks; i++)
    {
        unsigned char *buff = NULL;
        unsigned int buff_size = 0;
        mp4AudioSpecificConfig mp4ASC;

        mp4ff_get_decoder_config(infile, i, &buff, &buff_size);

        if (buff)
        {
            rc = faacDecAudioSpecificConfig(buff, buff_size, &mp4ASC);
            free(buff);

            if (rc < 0)
                continue;
            return i;
        }
    }

    return -1;
}

uint32_t read_callback(void *user_data, void *buffer, uint32_t length)
{
    return fread(buffer, 1, length, (FILE*)user_data);
}

uint32_t seek_callback(void *user_data, uint64_t position)
{
    return fseek((FILE*)user_data, position, SEEK_SET);
}


int fdpl_decode_MP4(const char *file_path, size_t &ch, size_t &sr,  size_t &samples, std::vector<float> *ch1, std::vector<float> *ch2)
{
    unsigned int track;
    uint32_t samplerate;//these for init, we prefer to take the ones at frameInfo
    unsigned char channels;
    void *sample_buffer;

    mp4ff_t *infile;
    FILE *mp4File;
    int sampleId, numSamples;

    faacDecHandle hDecoder;
    faacDecFrameInfo frameInfo;

    unsigned char *buffer;
    unsigned int buffer_size;

    /* initialise the callback structure */
    mp4ff_callback_t *mp4cb;
    mp4cb = (mp4ff_callback_t *)malloc(sizeof(mp4ff_callback_t));

    mp4File = fopen(file_path, "rb");
    mp4cb->read = read_callback;
    mp4cb->seek = seek_callback;
    mp4cb->user_data = mp4File;

    infile = mp4ff_open_read(mp4cb);
    if (!infile)
    {
        /* unable to open file */
        std::cerr<<"Error opening file "<< file_path <<std::endl;
        return 1;
    }

    if ((track = GetAACTrack(infile)) < 0)
    {
        std::cerr<<"Unable to find correct AAC sound track in the MP4 file"<<std::endl;
        mp4ff_close(infile);
        free(mp4cb);
        fclose(mp4File);
        return 1;
    }

    buffer = NULL;
    buffer_size = 0;
    mp4ff_get_decoder_config(infile, track, &buffer, &buffer_size);

    hDecoder = faacDecOpen();

    if(faacDecInit2(hDecoder, buffer, buffer_size, &samplerate, &channels) < 0)
    {
        /* If some error initializing occured, skip the file */
      	std::cerr<<"Error initializing decoder library"<<std::endl;
        faacDecClose(hDecoder);
        mp4ff_close(infile);
        free(mp4cb);
        fclose(mp4File);
        return 1;
    }
    if (buffer)
        free(buffer);

    numSamples = mp4ff_num_samples(infile, track);

    samples = 0;
    //firstTime = 1;

    for (sampleId = 0; sampleId < numSamples; sampleId++)
    {
        int rc;

        /* get access unit from MP4 file */
        buffer = NULL;
        buffer_size = 0;

        rc = mp4ff_read_sample(infile, track, sampleId, &buffer, &buffer_size);
        if (rc == 0)
        {
            std::cerr<<"error while decoding"<<std::endl;
            faacDecClose(hDecoder);
            mp4ff_close(infile);
            free(mp4cb);
            fclose(mp4File);
            return 1;
        }

        sample_buffer = faacDecDecode(hDecoder, &frameInfo, buffer, buffer_size);

        if (buffer)
            free(buffer);

        if ((frameInfo.error == 0) && (frameInfo.samples > 0) && (frameInfo.channels < 3))
        {
		size_t samples_channel = (size_t)frameInfo.samples/(size_t)frameInfo.channels;
       		samples += samples_channel;
		float *pcm;		
		pcm = (float *)sample_buffer;
		for(size_t i=0; i<samples_channel; i++){
		//writing the samples of each channel in separate float vectors
			for(size_t j=0; j<frameInfo.channels; j++){
			          		
				if(j==0){
					ch1 -> push_back(pcm[i*frameInfo.channels+j]);

				}else if(j==1){
					ch2 -> push_back(pcm[i*frameInfo.channels+j]);

				}else{
					std::cerr<<"Unexpected number of channels: "<<j<<std::endl;
					return -1;
				}
			}
		}		
        }
		
        if (frameInfo.error > 0)
        {
            std::cerr<<"error: "<<faacDecGetErrorMessage(frameInfo.error)<<std::endl;
            break;
        }
        
    }
    ch = frameInfo.channels;
    sr = frameInfo.samplerate; 
 
    faacDecClose(hDecoder);

    mp4ff_close(infile);
    free(mp4cb);
    fclose(mp4File);

    return frameInfo.error;
}




