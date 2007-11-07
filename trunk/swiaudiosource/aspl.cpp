/** * Foreign interface with predicates useful in the audiosource module. Basically this does:
 *	-get the extension of an audio file
 *	-delete the decoded audio data in memory that is not longer needed
 */

#include <swiaudioblob.h>
#include <swimo.h>

#include <vector>
#include <qstring.h>
#include <iostream>

using namespace std;

				/****************************************************************************				 * Prolog predicates used in the swiaudiosource module (C++/Prolog interface)
				 ***************************************************************************/


/** * Extracts the file extension from the file path to the audio file
 */

PREDICATE(aspl_file_extension, 2)
{
	//+ Path to audioFile
	//- extension of the audio file

	QString filePath((char *)A1);
	QString ext = filePath.section(".",-1);
	return A2 = PlTerm(PlAtom(ext.toLocal8Bit().data()));

}

/*
 * After we have used completely the signal, its data can be deleted from local memory. This is very important
 */

PREDICATE(aspl_clean_signal_inmemory, 1)
{
	//+MO::signal to be deleted

	//getting input:
	term_t signal = PL_new_term_ref();
	signal = term_t(PlTerm(A1));
	
	//setting variables of signal to be read
	term_t sample_rate = PL_new_term_ref();
	term_t channel_count = PL_new_term_ref();
	term_t samples_channel = PL_new_term_ref();
	term_t pcm1 = PL_new_term_ref();//blobs containing the pointer to the data in memory
	term_t pcm2 = PL_new_term_ref();
	MO::signal(channel_count, sample_rate, samples_channel, pcm1, pcm2, signal);//gets the parameters for signal (swimo)
	
	//Getting the pointers and deleting the data!
	vector<float> *ch1 = 0;
	vector<float> *ch2 = 0;
	
	int channels;
	PL_get_integer(channel_count, &channels);
	if(channels>0){
		AudioDataConversion::pointer_to_audio_blob(ch1, pcm1);//getting pointer to channel 1 data
		delete ch1;
		ch1 = 0;
		if(channels == 2){
			AudioDataConversion::pointer_to_audio_blob(ch2, pcm2);//get the pointer to channel 2 data
			delete ch2;//deleting data, free memory
			ch2 = 0;	
		}
		return true;
	}else{
		return true;
	}
}



