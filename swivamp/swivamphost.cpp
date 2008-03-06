/**
 * SWI-Prolog external interface to Vamp Plugins
 * This C/C++ source defines foreign prediactes loaded by the vamp module (direct interface with prolog).
 * This interface integrates the libvamp-hostsdk library for vamp hosts definition written by Chris Cannam (c4dm). This interfaces defines the functions
 * that should be supported by the host to extract features (see swivamplugin for predicates to describe the plugin)
 * David Pastor Escuredo 2007, c4dm, Queen Mary University of London
 *
 */

#include <swivamp.h>

#include <string>
#include <cmath>

#include <PluginLoader.h>
#include <PluginInputDomainAdapter.h>
#include <PluginChannelAdapter.h>

using namespace Vamp::HostExt;
using namespace std;


				/***********************************************************************
				**************************** Variables definition **********************
				***********************************************************************/

/*
 * Singleton loader for every plugin
 */
PluginLoader *prolog_loader=0;

/*
 * Current Feature Set computed. This should be used in an atomic predicate or we'd have concurrency issues
 */
Vamp::Plugin::FeatureSet currentfs;
float startfs = 0; //we need to keep the timestamp as c types because term_t not referenced are garbage collected (the same annoying thing)
float durationfs = 0;

/*
*
*/
vector<float> 
vmpl_select_frame(size_t, size_t, vector<float> *);

				/************************************************************************
 				********************* Foreign predicates (deterministic) ****************
				************************************************************************/

/**
 * This predicate retrieves the list of Plugin Keys (system specific identifications of plugins) available in the system and compatible with Vamp. The
 * list is returned as Prolog list of Keys (Prolog atoms)
 */

PREDICATE(vmpl_plugins, 1)
{
	//-PluginsList: list of Prolog atoms containing the plugin keys.

	try{
		if(!prolog_loader) prolog_loader = PluginLoader::getInstance();
		
		//Uses the function of the class PluginLoader (that calls Impl's one)
		vector<PluginLoader::PluginKey> keysList;
		keysList = prolog_loader->listPlugins();

		//Convert keyList vector in a Prolog list	
		PlTerm prologKeysList;
		PlTail tail(prologKeysList);
		for(unsigned int i=0;i<keysList.size();i++){
			
			PluginLoader::PluginKey ckey = keysList[i];
			int length = ckey.size();
			char key[length+1];
			for(int j=0;j<length;j++){
				key[j]=ckey[j];
			}	
			key[length] = '\000';
			PlAtom pkey(key);//atoms
			tail.append(pkey);
		}
		tail.close();

		//return the prolog list containing the PluginKeys
		return (A1 = prologKeysList);
	
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
  	}
}

/***  NOTE: following predicates do follow the plugin lifecycle, so the documentation can be found at Plugin.h file ****/

/***  First step of the plugin lifecycle ****/

/*
	vmpl_load_plugin(+PluginKey, +SampleRate, -Plugin). Loads any plugin available given its name and specifying the input sample rate. The plugin	
	in pointed by the returned prolog term
*/

PREDICATE(vmpl_load_plugin, 3)
{	
	//+ name of the plugin = Plugin key (see hostExt of Vamp-SDK)
	//+ input sample rate
	//+ Flag for adapters (to set)
	//- Pointer to plugin as a vamp_blob
	
	//This is a pointer to the plugin that will be wrapped and returned to prolog for later queries.
	Vamp::Plugin *plugin=0; 
	try{
		//Getting data from terms
		string pluginName((char *)A1);//plugin key
		float isr = float(double(A2));		
		//A3 would be the adapter flag in case we dont want to adapt all by default
		
		//just one prolog loader for every plugin
		if(!prolog_loader) prolog_loader = PluginLoader::getInstance();
		
		//load plugin 	
		plugin = prolog_loader->loadPlugin(pluginName, isr, PluginLoader::ADAPT_ALL);//All adapters active by default
		if (!plugin) {
        		cerr << "ERROR: Failed to load plugin" << endl;
        		return FALSE;
		}		
		//Now we return an id to the plugin
		term_t plugin_id = PL_new_term_ref();
		vmpl_register_plugin(plugin, plugin_id);
		return A3 = PlTerm(plugin_id);		
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}


/**
 *  vmpl_load_plugin_for(+PluginKey, +MO::signal or MO::frame, -Plugin). This is an optional predicate to load the plugin given an MO::element 	  
 *  containing audio data to be processed
 */

PREDICATE(vmpl_load_plugin_for, 3)
{	
	//+ name of the plugin = Plugin key (see hostExt of Vamp-SDK)
	//+ MO::signal or MO::frame
	//+ Flag for adapters (to set)
	//- Id for the plugin in memory
	
	//This is a pointer to the plugin that will be wrapped and returned to prolog for later queries.
	Vamp::Plugin *plugin=0; 
	try{
		//Getting data from terms
		string pluginName((char *)A1);//plugin

		//We get the data and check if we are passing a MO::signal or MO::frame
		term_t data = PL_new_term_ref();//signal and get the isr
		data = term_t(PlTerm(A2));
		
		//A3 would be the adapter flag in case we dont want to adapt all by default
		
		//just one prolog loader for every plugin
		if(!prolog_loader) prolog_loader = PluginLoader::getInstance();
		
		float isr = MO::GET::sample_rate(data);//see swimo.h
		if(isr<0){//incorrect input data
			return false;
		}
		//get instance of a plugin for the correct isr
		plugin = prolog_loader->loadPlugin(pluginName, isr, PluginLoader::ADAPT_ALL);//All adapters active by default
		if (!plugin) {
        		cerr << "ERROR: Failed to load plugin" << endl;
        		return FALSE;
		}		
		//Now we wrap the pointer into a blob so we can use it later on to fullfil the lifecycle
		term_t prolog_plugin = PL_new_term_ref();
		vmpl_register_plugin(plugin, prolog_plugin);
	
		return A3 = PlTerm(prolog_plugin);		
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

/***  Second step of the plugin lifecycle (use swivampplugin) ****/

/***  Third step of the plugin lifecycle ****/

/*
	Query the programs if necessary and select one of them using the following predicate
	This predicate sets the program specified as argument.
*/

PREDICATE(vmpl_select_program, 2)
{
	//+plugin
	//+name of the program to set for the plugin. 
	
	term_t plugin_id = PL_new_term_ref();
	plugin_id = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(plugin_id, plugin);
	string program((char *)A2);
	plugin-> selectProgram(program);//if not program, do nothing
	return TRUE;
}

/*
	Query parameters using vmpl_plugin_parameters
	Setting parameters. If not, default ones will be used as normal
*/
PREDICATE(vmpl_set_parameter, 3)
{
	//+plugin
	//+parameter to set
	//+value of the parameter. 
	
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);
	string parameter((char *)A2);
	float value = float((double)A3);
	plugin-> setParameter(parameter, value);
	return TRUE;
}

/***  Fourth step of the plugin lifecycle (they stay here in vamphost) ****/

/*
	Query preferred block size
*/

PREDICATE(vmpl_get_blockSize, 2)
{
	//+ plugin
	//- pref block size

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);
	size_t bs = plugin -> getPreferredBlockSize();
	return A2 = PlTerm(long(bs));
}

/*
	Query preferred step size
*/

PREDICATE(vmpl_get_stepSize, 2)
{
	//+ plugin
	//- pref step size

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);
	size_t ss = plugin -> getPreferredStepSize();
	return A2 = PlTerm(long(ss));
}

/*
	Query channel limits. Well, it is not so important as by default we use a ChannelAdapter plugin.
*/

PREDICATE(vmpl_get_min_channel, 2)
{
	//+ plugin
	//- min channels

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);
	size_t min = plugin -> getMinChannelCount();
	return A2 = PlTerm(long(min));
}

PREDICATE(vmpl_get_max_channel, 2)
{
	//+ plugin
	//- max channels

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);
	size_t max = plugin -> getMaxChannelCount();
	return A2 = PlTerm(long(max));
}

/***  Fifth step of the plugin lifecycle ****/

/*
	Initializing the plugin. We give freedom to set it, but should be done with the preferred ones.
*/

PREDICATE(vmpl_initialize_plugin, 4)
{
	//+plugin
	//+number of channels
	//+step size
	//+block size

	try{
		//getting parameters
		term_t blob = PL_new_term_ref();
		blob = term_t(PlTerm(A1));
		Vamp::Plugin *plugin;
		vmpl_get_plugin(blob, plugin);
		int channels = (int)A2;
		size_t stepSize = (size_t)(double)A3;
		size_t blockSize = (size_t)(double)A4;
		return plugin -> initialise(channels, stepSize, blockSize);

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
  	}
}

/***  Sixth step of the plugin lifecycle: use predicates for swivampplugin ****/

/***  Seventh step of the plugin lifecycle ****/


/*
	This is an alternative predicate which pretends to do better performance in time of running a plugin from prolog doing an internal
	framing. We then not put the frames into the id_db
*/

PREDICATE(vmpl_process_store_framing, 4){

	//+plugin
	//+signal
	//+start
	//+end

	//getting input arguments
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	term_t signal = PL_new_term_ref();
	signal = term_t(PlTerm(A2));
	size_t start = (size_t)(long)A3;
	size_t size = (size_t)(long)A4;

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

	//Checks that the frame requested is within the signal passed
	long limit;
	PL_get_long(samples_channel, &limit);
	if(start >= (size_t)limit){
		return false;
	}

	//Now we retrieve the pointers to the raw data in memory
	vector<float> *ch1;
	vector<float> *ch2;
	if(DataID::get_data_for_id((const char *)id1, ch1)<=0){
		return false;
	}
	int channels;
	PL_get_integer(channel_count, &channels);
		
	vector<float> vector_ch1 = vmpl_select_frame(start, start+size, ch1);

	//Creating the multidimensional array as input
	float **plugbuf = new float*[channels];
	for (int c = 0; c < channels; ++c){
		plugbuf[c] = new float[vector_ch1.size() + 2];//why + 2??????
	}
    
	size_t j = 0;
        while (j < vector_ch1.size()) {
                plugbuf[0][j] = vector_ch1[j];
                ++j;
        }
	if(channels==2){

		DataID::get_data_for_id((const char *)id2, ch2);
		vector<float> vector_ch2 = vmpl_select_frame(start, start+size, ch2);
		j=0;
		while (j < vector_ch1.size()){
			plugbuf[1][j] = vector_ch2[j];
                	++j;
		}
	}	

	long sr;
	PL_get_long(sample_rate, &sr);

	//Now we just store the resulting FeatureSet and its default timestamp
	currentfs = plugin->process(plugbuf, Vamp::RealTime::frame2RealTime(start, (int)sr));
	startfs = (float)start/(float)sr;
	durationfs = (float)size/(float)sr;
	
	//free memory of the block of data
	for(size_t k=0; k<channels; k++){
		delete[] plugbuf[k];
	}
	delete[] plugbuf;
	
	return true;
}


/*
	vmpl_process_block(+plugin, +MO::frame, +MO::timestamp, output, -features) Minimum predicate calling the process of the plugin for a
	MO::frame object as input

	vmpl_process_block wraps this predicate and vmpl_featureSet_output
*/

PREDICATE(vmpl_process_store, 3)
{
	//+plugin
	//+Block of data to process (both channels) passed as MO::frame!!!
	//+MO::timestamp of the MO::frame
	
	//getting input arguments
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);
 
	//Frame and the necessary arguments
	term_t frame = PL_new_term_ref();
	frame = term_t(PlTerm(A2));
	float isr = MO::GET::sample_rate(frame);
	size_t initSample = MO::GET::first_sample(frame);
	
	const float* const* input;
	input = vmpl_frame_to_input(frame);

	//Timestamp of the frame. This is important, but careful!!! the feature may not have the same timestamp of its frame!!!
	term_t framets = PL_new_term_ref();
	framets = term_t(PlTerm(A3));
	
	//Now we just store the resulting FeatureSet and its default timestamp
	currentfs = plugin->process(input, Vamp::RealTime::frame2RealTime(initSample, (int)isr));
	startfs = MO::GET::start(framets);
	durationfs = MO::GET::duration(framets);
	
	//free memory of the block of data
	for(size_t k=0; k<(size_t)MO::GET::channels_count(frame); k++){
		delete[] input[k];
	}
	delete[] input;
	
	return true;

}

/*
 *	While current Feature Set is valid we can query it to get the outputs we need
 *
 *	This predicate returns:
 *				
 *	[Feature1, Feature2, Feature3, Feature4] for one ouput and one frame (normally there is one feature though)
 */
PREDICATE(vmpl_featureSet_output, 3){

	//+Plugin
	//+Output
	//+Prolog list representing the features returned for just one frame and one output selected

	//getting input arguments
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	//retrieving stored context
	term_t duration = PL_new_term_ref();
	term_t start = PL_new_term_ref();
	PL_put_float(start, startfs);
	PL_put_float(duration, durationfs);
	term_t framets = PL_new_term_ref();
	MO::timestamp(start, duration, framets);	

	PlTerm feature(vmpl_frame_features_to_prolog(currentfs, (int)A2, framets, plugin -> getOutputDescriptors()[(int)A2]));

	return A3 = feature;

}

/*** Eighth step of the lifecycle ***/

/*
 * vmpl_remaining_features(+plugin, +lastSamplePos, +sampleRate, +output, -Features): Predicate retrieving remaining features. Should be called 
 * just once after all the frames are processed
 */

PREDICATE(vmpl_store_remaining, 3)
{
	//+plugin
	//+last sample position
	//+samplerate
	cerr<<"remaining"<<endl;
	//getting input arguments
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);
 
	//Frame and the necessary arguments
	double lastSample = (double)A2;
	double sr = (double)A3;
	
	//saving context
	startfs = (float)lastSample/sr;
	durationfs = 0.0f;
	
	//wraps the plugin process and returns a list of MO::feature elements for each frame
	currentfs = plugin->getRemainingFeatures();
	return true;
}

/*** Ninth step lifecycle ***/

/*
 * Reset and initialize again if needed
 */

PREDICATE(vmpl_plugin_reset, 1)
{
	//+plugin

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	plugin -> reset();
	return true;
}

/*destroy it if not longer used (free space)*/

PREDICATE(vmpl_destroy_plugin, 1)
{
	//+plugin

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);
	
	delete plugin;
	return true;
}
	
//**Redifine this here doesn¡t seem good idea**/
	
vector<float>
vmpl_select_frame(size_t start, size_t end, vector<float> *channel){	

	vector<float> frame;
	size_t limit = channel-> size();
	for(size_t i=start; i<(end+1); i++){
		if(i < limit){
			
			frame.push_back(channel->at(i));			
		}else{
			frame.push_back(0.0f);//complete with 0 till fill the size of the frame queried
		}
	}
	
	return frame;
}


	
