/**
 * SWI-Prolog external interface to Vamp Plugins
 * This C/C++ source defines foreign prediactes loaded by the vamp module (direct interface with prolog).
 * This interface integrates the libvamp-hostsdk library for vamp hosts definition written by Chris Cannam (c4dm)
 * David Pastor Escuredo 2007
 *
 */

#include <swiaudioblob.h>
#include <swimo.h>

#include <string>
#include <sndfile.h>
#include <cmath>

#include <PluginHostAdapter.h>
#include <PluginLoader.h>#include <PluginInputDomainAdapter.h>
#include <PluginChannelAdapter.h>

using namespace Vamp::HostExt;
using namespace std;


			/***************************************************************************************************
			***** Prototypes of the C functions used in this interface to enhance the simpler C++ interface ****
			***************************************************************************************************/

int
vmpl_plugin_to_blob(Vamp::Plugin *, term_t);//Vamp::Plugin* to blob so we can refer it later on

Vamp::Plugin *
vmpl_blob_to_plugin(term_t);//We get the pointer to plugin from the blob

const float* const*
vmpl_frame_to_input(term_t);//Constructs a valid block of data as vamp plugin input from a MO::frame

term_t
vmpl_features_to_prolog(Vamp::Plugin::FeatureSet, int, Vamp::RealTime);//Converts the FutureSet object extracted from the plugins into a complex Prolog term

Vamp::RealTime 
vmpl_prolog_to_timeStamp(term_t);//Takes a prolog term representing the timestamp and creates a Vamp::RealTime object

term_t
vmpl_prolog_from_timeStamp(Vamp::RealTime);//Creates a prolog representation of the Vamp::RealTime object representing the timestamp

term_t
vmpl_timestamp_humanReadable(Vamp::RealTime);//From the Vamp::RealTime object to a human-readable timestamp wrapped into a prolog term

term_t
vmpl_timestamp_float(Vamp::RealTime);//Simplest prolog representation of the timestamp as float

Vamp::RealTime
vmpl_float_timestamp(term_t);//From a float to timestamp as Vamp::RealTime


				/***********************************************************************
				**************************** Variables definition **********************
				***********************************************************************/

/*
 * Singleton loader for every plugin
 */
PluginLoader *prolog_loader=0;

/*
 * Prolog blob wrapping a pointer to a vamp plugin. Check this has not bugs if the same plugin
 */
static PL_blob_t vamp_blob = 
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
					/* unique representation for a vamp plugin*/
  "vamp_plugin",
  NULL,					/* release */
  NULL,					/* compare */
  NULL,					/* write */
  NULL,					/* acquire */
  NULL,					/* save load*/
  NULL,
};

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
		//Now we wrap the pointer into a blob so we can use it later on to fullfil the lifecycle
		term_t prolog_plugin = PL_new_term_ref();
		vmpl_plugin_to_blob(plugin, prolog_plugin);
		return A3 = PlTerm(prolog_plugin);		
		
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
	//- Pointer to plugin as a vamp_blob
	
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
		vmpl_plugin_to_blob(plugin, prolog_plugin);
		return A3 = PlTerm(prolog_plugin);		
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

/***  Second step of the plugin lifecycle ****/

/*
	We just query available outputs (returns a list with the names)
*/

PREDICATE(vmpl_pluginOutputs, 2)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//-outputsList of the working plugin (just the names)

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	plugin = vmpl_blob_to_plugin(blob);
	try{
		Vamp::Plugin::OutputList out = plugin -> getOutputDescriptors();
		cerr << (int)out.size() << " outputs." << endl;
		PlTerm prologOutputList;
		PlTail tail(prologOutputList);	

		for(unsigned int i=0;i<out.size();i++){
			
			string output = out[i].identifier;
			int length = output.size();
			char o[length+1];
			for(int j=0;j<length;j++){
				o[j]=output[j];
			}	
			o[length] = '\000';
			tail.append(PlAtom(o));
		}
		tail.close();
		return A2 = prologOutputList;

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

/***  Third step of the plugin lifecycle ****/

/*
	We just query available programs (returns a list with the names)
*/

PREDICATE(vmpl_pluginPrograms, 2)
{
	//+ plugin
	//- list of the available programs for the working plugin.
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	plugin = vmpl_blob_to_plugin(blob);
	try{
		Vamp::PluginBase::ProgramList progList = plugin -> getPrograms();
		//cerr << (int)progList.size() << " programs: " << endl;
		
		PlTerm prologProgList;
		PlTail tail(prologProgList);	

		for(unsigned int i=0;i<progList.size();i++){
			
			string program = progList[i];
			int length = program.size();
			char p[length+1];
			for(int j=0;j<length;j++){
				p[j]=program[j];
			}	
			p[length] = '\000';
			tail.append(PlAtom(p));
		}
		tail.close();
		return A2 = prologProgList;
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

/*
	This predicate sets the program specified as argument.
*/

PREDICATE(vmpl_select_program, 2)
{
	//+plugin
	//+name of the program to set for the plugin. 
	
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	plugin = vmpl_blob_to_plugin(blob);
	string program((char *)A2);
	plugin-> selectProgram(program);//if not program, do nothing
	return TRUE;
}

/*
	Query programs. The default values can't be retrieved (so far).
*/

PREDICATE(vmpl_pluginParameters, 2)
{
	//+ plugin
	//- List of the parameters that need be set for the working plugin
	
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	plugin = vmpl_blob_to_plugin(blob);
	try{
		Vamp::PluginBase::ParameterList parList = plugin -> getParameterDescriptors();
		//cerr << (int)parList.size() << " parameters" << endl;
		
		PlTerm prologParamList;
		PlTail tail(prologParamList);	

		for(unsigned int i=0;i<parList.size();i++){
			
			string param = parList[i].identifier;
			int length = param.size();
			char p[length+1];
			for(int j=0;j<length;j++){
				p[j]=param[j];
			}	
			p[length] = '\000';
			tail.append(PlAtom(p));
		}
		tail.close();
		return A2 = prologParamList;
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

/*
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
	plugin = vmpl_blob_to_plugin(blob);
	string parameter((char *)A2);
	float value = float((double)A3);
	plugin-> setParameter(parameter, value);
	return TRUE;
}

/***  Fourth step of the plugin lifecycle ****/

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
	plugin = vmpl_blob_to_plugin(blob);
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
	plugin = vmpl_blob_to_plugin(blob);
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
	plugin = vmpl_blob_to_plugin(blob);
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
	plugin = vmpl_blob_to_plugin(blob);
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
		plugin = vmpl_blob_to_plugin(blob);
		int channels = (int)A2;
		size_t stepSize = (size_t)(double)A3;
		size_t blockSize = (size_t)(double)A4;
		return plugin -> initialise(channels, stepSize, blockSize);

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
  	}
}

/***  Fifth step of the plugin lifecycle: MISSING as we don't care about the number of values/output now ****/

/***  Sixth step of the plugin lifecycle ****/

/*
	vmpl_run_plugin(+plugin, +block, -features) Minimum predicate calling the process of the plugin
*/

PREDICATE(vmpl_process_block, 4)
{
	//+plugin
	//+Block of data to process (both channels) passed as MO::frame!!!
	//+input
	//-Set of Features
	
	//getting input arguments
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	plugin = vmpl_blob_to_plugin(blob);
 
	//Frame and the necessary arguments
	term_t frame = PL_new_term_ref();
	frame = term_t(PlTerm(A2));
	float isr = MO::GET::sample_rate(frame);
	size_t initSample = MO::GET::first_sample(frame);
	
	const float* const* input;
	input = vmpl_frame_to_input(frame);

	//process the frame after having converted it into a valid block of data for vamp plugins
	
	Vamp::RealTime rt = Vamp::RealTime::frame2RealTime(initSample, (int)isr);
	PlTerm feature(vmpl_features_to_prolog(plugin->process(input, rt), (int)A3, rt));
	
	//delete input
	return A4= feature;
}

				/************************************************************************
 				******************* C Interface functions implementation ****************
				************************************************************************/

/*
 * This one returns a term reference to a blob containing a pointer to a plugin, so we can refer to it within a context
 */
int
vmpl_plugin_to_blob(Vamp::Plugin *plugin, term_t blob){

	return PL_unify_blob(blob, (void **)&plugin, sizeof(plugin), &vamp_blob);
}

/*
 * This one gives back the Vamp::Plugin pointer
 */

Vamp::Plugin *
vmpl_blob_to_plugin(term_t blob){

	PL_blob_t *type;        //type
	void *plugin;		//void pointer to plugin
	size_t size;
	PL_get_blob(blob, &plugin, &size, &type);
	if(type != &vamp_blob){
		throw PlException("Not Vamp Plugin Blob");
	}
	return *(Vamp::Plugin **)(plugin);
}

/*
 * This function converts a MO::frame into a proper block of data SPECIFIC for Vamp Plugins input type. There is no need to the other way around in this
 * type
 */

const float* const*
vmpl_frame_to_input(term_t frame){

	term_t sample_rate = PL_new_term_ref();
	term_t channel_count = PL_new_term_ref();
	term_t initpos = PL_new_term_ref();
	term_t ch1 = PL_new_term_ref();//blobs containing the data of the frame
	term_t ch2 = PL_new_term_ref();
	MO::frame(channel_count, sample_rate, initpos, ch1, ch2, frame);
	
	//parameters to initialize the block of input data
	int channels;
	PL_get_integer(channel_count, &channels);

	vector<float> vector_ch1 = AudioDataConversion::term_to_audio_vector(ch1);
	vector<float> vector_ch2;
	if(channels==2){
		vector_ch2 = AudioDataConversion::term_to_audio_vector(ch2);
	}

	//Creating the multidimensional array as input
	float **plugbuf = new float*[channels];
	for (int c = 0; c < channels; ++c){
		plugbuf[c] = new float[vector_ch1.size() + 2];//why + 2??????
	}
    
	size_t j = 0;
	//Channel 1
        while (j < vector_ch1.size()) {
                plugbuf[1][j] = vector_ch1.at(j);
                ++j;
        }
	if(channels==2){
		j=0;
		while (j < vector_ch1.size()){
			plugbuf[1][j] = vector_ch2.at(j);
                	++j;
		}
	}	
	return plugbuf;
}

//Returns a prolog list that represents a Vamp::Plugin::FeatureList for the output passed as argument (create the vice-versa typeconversion).
term_t
vmpl_features_to_prolog(Vamp::Plugin::FeatureSet fs, int output, Vamp::RealTime rt){

	//The feature set for a certain input block of data is a map with n list of feature where n is the number of the outputs that the plugin
	//provides. Each feature is a structure: label, timeStamp, vector of values.
	//We need to return a prolog thing that represents only a FeatureList for the given output.
	//For each feature we obtain a Prolog Compound Term:
	//event(timestamp, vectorOfValues)
	//The number of values is specific for each output and it is retrieved before.
	//The feature set is returned as prolog list which is a list of compound terms for a given input frame.
	try{
		PlTerm prologFeatList;
		PlTail tail(prologFeatList);	
		Vamp::Plugin::FeatureList fl = fs[output];
		//cout<<fl.size()<<endl;
		if(fl.size()>0){//avoiding empty lists when there are no features for an input frame
			for(unsigned int j=0; j<fl.size();j++){
				//Create a functor that represents a Vamp::Plugin::Feature
				//with this structure: event(timestamp, values)				
				//This structure is a compound term which functor is "event" and 
				//feature is the vector of arguments: timestamp and values
				PlTermv feature(fl[j].values.size()+1);				
				if(fl[j].hasTimestamp){						
					PlTerm timestamp = vmpl_timestamp_float(fl[j].timestamp); 
					feature[0] = timestamp;
				}else{
					PlTerm timestamp = vmpl_timestamp_float(rt);//if hasTimeStamp = false, we use the frame timestamp
					feature[0] = timestamp;
				}		
				for(unsigned int v=0; v<fl[j].values.size(); v++){
					
					feature[v+1] = ((float)fl[j].values[v]);
				}
				PlCompound prologFeature("event",feature);	
				tail.append(prologFeature);
			}
			tail.close();
			return prologFeatList;
		}
		return PlTerm((long)0);//returned if the list is empty for checking condition: no features in the passed frame.

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
  	}
}

//A float represeting the timestamp in sec for better management as prolog term
term_t
vmpl_timestamp_float(Vamp::RealTime rt){

	float timestamp = (float)rt.sec + ((float)rt.nsec)/1000000000;
	return PlTerm(timestamp);
}

//Returning a Real Time object representig the timestamp.
Vamp::RealTime
vmpl_float_timestamp(float timestamp){
	
	int sec = (int)timestamp; //look out! maybe it may round up and it would be wrong!!
	int nsec = (int)((timestamp-sec)*1000000000);
	return Vamp::RealTime(sec, nsec);
}

//Converts the RealTime object into a human-readable prolog term to be displayed in case.
term_t
vmpl_timestamp_humanReadable(Vamp::RealTime rt){

	string timestamp = rt.toString();
	int length = timestamp.size();	
	char ts[length+1];
	for(int j=0;j<length;j++){
		ts[j]=timestamp[j];
	}	
	ts[length] = '\000';
	return PlTerm(ts);
}


//The following two predicates are useful if we need to pass the realtime object as argument. Type conversion.
//from a prolog list [sec, nsec], this function creates a RealTime object.
Vamp::RealTime 
vmpl_prolog_from_timeStamp(term_t in){

	term_t head = PL_new_term_ref();
	term_t tail = PL_new_term_ref();
	term_t templist = in;
		
	int sec;
	int nsec;
	PL_get_list(templist, head, tail);
	sec = (int)head;
	nsec = (int)tail;

	return Vamp::RealTime(sec,nsec);
}

//This predicate creates a list [sec,  nsec] as Prolog representation of a Vamp::RealTime object
term_t
vmpl_prolog_to_timeStamp(Vamp::RealTime rt){

	try{
		PlTerm realTimeList;
		PlTail tail(realTimeList);
		tail.append((long)rt.sec);
		tail.append((long)rt.nsec);
		tail.close();
		return realTimeList;

	}catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
  	}
}


