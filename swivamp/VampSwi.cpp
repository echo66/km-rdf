/**
 * SWI-Prolog external interface to Vamp Plugins
 * This C/C++ source defines the module vampswi that is loaded by the vamp module (direct interface with prolog).
 * This interface integrates the libvamp-hostsdk library for vamp host definition written by Chris Cannam 
 * David Pastor Escuredo 2007
 *
 */

#include <SWI-cpp.h>
#include <iostream>
#include <string>
#include <sndfile.h>
#include <cmath>

#include <PluginHostAdapter.h>
#include <PluginLoader.h>#include <PluginInputDomainAdapter.h>
#include <PluginChannelAdapter.h>

using namespace Vamp::HostExt;
using namespace std;

//These are functions written on the C interface with SWI-Prolog to enhance the C++ interface
term_t
features_to_prologList(Vamp::Plugin::FeatureSet, int, Vamp::RealTime);

Vamp::RealTime 
get_timeStamp(term_t);

term_t
prolog_timeStamp(Vamp::RealTime);

term_t
plugin_remainingFeatures(int, int, int);

term_t
plugin_process(const float* const*, Vamp::RealTime, int);

term_t
timestamp_humanReadable(Vamp::RealTime);

//Variables
PluginLoader *prolog_loader=0;
Vamp::Plugin *plugin=0; //working plugin


/**
 * VAMPSWI: LIST OF PREDICATES
 */

//This predicate retrieves the list of Plugin Keys (system specific identifications of plugins). The list is returned as Prolog list of Keys (PlStrings)
PREDICATE(pluginsList, 1)
{
	//-PluginsList

	try{
		if(!prolog_loader) prolog_loader = PluginLoader::getInstance();
		
		//Uses the function of the class PluginLoader (that calls Impl's one)
		vector<PluginLoader::PluginKey> keysList;
		keysList = prolog_loader->listPlugins();

		//Convert keyList vector in a Prolog list	
		PlTerm prologKeysList;
		PlTail tail(prologKeysList);
		PlString *pkey;
		for(unsigned int i=0;i<keysList.size();i++){
			
			PluginLoader::PluginKey ckey = keysList[i];
			int length = ckey.size();
			char key[length+1];
			for(int j=0;j<length;j++){
				key[j]=ckey[j];
			}	
			key[length] = '\000';
			pkey = new PlString(key);
			tail.append(*pkey);
			delete[] pkey;
		}
		tail.close();

		//return the prolog list containing the PluginKeys
		return (A1 = prologKeysList);
	
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
  	}
}

//Prolog predicate to create an instance of the plugin wrapping the function loadPlugin of the class Loader
PREDICATE(instantiate_plugin, 3)
{	
	//+name of the plugin = Plugin key (see hostExt of Vamp-SDK) 
	//+Input Sample Rate
	//+Flag for adapters: we set adapt all, so we dont take care about the channels and input domain
	
	try{
		string pluginName((char *)A1);
		double isr= A2;
		//A3 would be the adapter flag in case we dont want to adapt all by default
		if(!prolog_loader) prolog_loader = PluginLoader::getInstance();
		plugin = prolog_loader->loadPlugin(pluginName,(float)isr,PluginLoader::ADAPT_ALL);//All adapters active by default
 
		if (!plugin) {
        		cerr << "ERROR: Failed to load plugin" << endl;
        		return FALSE;
		}		
		return TRUE;

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

//Free the space and pointer of the previous working plugin
PREDICATE(destroy_plugin, 0)
{
	try{ 
		delete plugin;
		plugin = 0;
		return TRUE;

 	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
  	}

}

//The following predicates retrieve meta data of the working plugin 
PREDICATE(pluginMaker, 1)
{	
	string author = plugin->getMaker();
	char maker[author.size()+1];
	for(unsigned int j=0;j<author.size();j++){
		maker[j]=author[j];
	}
	maker[author.size()] = '\000';
	PlString pMaker(maker);
	return (A1 = pMaker);
}

PREDICATE(pluginIdentifier, 1)
{
	string id = plugin->getIdentifier();
	char identifier[id.size()+1];
	for(unsigned int j=0;j<id.size();j++){
		identifier[j]=id[j];
	}
	identifier[id.size()] = '\000';
	PlString pId(identifier);
	return (A1 = pId);
}

PREDICATE(pluginName, 1)
{
	string name = plugin->getName();
	char cname[name.size()+1];
	for(unsigned int j=0;j<name.size();j++){
		cname[j]=name[j];
	}
	cname[name.size()] = '\000';
	PlString pName(cname);
	return (A1 = pName);
}
PREDICATE(pluginDescription, 1)
{
	string des = plugin->getDescription();
	char cdes[des.size()+1];
	for(unsigned int j=0;j<des.size();j++){
		cdes[j]=des[j];
	}
	cdes[des.size()] = '\000';
	PlString pDes(cdes);
	return (A1 = pDes);
}

PREDICATE(pluginCopyright, 1)
{
	string copy = plugin->getCopyright();
	char ccopy[copy.size()+1];
	for(unsigned int j=0;j<copy.size();j++){
		ccopy[j]=copy[j];
	}
	ccopy[copy.size()] = '\000';
	PlString pCopy(ccopy);
	return (A1 = pCopy);
}

PREDICATE(pluginVampVersion, 1)
{
	int vamp = plugin->getVampApiVersion();
	return (A1 = vamp);
}

PREDICATE(pluginVersion, 1)
{
	int version = plugin -> getPluginVersion();
	return (A1=version);
}

//The host may query outputs of the plugin. The plugin process call requires the selection of a certain output to be retrieved: 2nd step lifecycle
PREDICATE(pluginOutputs, 1)
{
	//-outputsList of the working plugin

	try{
		Vamp::Plugin::OutputList out = plugin -> getOutputDescriptors();
		cerr << (int)out.size() << " outputs." << endl;
		PlTerm prologOutputList;
		PlTail tail(prologOutputList);
		PlString *pout;	

		for(unsigned int i=0;i<out.size();i++){
			
			string output = out[i].identifier;
			int length = output.size();
			char o[length+1];
			for(int j=0;j<length;j++){
				o[j]=output[j];
			}	
			o[length] = '\000';
			pout = new PlString(o);
			tail.append(*pout);
			delete[] pout;
		}
		tail.close();
		return A1 = prologOutputList;

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

//Retrieve programs and parameters to be set. 2nd step of the lifecycle
PREDICATE(pluginPrograms, 1)
{
	//- list of the available programs for the working plugin
	
	try{
		Vamp::PluginBase::ProgramList progList = plugin -> getPrograms();
		//cerr << (int)progList.size() << " programs: " << endl;
		
		PlTerm prologProgList;
		PlTail tail(prologProgList);
		PlString *pprog;	

		for(unsigned int i=0;i<progList.size();i++){
			
			string program = progList[i];
			int length = program.size();
			char p[length+1];
			for(int j=0;j<length;j++){
				p[j]=program[j];
			}	
			p[length] = '\000';
			pprog = new PlString(p);
			tail.append(*pprog);
			delete[] pprog;
		}
		tail.close();
		return A1 = prologProgList;
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

PREDICATE(pluginParameters, 1)
{
	//-List of the parameters that need to be set for the working plugin
	
	try{
		Vamp::PluginBase::ParameterList parList = plugin -> getParameterDescriptors();
		//cerr << (int)parList.size() << " parameters" << endl;
		
		PlTerm prologParamList;
		PlTail tail(prologParamList);
		PlString *ppar;	

		for(unsigned int i=0;i<parList.size();i++){
			
			string param = parList[i].identifier;
			int length = param.size();
			char p[length+1];
			for(int j=0;j<length;j++){
				p[j]=param[j];
			}	
			p[length] = '\000';
			ppar = new PlString(p);
			tail.append(*ppar);
			delete[] ppar;
		}
		tail.close();
		return A1 = prologParamList;
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

//Returns the inputSampleRate for the given audio (path to it)
PREDICATE(inputSampleRate, 2)
{
	//+ audio path
	//- input sample rate

	try{
		
		SNDFILE *sndfile;
		SF_INFO sfinfo;
		memset(&sfinfo, 0, sizeof(SF_INFO));	
		string wavname((char *)A1);
		sndfile = sf_open(wavname.c_str(), SFM_READ, &sfinfo);

		if (!sndfile) {
			cerr << "ERROR: Failed to open input file \""
		             << wavname << "\": " << sf_strerror(sndfile) << endl;
		}
		int sr = sfinfo.samplerate;
		sf_close(sndfile);//close the audiofile
		return A2=sr;

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
  	}
}

//Retrieves the number of channels of the audioFile. The limits of the plugin does not matter as the plugin is configured as a channel adapter.
PREDICATE(channels_information, 2)
{
	//+ audio file
	//- number of channels of the audio file
	
	try{

		SNDFILE *sndfile;
		SF_INFO sfinfo;
		memset(&sfinfo, 0, sizeof(SF_INFO));	
		string wavname((char *)A1);
		sndfile = sf_open(wavname.c_str(), SFM_READ, &sfinfo);

		if (!sndfile) {
			cerr << "ERROR: Failed to open input file \""
		             << wavname << "\": " << sf_strerror(sndfile) << endl;
		}		
		int audio = sfinfo.channels;
		sf_close(sndfile);//close the audio file
		return A2=audio;

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

//Initialization of the plugin. Only the channels of the audio File are passed to allow the adapter perform the necessary pre-processing.
PREDICATE(initialize_plugin, 1)
{
	//-number of channels: if the adapter is flagged, then it is enough with the audio channels argument
	//For convenience, the stepSize and the blockSize are always intialized as the preferred settings.
	
	try{
		int channels = (int)A1;
		int stepSize = plugin -> getPreferredStepSize();
		int blockSize = plugin -> getPreferredBlockSize();
		bool init = plugin -> initialise(channels, stepSize, blockSize);
		return init;

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
  	}
}

//This predicate calls the predicate plugin_process iteratively passing blocks of data for a given audio file and retrieving a prolog list of sublists
//that correspond to the features extracted for each of input blocks.
PREDICATE(run_plugin, 3)
{	
	//+the audio file passed as input
	//+the output selected. 
	//-big prolog list: [output, number of values/output, [sublists of features for each input block],....]
	//the working plugin pointed by the global pointer: plugin.
	
	SNDFILE *sndfile;
	SF_INFO sfinfo;
	memset(&sfinfo, 0, sizeof(SF_INFO));	
	string wavname((char *)A1);
	sndfile = sf_open(wavname.c_str(), SFM_READ, &sfinfo);

	if (!sndfile) {
		cerr << "ERROR: Failed to open input file \""
	             << wavname << "\": " << sf_strerror(sndfile) << endl;
	}

	//setting the input stream.
	int blockSize = plugin -> getPreferredBlockSize();
	int stepSize = plugin -> getPreferredStepSize();
	int channels = sfinfo.channels;
        float *fileBuf = new float[blockSize * channels];
    	float **plugbuf = new float*[channels];
	for (int c = 0; c < channels; ++c) plugbuf[c] = new float[blockSize + 2];
	
	//Setting the prolog list
	PlTerm featureList;
	PlTail tail(featureList);

	//Information about the output has head of the list
	Vamp::Plugin::OutputList out = plugin -> getOutputDescriptors();
	string x = out[(int)A2].name;
	char selecOutput[x.size()+1];
	for(unsigned int m=0;m<x.size();m++){
		selecOutput[m]=x[m];
	}
	selecOutput[x.size()] = '\000';
	PlString poutput(selecOutput);
	tail.append(poutput);

	//the number of values/output depends on the initialization.
	if(out[(int)A2].hasFixedBinCount){
		tail.append((long)out[(int)A2].binCount);
	}else{
		tail.append("No fixed bin count!");//unusual case
	}
    	
	//passing the input to the plugin and retrieving a prolog list
	for (size_t i = 0; i < sfinfo.frames; i += stepSize) {

        int count;

        if (sf_seek(sndfile, i, SEEK_SET) < 0) {
           cerr << "ERROR: sf_seek failed: " << sf_strerror(sndfile) << endl;
           break;
        }
        
        if ((count = sf_readf_float(sndfile, fileBuf, blockSize)) < 0) {
          cerr << "ERROR: sf_readf_float failed: " << sf_strerror(sndfile) << endl;
          break;
        }

        for (int c = 0; c < channels; ++c) {
            int j = 0;
            while (j < count) {
                plugbuf[c][j] = fileBuf[j * sfinfo.channels + c];
                ++j;
            }
            while (j < blockSize) {
                plugbuf[c][j] = 0.0f;
                ++j;
            }
        }
	PlTerm result = plugin_process(plugbuf, Vamp::RealTime::frame2RealTime(i, sfinfo.samplerate),int(A2));
	if(result!=PlTerm((long)0)){
		tail.append(result);
	}
	}
    PlTerm remaining = plugin_remainingFeatures(int(A2), sfinfo.frames, sfinfo.samplerate);
    if(remaining!=PlTerm((long)0)){
    	tail.append(remaining); 
    }
    tail.close();
    return A3 = featureList;
}

//These two functions wrap the plugin feature extraction functions returning a prolog representation of the feature set instead of a
//Vamp::Plugin::FeatureSet for each input block of data passed and a Vamp::RealTime object. The input for this hybrid function is still on C++,
//so can not be called as prolog predicate itself but from a predicate that calls it iteratively
//The returned list created by the function features_to_prologList only contains a list of the whole set according to the argument int ouput.
term_t
plugin_process(const float* const* input, Vamp::RealTime rt, int output){

	try{
		return features_to_prologList(plugin -> process(input, rt), output, rt);	
	
	}catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
  	}
}

term_t
plugin_remainingFeatures(int output, int lastFrame, int sr){

	try{
		Vamp::RealTime rt(lastFrame, sr);//we need to associate the timestamp of the last frame for remaining features.
		return features_to_prologList(plugin -> getRemainingFeatures(), output, rt);

	}catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
  	}
}

//Returns a prolog list that represents a Vamp::Plugin::FeatureList for the output passed as argument.
term_t
features_to_prologList(Vamp::Plugin::FeatureSet fs, int output, Vamp::RealTime rt){

	//The feature set for a certain input block of data is a map with n list of feature where n is the number of the outputs that the plugin
	//provides. Each feature is a structure: label, timeStamp, vector of values.
	//We need to return a prolog thing that represents only a FeatureList for the output given.
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
					PlTerm timestamp = timestamp_humanReadable(fl[j].timestamp); 
					feature[0] = timestamp;
				}else{
					PlTerm timestamp = timestamp_humanReadable(rt);//if hasTimeStamp = false, we use the frame timestamp
					feature[0] = timestamp;
				}		
				for(unsigned int v=0; v<fl[j].values.size(); v++){
					
					feature[v+1] = ((float)fl[j].values[v]);//we may also create a sublist as vector of values...
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

//Converts the RealTime object into a human-readable prolog term to be passed to the feature compound term
term_t
timestamp_humanReadable(Vamp::RealTime rt){

	string timestamp = rt.toString();
	int length = timestamp.size();	
	char ts[length+1];
	for(int j=0;j<length;j++){
		ts[j]=timestamp[j];
	}	
	ts[length] = '\000';
	return PlString(ts);
}

//Type conversion of the previous predicate: Ojo es analizar un string...
//Vamp::RealTime
//humanReadable_timestamp(term_t){
//}

//This converts a prolog List into a FeatureList (The set is obtained by creating a map of all the lists for a plugin).
Vamp::Plugin::FeatureList
prologList_toFeatureList(term_t in, int numValues){

	PlTail tail(in);
	PlTerm e;
	
	Vamp::Plugin::FeatureList fs;
	bool tstamp=true;
	int k=0;
	int count = 0;
	//the list is RealTime list, set of values and so forth
	while(tail.next(e)){
		if(tstamp){
			PlTail subtail(e);
			PlTerm se;
			subtail.next(se);
			if((int)se!=-1){
				fs[k].hasTimestamp = true;
				fs[k].timestamp = get_timeStamp(e);
			}else{
				fs[k].hasTimestamp = false;
			}
			tstamp=false;
		}else{
			fs[k].values[count-1] = (int)e;
			if(count == numValues){
				count = 0;
				tstamp= true;
			}			
		}		
		k++;
		count++;
	}
	
	return fs;
}

//The following two predicates are useful if we need to pass the realtime object as argument. Type conversion.
//from a prolog list [sec, nsec], this function creates a RealTime object.
Vamp::RealTime 
get_timeStamp(term_t in){

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
prolog_timeStamp(Vamp::RealTime rt){

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

//Prints the list returned from run_plugin. Only for testing usage
PREDICATE(printFeatures, 1)
{
	PlTail tail(A1);
	PlTerm e;
	tail.next(e);
	cout << (char *)e << endl; //name
	tail.next(e);
	cout << (char *)e << endl; //values/output
	while(tail.next(e)){
		PlTail subtail(e);
		PlTerm sube;
		while(subtail.next(sube)){
			cout << (char *)sube << endl;
		}

	}
	return TRUE;
}

//This prolog wraps into a whole Prolog predicate (prolog terms as input and output) the Plugin process call for a specific output.
//PREDICATE(prologPlugin_process/4)
//{
//	return A4 = plugin_process(prologlist_to_inputbuffer(A1),get_timeStamp(A2),int(A3));
//}

//term_t
//inputbuffer_to_prologlist{}

//const float *const *
//prologlist_to_inputbuffer{}

