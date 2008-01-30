/**
 * SWI-Prolog external interface to Vamp Plugins
 * This C/C++ source defines foreign functions a library for swivamp. Mainly type-conversion stuff
 * David Pastor Escuredo 2007
 *
 * ToDo:
 	1- So far we just pass frames to the predicate wrapping the process method. It may be interesting to pass the whole signal and get comletely
	responsible of the framing as it can reduces time in some cases.
	
	2- We are only supporting 2 channels signals and 2 channels input blocks for the plugin. We should support more as some plugins may need to have
	more channels as input.

	3- The only way to run a process is just by giving a frame from a specific signal. We want to give the whole signal and frames from different 
	signals as well, forming a multichannel block if necessary.
 */

#include <swivamp.h>


using namespace std;	

/*
 * This structure associates an id to each plugin
 */
static struct SwiVampPlugin{

	/* id: __plugin_id */
	QString id;
	/* pointer a Vamp Plugin */
	Vamp::Plugin *plugin;
	/* name of the plugin */
	char *type;
}

/*
 * Database with all the entries id/plugin
 */

vamp_plugins_db[MAX_VAMP_PLUGIN];

/* Variables */
size_t active_plugins = 0;

				/************************************************************************
 				******************* C Interface functions implementation ****************
				************************************************************************/

/** Some functions to deal with the database **/

/*
 * Stores the instance of the plugin when vamp_plugin_load and returns an id that will be the handle for the plugin onwards. The plugin
 * is defined by the name and the sample rate. We don't reuse plugins (we could) and we don't save them through sessions.
 */

int
vmpl_register_plugin(Vamp::Plugin *plugin, term_t id){

	vamp_plugins_db[active_plugins].plugin = plugin;
	vamp_plugins_db[active_plugins].id = vmpl_id_for_vamp();
	
	PL_unify(id, term_t(PlTerm(PlAtom((vamp_plugins_db[active_plugins].id).toLocal8Bit().data()))));
	active_plugins++;
	return 0; //success
}

/*
 * Creates a simple id incrementally for the plugins
 */

QString
vmpl_id_for_vamp()
{
	QString head("__vamp::plugin_");

	//incremental id for blobs	
	QString var;
	var = QString("%1")
		.arg((long)active_plugins);
	head.append(var);
	return head;

}

/*
 * Gets the plugin for the id given
 */

int
vmpl_get_plugin(term_t id_t, Vamp::Plugin * &plugin){

	char *id;
	PL_get_atom_chars(id_t,&id);
	QString qid((const char*)id);
	
	for(size_t r=0; r<MAX_VAMP_PLUGIN; r++){
		
		if(qid.compare(vamp_plugins_db[r].id)==0){
			plugin = vamp_plugins_db[r].plugin;
			return 0;
		}
	}
	return -1;	
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

	char *id1;//atom to const char *
	char *id2;
	PL_get_atom_chars(ch1, &id1);
	PL_get_atom_chars(ch2, &id2);

	//Now we retrieve the pointers to the raw data in memory
	vector<float> *vector_ch1;
	vector<float> *vector_ch2;
	DataID::get_data_for_id((const char *)id1, vector_ch1);

	//Creating the multidimensional array as input
	float **plugbuf = new float*[channels];
	for (int c = 0; c < channels; ++c){
		plugbuf[c] = new float[vector_ch1->size() + 2];//why + 2??????
	}
    
	size_t j = 0;
        while (j < vector_ch1->size()) {
                plugbuf[0][j] = vector_ch1->at(j);
                ++j;
        }
	if(channels==2){

		DataID::get_data_for_id((const char *)id2, vector_ch2);		
		j=0;
		while (j < vector_ch1->size()){
			plugbuf[1][j] = vector_ch2->at(j);
                	++j;
		}
	}	
	
	return plugbuf;//the memory is freed afterwards
}

/*
 * Returns a prolog list of MO::feature for a passed frame of data
 * Our MO::feature is a single feature for one frame and one output. In other words Vamp::FeatureSet[a][b]
 * MO::feature is 'Feature'(type, MO::timestamp, FeatureEvent) where FeatureEvent is (by now) a vector
 */

term_t
vmpl_frame_features_to_prolog(Vamp::Plugin::FeatureSet fs, int output, term_t framets, Vamp::Plugin::OutputDescriptor od){

	//Vamp::FeatureSet (map of a list of features for each frame/output. 
	//framets is the MO::timestamp of the passed frame
	//od is the outputdescriptor for ouput
	
	try{
		//Description of the output
		term_t featureType = PL_new_term_ref();
		featureType = term_t((PlTerm)vmpl_string_to_atom(od.identifier));
		//SampleType
		int stype = od.sampleType; //We rely on this to assign the correct timestamp
		//stype 0 -> one sample per output
		//stype 1 -> fixed output sample rate
		//stype 2 -> variable output sample rate
		float osr = 0.0f;
		if(stype!=0){
			osr = od.sampleRate; //We use this to calculate the timestamps when there is more than one sample per step
		}
		PlTerm prologFeatList;
		PlTail tail(prologFeatList);	
		Vamp::Plugin::FeatureList fl = fs[output];

		if(fl.size()>0){//avoiding empty lists when there are no features for an input frame
			
			for(unsigned int j=0; j<fl.size();j++){
				term_t feature_term = PL_new_term_ref();//MO::feature
				term_t featurets = PL_new_term_ref();//MO::timestamp
				
				//Different MO::timestamp algorithm for each sample type
				if(stype==0){
					//the MO::timestamp of the frame as there is only one feature in the frame

					featurets = framets; 
				}
				else if(stype==1){
					//Start: the first one will have the same that the input frame and the rest will increase 1/osr
					//Duration: constant duration 1/osr	
				
					float frameStart = MO::GET::start(framets);
					float start_point=frameStart + (j*(1/osr));

					term_t start = PL_new_term_ref();
					term_t duration = PL_new_term_ref();
					PL_put_float(start, start_point);	
					PL_put_float(duration, 1/osr);		
					MO::timestamp(start, duration, featurets);
				}	
				else if(stype==2){ 
					//Start: reads feature.timestamp
					//Duration checks osr. 1/osr if any or 0 if not defined
					//osr may exist to give some resolution to the feature(optional), BUT the real output rate is variable
					
					float d = 0.0f;
					if(osr!= 0.0f){
						d = 1/osr;
					}			
					term_t start = PL_new_term_ref();
					term_t duration = PL_new_term_ref();
					PL_put_float(start, vmpl_timestamp_float(fl[j].timestamp));	
					PL_put_float(duration, d);		
					MO::timestamp(start, duration, featurets);

				}
				else{ cerr<<"Unexpected sample type"<<endl; }

				term_t feature_event; //new id for the feature values. Id of the form __data_id
				
				vector<float> *f_vector;
				f_vector = new vector<float>();

				for(size_t r=0; r<fl[j].values.size(); r++){
					f_vector -> push_back(fl[j].values.at(r));
				}				
				//creating an id for the data stored in memory
				feature_event = term_t(PlTerm(PlAtom(DataID::assign_data_id(f_vector))));				
				MO::feature(featureType, featurets, feature_event, feature_term);
				tail.append(PlTerm(feature_term));
			}
		}
		tail.close();
		return prologFeatList; //Returns a prolog list that will be flattened later on if empty

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
  	}
}

/*
 * A float represeting the timestamp in sec for better management as prolog term. 
 */

float
vmpl_timestamp_float(Vamp::RealTime rt){

	return ((float)rt.sec + (float)((float)rt.nsec)/1000000000);
	
}

/*
 * To save lines of code
 */

PlAtom
vmpl_string_to_atom(string x){

	int length = x.size();
	char y[length+1];
	for(int j=0;j<length;j++){
		y[j]=x[j];
	}	
	y[length] = '\000';

	return PlAtom(y);
}

