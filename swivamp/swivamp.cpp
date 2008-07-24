/**
	SWI-Prolog external interface to Vamp Plugins
	This C/C++ source defines foreign functions a library for swivamp. Mainly type-conversion stuff
	
	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2007 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
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

/* Functors for swivamp output */
functor_t timestamp_t = PL_new_functor(PL_new_atom("timestamp"), 2);//MO::timestamp
functor_t feature_t = PL_new_functor(PL_new_atom("feature"), 3);//MO::feature

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
 * This function converts the frame representation used in SWI-Prolog into a Vamp input multidimensional array to call the plugin process support n
 * channels
 */

const float* const*
vmpl_frame_to_input(int channels, size_t size, term_t fdata){

	//datalist as input. Only 2 channels

	PlTail tail(fdata);

	PlTerm ch;
	size_t index = 0;
	char *id;	
	int count = 0;

	//multidimensional array as input
	float **plugbuf = new float*[channels];
	for (int c = 0; c < channels; ++c){
		plugbuf[c] = new float[size + 2];//why + 2??????
	}

	while(tail.next(ch)){		
		PL_get_atom_chars(ch, &id);
		vector<float> *data;
		BLOBID::get_data_for_id((const char*)id, data);

		while (index < size) {
                	plugbuf[count][index] = data->at(index);
               	 	++index;
        	}
		count++;
		index = 0;
	}
	
		
	return plugbuf;//the memory is freed afterwards
}

/*
 * This predicate converts the information in C code into a Prolog functor feature(Type, timestamp(Start, DurationTRICKY), BLOBIDDAta). The returned term is a list of these features for a given input frame and an output index!

 * This still uses something defined in swimo and i may change it to output just the data and create the functor in Prolog, but i think it does not
 * cause many problems as it its now. It follows the VAMP recommendations for hosts http://www.vamp-plugins.org/guide.pdf

 * IMPORTANT: Duration of the timestamp is DOUBTFUL!! it has to be interpreted properly afterwards, here we just try to capture all the info possible
 */

term_t
vmpl_frame_features_to_prolog(Vamp::Plugin::FeatureSet fs, int output, float startf, float durationf, Vamp::Plugin::OutputDescriptor od){

	//Vamp::FeatureSet (map of a list of features for each frame/output. 
	//int output index
	//start of the original frame
	//duration of the original frame
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
					//the MO::timestamp of the frame as there is only one feature in the frame if there is not timestamp
					term_t start = PL_new_term_ref();
					term_t duration = PL_new_term_ref();
					PL_put_float(start, startf);	
					PL_put_float(duration, durationf);
					
					timestamp_functor(start, duration, featurets);
				}
				else if(stype==1){
					//If there is not timestamp
					//Start: the first one will have the same that the input frame and the rest will increase 1/osr
					//Duration: constant duration 1/osr	
					
					//if there is timestamp, we read it instead of the frame
					if(fl[j].hasTimestamp==1){
						float d = 0.0f;
						if(osr!= 0.0f){
							d = 1/osr;
						}			
						term_t start = PL_new_term_ref();
						term_t duration = PL_new_term_ref();
						PL_put_float(start, vmpl_timestamp_float(fl[j].timestamp));	
						PL_put_float(duration, d);
						timestamp_functor(start, duration, featurets);
					}else{				

						float start_point=startf + (j*(1/osr));

						term_t start = PL_new_term_ref();
						term_t duration = PL_new_term_ref();
						PL_put_float(start, start_point);	
						PL_put_float(duration, 1/osr);	
						timestamp_functor(start, duration, featurets);
					}	
					
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
					timestamp_functor(start, duration, featurets);

				}
				else{ cerr<<"Unexpected sample type"<<endl; }

				//term_t feature_event; //new id for the feature values. Id of the form __data_id
				
				//WE CAN COME BACK TO BLOBIDs INSTEAD

				//vector<float> *f_vector;
				//f_vector = new vector<float>();

				PlTerm feature_event;
				PlTail datalist(feature_event);

				for(size_t r=0; r<fl[j].values.size(); r++){
					datalist.append((double)fl[j].values.at(r));
				}			
				datalist.close();				
		
				//creating an id for the data stored in memory
				//feature_event = term_t(PlTerm(PlAtom(BLOBID::assign_data_id(f_vector))));				
				feature_functor(featureType, featurets, feature_event, (term_t)feature_term);//I really should change this
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

/**
	timestamp both ways are supported by this function:

	1. timestamp(+start, +duration, -timestamp_term)
	If timestamp_term is not a compound term, it creates a "timestamp" compound term given: 	
        	-starting time point (sec)
		-duration (sec)
		
	and unifies to timestamp_term obtaining:
										
					'timestamp'(start, duration)

	2. timestamp(-start, -duration, +timestamp_term) 
	This way it analyzes and extracts the data from the term if timestamp_term does refer to a compound term 
	(cheking that it is the correct name)
*/

void
timestamp_functor(term_t start, term_t duration, term_t timestamp){

	//now, signal_term is checked out. If the results ==0, it means that there is no compound term, so the term is created and unified to it
	if(PL_is_functor(timestamp, timestamp_t)==0){

		//Construct and unifies the term
		PL_cons_functor(timestamp, timestamp_t, start, duration);
	
	//otherwise the term is read
	}else{
		//should check name and arity for security
		PL_get_arg(1, timestamp, start);		//start is read
		PL_get_arg(2, timestamp, duration);		//duration
	}

}

/**
	feature both ways are supported by this function:
	
	1. feature(+type, +timestamp, +featureEvent, -feature)
	 	
        	-feature type
		-MO::timestamp. Can be the one for the frame or a specific one!!!
		-featureEvent: Encapsulates the values of the feature!!
		
	and unifies to feature_term obtaining:
										
					'feature'(type, MO::timestamp, Event)

	2. feature(-type, -timestamp, -featureEvent, +feature)
	This way it analyzes and extracts the data from the term if feature_term does refer to a compound term 
	(cheking that it is the correct name)
*/

void
feature_functor(term_t type, term_t timestamp, term_t featureEvent, term_t feature_term){

	//now, signal_term is checked out. If the results ==0, it means that there is no compound term, so the term is created and unified to it
	if(PL_is_functor(feature_term, feature_t)==0){
		
	//Construct and unifies the term
	PL_cons_functor(feature_term, feature_t, type, timestamp, featureEvent);
		
	//otherwise the term is read
	}else{
		//should check name and arity for security
		PL_get_arg(1, feature_term, type);		
		PL_get_arg(2, feature_term, timestamp);	
		PL_get_arg(3, feature_term, featureEvent);			
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

