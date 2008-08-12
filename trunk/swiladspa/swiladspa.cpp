/**
	LADSPA/SWI-Prolog foreign interface. This source deals with working plugin instances.

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
	*/

#include <swiladspa.h>
#include <iostream>

static struct SwiLADSPAPlugin{

	/* id: __plugin_id */
	string id;
	/* pointer a LADSPA Plugin */
	LADSPAPlugin::LADSPAPlugin *plugin;
	/* name of the plugin */
	string type;//we need this to retrieve the descriptor
}
					
/*
 * Database with all the entries id/plugin. This is a database of working plugins.
 */
LADSPA_plugins_db[MAX_LADSPA_PLUGIN];

/* Variables */
size_t active_plugins = 0;

using namespace std;

				/************************************************************************
 				******************* C Interface functions implementation ****************
				************************************************************************/

/** Some functions to deal with the database **/

/*
 * Stores the instance of the plugin when loading a ladspa plugin and returns an id that will be the handle for the plugin onwards.
 */

int
ldpl_register_plugin(LADSPAPlugin::LADSPAPlugin *plugin, string type, term_t id){

	LADSPA_plugins_db[active_plugins].plugin = plugin;
	LADSPA_plugins_db[active_plugins].type = type;
	LADSPA_plugins_db[active_plugins].id = ldpl_id_for_ladspa();
	
	PL_unify(id, term_t(PlTerm(PlAtom((LADSPA_plugins_db[active_plugins].id).data()))));
	active_plugins++;
	return 0; //success
}

/*
 * Creates a simple id incrementally for the plugins __ladspa::plugin_id
 */

string
ldpl_id_for_ladspa()
{
	string head("__ladspa::plugin_");

	//incremental id for blobs	
	string var = (const char*)active_plugins;
	return head+var;

}

/*
 * Gets the plugin for the id given
 */

int
ldpl_get_plugin(term_t id_t, LADSPAPlugin::LADSPAPlugin * &plugin, string &type){

	char *id;
	PL_get_atom_chars(id_t,&id);
	string qid((const char*)id);
	
	for(size_t r=0; r<MAX_LADSPA_PLUGIN; r++){
		
		if(qid.compare(LADSPA_plugins_db[r].id)==0){
			plugin = LADSPA_plugins_db[r].plugin;
			type = LADSPA_plugins_db[r].type;
			return 0;
		}
	}
	return -1;	
}

/************************************ TYPE CONVERSION ******************************/

/**Take a list of data ids (framed stuff) and set the buff**/
int
ldpl_set_input_buffers(term_t data, LADSPA_Data **buf, int ports, size_t block){

	PlTail tail(data);
	PlTerm ch;
	unsigned int chCount=0;
	while(tail.next(ch)){		
		chCount++;
	}
	if(chCount>ports) { std::cerr<<"More channels than input ports"<<std::endl; }
	else if(chCount<ports) { 
		std::cerr << "Ports not filled!!" << std::endl; 
		return -1;
	}
	else{}

	PlTail frame(data);
	PlTerm pcm;
	int data_blocks = 1;
	for(int j = 0; j < ports; j++){

		frame.next(pcm);
		char *id;//atom to const char *
		PL_get_atom_chars(pcm, &id);

		//Now we retrieve the pointers to the raw data in memory
		vector<float> *vector;
		BLOBID::get_data_for_id((const char *)id, vector);

		//Vector should have blocksize length...
		
		for(int r =0 ; r<block; r++){	
			std::cerr<<"count"<<r<<std::endl;
			std::cerr<<"block"<<block<<std::endl;
			std::cerr<<"vector"<<vector->size()<<std::endl;	
			if(vector->size()<r){
				std::cerr<<"last frame may be smaller than a block size"<<std::endl;
				buf[j][r] = 0;//filling with 0, but should be able to pass correct blocks.
			}else{
				//filling buffers
				std::cerr<<vector->at(r)<<std::endl;
				buf[j][r] = vector->at(r);
			}
		}
	}	
	return 1;
}

/*
 * util
 */
PlAtom
ldpl_string_to_atom(string x){

	int length = x.size();
	char y[length+1];
	for(int j=0;j<length;j++){
		y[j]=x[j];
	}	
	y[length] = '\000';

	return PlAtom(y);
}

/*
 * Creates a plugin key (simarity with swivamp)
 *//*
 * Creates a plugin key (simarity with swivamp)
 */
string
ldpl_plugin_key(string label){

	string head("ladspa-plugin::");//put libraries here?

	return head+label;
}

string
ldpl_plugin_key(string lib, string label){

	string key;
	key+=lib;
	key+=":";
	key+=label;
	return key;
}








