/**
	Host for LADSPA Plugins. 

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
	*/

#include <LADSPALoader.h>
#include <iostream>

/*************************************************** VARIABLES *****************************************/

/**
	One loader for any plugin
*/
LADSPALoader::LADSPALoader *l_loader;

/**
	Map of output. One working plugin?? Do I want to output a whole signal back???
	extend it to cover input singls or something???
*/

std::vector<float> *out1;
std::vector<float> *out2;

/************************************ PREDICATES ******************************/
/**
	Get a LADSPA plugins loader
*/
PREDICATE(ldpl_loader, 0){

	l_loader = new LADSPALoader::LADSPALoader();
	return true;	
}

/**
	This predicate lists the plugins in the systems returning a list of identifiers (better to use labels?)
*/
PREDICATE(ldpl_plugins, 1)
{
	//-listOfLADSPAPlugins
	std::vector<std::string> ladspaList = l_loader->ladspa_plugins();

	PlTerm list;
	PlTail tail(list);
	for(unsigned int j= 0; j<ladspaList.size(); j++){

	tail.append(PlTerm((const char*)ladspaList[j].data()));
	}
	tail.close();
	return A1 = list;

}

/*********************************************
*** QUERYING PLUGIN DESCRIPTION **************
*********************************************/

/**
	Plugin maker. We just need the identifier of the plugin.
*/
PREDICATE(ldpl_plugin_maker,  2)
{
	//+plugin identifier
	//-maker

	std::string ident((char *)A1);


	string author = l_loader->LADSPALoader::plugin_maker(ident);
	char maker[author.size()+1];
	for(unsigned int j=0;j<author.size();j++){
		maker[j]=author[j];
	}
	maker[author.size()] = '\000';
	return A2 = PlAtom(maker);
}

/**
	Returns library owning the plugin
*/
PREDICATE(ldpl_plugin_library,  2)
{
	//+plugin identifier
	//-library

	std::string ident((char *)A1);

	string author = l_loader->LADSPALoader::plugin_soname(ident);
	char maker[author.size()+1];
	for(unsigned int j=0;j<author.size();j++){
		maker[j]=author[j];
	}
	maker[author.size()] = '\000';
	return A2 = PlAtom(maker);
}

/**
	Ports stuff
*/

PREDICATE(ldpl_plugin_ports_count,  2)
{
	//+plugin identifier
	//-parameters count

	std::string ident((char *)A1);

	long count = l_loader->LADSPALoader::plugin_ports_count(ident);
	return A2 = PlTerm(count);
}

/**
	Get the key of a working plugin
*/

PREDICATE(ldpl_get_key, 2)
{
	//+Plugin
	//-PluginKey
	
	std::string ident;
	LADSPAPlugin::LADSPAPlugin *plugin;
	ldpl_get_plugin(term_t(A1), plugin, ident);

	string key = plugin->m_name;

	char k[key.size()+1];
	for(unsigned int j=0;j<key.size();j++){
		k[j]=key[j];
	}
	k[key.size()] = '\000';

	return A2 = PlAtom(k);
}	

/*************************************************
******************* PORTS ************************
*************************************************/

PREDICATE(ldpl_input_audio, 2)
{
	//+plugin name
	//-list of input audio ports (index of it)

	std::string ident((char *)A1);
	std::vector<int> input = l_loader->LADSPALoader::inputAudio_ports(ident);
	
	PlTerm list;
	PlTail tail(list);
	for(int j=0; j<input.size();j++){
		
		tail.append((long)input[j]);
	}
	tail.close();
	return A2 = list;
}

PREDICATE(ldpl_output_audio, 2)
{
	//+plugin name
	//-list of output audio ports (index of it)

	std::string ident((char *)A1);
	std::vector<int> input = l_loader->LADSPALoader::outputAudio_ports(ident);
	
	PlTerm list;
	PlTail tail(list);
	for(int j=0; j<input.size();j++){
		
		tail.append((long)input[j]);
	}
	tail.close();
	return A2 = list;
}

PREDICATE(ldpl_input_control, 2)
{
	//+plugin name
	//-list of input control ports (index of it)

	std::string ident((char *)A1);
	std::vector<int> input = l_loader->LADSPALoader::inputControl_ports(ident);
	
	PlTerm list;
	PlTail tail(list);
	for(int j=0; j<input.size();j++){
		
		tail.append((long)input[j]);
	}
	tail.close();
	return A2 = list;
}

PREDICATE(ldpl_output_control, 2)
{
	//+plugin name
	//-list of output control ports (index of it)

	std::string ident((char *)A1);
	std::vector<int> input = l_loader->LADSPALoader::outputControl_ports(ident);
	
	PlTerm list;
	PlTail tail(list);
	for(int j=0; j<input.size();j++){
		
		tail.append((long)input[j]);
	}
	tail.close();
	return A2 = list;
}

PREDICATE(ldpl_port_name_for, 3)
{
	//+plugin name
	//-index
	//-name

	std::string ident((char *)A1);
	std::string name = l_loader->LADSPALoader::port_name(ident, (int)A2);

	return A3 = PlTerm(PlAtom(name.data()));		
}

/**************************************************
******** MANAGE THE PLUGIN ************************/

/**
	Instantiate plugin
*/
PREDICATE(ldpl_instantiate_plugin, 4)
{
	//+ident
	//+sr
	//+blockSize
	//-plugin blob

	std::string ident((char *)A1);
	unsigned long sr = (unsigned long)(long)A2;
	size_t bsize = (size_t)(long)A3;
	
	LADSPAPlugin::LADSPAPlugin *plugin = l_loader->LADSPALoader::instantiate(ident, sr, bsize);

	std::cerr<<"instantiated"<<std::endl;
	term_t plugin_t;
	plugin_t = PL_new_term_ref();
	ldpl_register_plugin(plugin, ident, plugin_t);


	//init new outputs
	out1 = 0;
	out2 = 0;
	
	out1 = new std::vector<float>(bsize);
	out2 = new std::vector<float>(bsize);
	return A4 = PlTerm(plugin_t);
}

/**
 	connect ports. Should be done just after instantiation.
*/
PREDICATE(ldpl_connect_ports, 1)
{
	//+plugin
	std::string ident;
	LADSPAPlugin::LADSPAPlugin *plugin;
	ldpl_get_plugin(term_t(A1), plugin, ident);

	l_loader->LADSPALoader::connect_audio_ports(plugin);

	return true;

}

/**
	Default controls
*/
PREDICATE(ldpl_set_default_controls, 1)
{
	//+plugin
	std::string ident;
	LADSPAPlugin::LADSPAPlugin *plugin;
	ldpl_get_plugin(term_t(A1), plugin, ident);
	l_loader->LADSPALoader::set_default_controls(plugin);
	return true;
}

/**
	Set a parameter
*/
PREDICATE(ldpl_set_parameter, 3)
{
	//+plugin
	//+input control port
	//+value
	std::string ident;
	LADSPAPlugin::LADSPAPlugin *plugin;
	ldpl_get_plugin(term_t(A1), plugin, ident);
	l_loader->LADSPALoader::set_parameter(plugin, int(A2), (float)(double)A3);
	return true;
}

/**
	Set an output control
*/
PREDICATE(ldpl_set_output_control, 3)
{
	//+plugin
	//+output control port
	//+value
	std::string ident;
	LADSPAPlugin::LADSPAPlugin *plugin;
	ldpl_get_plugin(term_t(A1), plugin, ident);
	l_loader->LADSPALoader::set_output_control(plugin, int(A2), (float)(double)A3);
	return true;
}


/**
	Init plugin
*/
PREDICATE(ldpl_activate_plugin, 1)
{
	//+pluginblob
	string ident;
	LADSPAPlugin::LADSPAPlugin *plugin;	

	ldpl_get_plugin(term_t(A1), plugin, ident);
	if(plugin->LADSPAPlugin::activate()<0) return false;

	return true;
}

/**
	Run
*/
PREDICATE(ldpl_run_plugin_framing, 4)
{
	//+Signal data list
	//+pluginblob
	//+Start
	//+BlockSize

	string ident;
	LADSPAPlugin::LADSPAPlugin *plugin;	
	ldpl_get_plugin(term_t(A2), plugin, ident);

	LADSPA_Data **bufs = plugin -> LADSPAPlugin::get_audio_input();
		
	//cleaning buffers (in case)
	int ports = l_loader->LADSPALoader::inputAudio_ports(ident).size();
	for (size_t j=0; j<ports; j++){
		delete [] bufs[j]; //is ok like this?
		bufs[j] = new LADSPA_Data[(long)A4];
	}	

	//Setting input buffers 
	if(ldpl_set_input_buffers(term_t(A1), bufs, l_loader->LADSPALoader::inputAudio_ports(ident).size(), (size_t)(long)A3, (size_t)(long)A4)<0) return false;
	std::cerr<<"input set"<<std::endl;
	plugin->LADSPAPlugin::run((size_t)(long)A3);

	return true;
}

/**
	Run
*/
PREDICATE(ldpl_run_plugin_frame, 3)
{
	//+List of PCM channels
	//+pluginblob
	//+BlockSize

	string ident;
	LADSPAPlugin::LADSPAPlugin *plugin;	
	ldpl_get_plugin(term_t(A2), plugin, ident);

		LADSPA_Data **bufs = plugin -> LADSPAPlugin::get_audio_input();
		
	//cleaning buffers (in case)
	int ports = l_loader->LADSPALoader::inputAudio_ports(ident).size();
	for (size_t j=0; j<ports; j++){
		delete [] bufs[j]; //is ok like this?
		bufs[j] = new LADSPA_Data[(long)A3];
	}	

	//Setting input buffers 
	if(ldpl_set_input_buffers(term_t(A1), bufs, l_loader->LADSPALoader::inputAudio_ports(ident).size(), (size_t)(long)A3)<0) return false;

	plugin->LADSPAPlugin::run((size_t)(long)A3);

	return true;
}

/**
	deactivate plugin
*/
PREDICATE(ldpl_deactivate_plugin, 1)
{
	//+pluginblob
	string ident;
	LADSPAPlugin::LADSPAPlugin *plugin;	

	ldpl_get_plugin(term_t(A1), plugin, ident);
	plugin->LADSPAPlugin::deactivate();

	return true;
}

/**
	cleanup plugin
*/
PREDICATE(ldpl_cleanup_plugin, 1)
{
	//+pluginblob
	string ident;
	LADSPAPlugin::LADSPAPlugin *plugin;	

	ldpl_get_plugin(term_t(A1), plugin, ident);
	plugin->LADSPAPlugin::cleanup();

	return true;

	//CLEAN BUFFERS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
}

/**
	collect output
*/
PREDICATE(ldpl_collect_output, 2)
{
	//+plugin
	string ident;
	LADSPAPlugin::LADSPAPlugin *plugin;	

	ldpl_get_plugin(term_t(A1), plugin, ident);

	size_t block = (size_t)(long)A2;

	std::vector<int> outputs = l_loader->LADSPALoader::outputAudio_ports(ident);

	//only process 2 channels
	for(int j=0; j<outputs.size(); j++){

		if(j==0){
					
			LADSPA_Data *data = plugin->LADSPAPlugin::get_output(outputs[j]);
			//int l = sizeof(*data)/sizeof(float);
			//std::cerr<<"port"<<outputs[j]<<std::endl;
			//std::cerr<<l<<std::endl;
			//for(int r = 0; r<block; r++){
			//	std::cerr<<"datum"<<(float)data[r]<<std::endl;		
			//}
			//delete[] data;

		/**}else{

			std::cerr<<"only 2 channels max"<<std::endl;
			return false;
		}*/
		}
		return true;
	}

}

/**
	processed_output
*/
PREDICATE(lpdl_processed_data, 1){

	PlTerm outputData;
	PlTail tail(outputData);
	
	const char *id1 = BLOBID::assign_data_id(out1);
	tail.append(PlAtom(id1));

	const char *id2 = BLOBID::assign_data_id(out2);
	tail.append(PlAtom(id2));

	tail.close();

	return A1 = outputData;

}

/**
	ldpl_return_output. 
	This one returns the output of a plugin	after calling run as a list of prolog blob ids.
*/
PREDICATE(ldpl_return_output, 2){

	string ident;
	LADSPAPlugin::LADSPAPlugin *plugin;

	ldpl_get_plugin(term_t(A1), plugin, ident);

	std::vector<int> outputs = l_loader->LADSPALoader::outputAudio_ports(ident);

	PlTerm outputData;
	PlTail tail(outputData);
	for(int j = 0; j<outputs.size(); j++){

		LADSPA_Data *data = plugin->LADSPAPlugin::get_output(outputs[j]);
		int l = sizeof(*data)/sizeof(float);

		std::vector<float> *v;
		v = new std::vector<float>;
		for(int h=0; h<l; h++){
			v->push_back((float)data[h]);
		}
		const char *id = BLOBID::assign_data_id(v);
		tail.append(PlAtom(id));
	}
	tail.close();
	return A2 = outputData;
}





