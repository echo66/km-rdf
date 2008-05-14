/**
	PRolog interface LADSPA Plugins
	David PAstor 2008
*/

#include <LADSPALoader.h>

/*************************************************** VARIABLES *****************************************/

/**
	One loader for any plugin
*/
LADSPALoader::LADSPALoader *l_loader;

/**
	Map of output. One working plugin?? Do I want to output a whole signal back???
	extend it to cover input singls or something???
*/

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
******** MANAGE THE PLUGIN ************************
**************************************************/

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

	term_t plugin_t;
	plugin_t = PL_new_term_ref();
	ldpl_register_plugin(plugin, ident, plugin_t);

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
	plugin->LADSPAPlugin::activate();

	return true;
}


/**
	Run
*/
PREDICATE(ldpl_run_plugin, 3)
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
PREDICATE(ldpl_collect_output, 1)
{
	//+plugin
	string ident;
	LADSPAPlugin::LADSPAPlugin *plugin;	

	ldpl_get_plugin(term_t(A1), plugin, ident);

	std::vector<int> outputs = l_loader->LADSPALoader::outputAudio_ports(ident);
	std::vector<float> data;

	for(int j=0; j<outputs.size(); j++){

		data = ladspa_output.find(outputs[j])
	}

}
*/

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
		const char *id = DataID::assign_data_id(v);
		tail.append(PlAtom(id));
	}
	tail.close();
	return A2 = outputData;
}






