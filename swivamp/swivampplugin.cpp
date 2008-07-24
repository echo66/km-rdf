/**
	SWI-Prolog external interface to Vamp Plugins
	This C/C++ source defines foreign predicates loaded by the vamp module (direct interface with prolog).
	This interface pretends to provide any host with a set of effective queries over the plugin according to the Vamp Ontology (http://purl.org/ontology/vamp/).

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2007 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
 	*/

#include <swivamp.h>

#include <string>
#include <cmath>

using namespace std;

						/********************************************
						** PROLOG PREDICATES DESCRIBING THE PLUGIN **
						********************************************/

/*NOTE: The plugin first argument is not a Plugin Key but a real instance of a plugin!!!!!!*/

/*
 * Bunch of predicates to query meta-data of the plugin. It doesn't matter the parameters which the plugin has been loaded with
 * Anyway, they should be combined with vmpl_load_plugin/3 to work properly.
 */ 

PREDICATE(vmpl_get_maker, 2)
{
	//+Plugin: a pointer to an instance wrapped into a blob
	//-maker

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	string author = plugin->getMaker();
	char maker[author.size()+1];
	for(unsigned int j=0;j<author.size();j++){
		maker[j]=author[j];
	}
	maker[author.size()] = '\000';
	return A2 = PlAtom(maker);
}

PREDICATE(vmpl_get_identifier, 2)
{
	//+Plugin: a pointer to an instance wrapped into a blob
	//-identifier

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	return A2 = vmpl_string_to_atom(plugin->getIdentifier());
}

PREDICATE(vmpl_get_name, 2)
{
	//+Plugin: a pointer to an instance wrapped into a blob
	//-name

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	return A2 = vmpl_string_to_atom(plugin->getName());
}

PREDICATE(vmpl_get_description, 2)
{
	//+Plugin: a pointer to an instance wrapped into a blob
	//-description

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);
	
	return A2 = vmpl_string_to_atom(plugin->getDescription());
}

PREDICATE(vmpl_get_copyright, 2)
{
	//+Plugin: a pointer to an instance wrapped into a blob
	//-copyright

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	return A2 = vmpl_string_to_atom(plugin->getCopyright());
}

PREDICATE(vmpl_get_vampVersion, 2)
{
	//+Plugin: a pointer to an instance wrapped into a blob
	//-vamp version

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	int vamp = plugin->getVampApiVersion();
	return (A2 = vamp);
}

PREDICATE(vmpl_get_pluginVersion, 2)
{
	//+Plugin: a pointer to an instance wrapped into a blob
	//-plugin version

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	int version = plugin -> getPluginVersion();
	return (A2 = version);
}
							/*****************************************
                                                        ******** PLUGIN OUTPUTDESCRIPTOR *********
                                                        *****************************************/

/*
 * Predicate to get the number of outputs of one plugin. Very useful from prolog point of view
 */

PREDICATE(vmpl_plugin_numberOutputs, 2)
{
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList out = plugin -> getOutputDescriptors();
		return A2 = PlTerm((long)out.size());

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

/*
 * Single predicate to retrieve each of the outputDescriptor attributes (more flexibility). We could give the outputDescriptor wrapped into a blob
 * but there is not time-relevance on that comparing to do getOutputDescriptors I think
 */

PREDICATE(vmpl_outputDescriptor_identifier, 3)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//+number of output
	//-identifier

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList outputs = plugin -> getOutputDescriptors();
		return A3 = vmpl_string_to_atom(outputs[(int)A2].identifier);

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

PREDICATE(vmpl_outputDescriptor_name, 3)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//+number of output
	//-name

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList outputs = plugin -> getOutputDescriptors();
		return A3 = vmpl_string_to_atom(outputs[(int)A2].name);

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

PREDICATE(vmpl_outputDescriptor_description, 3)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//+number of output
	//-description

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList outputs = plugin -> getOutputDescriptors();
		return A3 = vmpl_string_to_atom(outputs[(int)A2].description);

	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

PREDICATE(vmpl_outputDescriptor_unit, 3)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//+number of output
	//-unit

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList outputs = plugin -> getOutputDescriptors();
		return A3 = vmpl_string_to_atom(outputs[(int)A2].unit);
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

/*From here one the values returned may vary depending on the initialization of the plugin */

PREDICATE(vmpl_outputDescriptor_hasFixedBinCount, 2)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//-boolean

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList outputs = plugin -> getOutputDescriptors();
		return outputs[(int)A2].hasFixedBinCount;
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

PREDICATE(vmpl_outputDescriptor_binCount, 3)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//+number of output
	//-binCount

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList outputs = plugin -> getOutputDescriptors();
		return A3 = PlTerm((long)outputs[(int)A2].binCount);
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

//Not doing binary names

PREDICATE(vmpl_outputDescriptor_hasKnownExtents, 2)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//-boolean

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList outputs = plugin -> getOutputDescriptors();
		return outputs[(int)A2].hasKnownExtents;
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

PREDICATE(vmpl_outputDescriptor_minValue, 3)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//+number of output
	//-minValue

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList outputs = plugin -> getOutputDescriptors();
		return A3 = PlTerm((double)outputs[(int)A2].minValue);
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

PREDICATE(vmpl_outputDescriptor_maxValue, 3)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//+number of output
	//-maxValue

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList outputs = plugin -> getOutputDescriptors();
		return A3 = PlTerm((double)outputs[(int)A2].maxValue);
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

PREDICATE(vmpl_outputDescriptor_isQuantized, 2)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//-boolean

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList outputs = plugin -> getOutputDescriptors();
		return outputs[(int)A2].isQuantized;
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

PREDICATE(vmpl_outputDescriptor_quantizeStep, 3)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//+number of output
	//-step

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList outputs = plugin -> getOutputDescriptors();
		return A3 = PlTerm((double)outputs[(int)A2].quantizeStep);
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

PREDICATE(vmpl_outputDescriptor_sampleType, 3)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//+number of output
	//-unit

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList outputs = plugin -> getOutputDescriptors();
		return A3 = PlTerm((long)outputs[(int)A2].sampleType);
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
	}
}

PREDICATE(vmpl_outputDescriptor_sampleRate, 3)
{	
	//+plugin (already loaded, so we just have a pointer to it)
	//+number of output
	//-sample rate

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);

	try{
		Vamp::Plugin::OutputList outputs = plugin -> getOutputDescriptors();
		return A3 = PlTerm((double)outputs[(int)A2].sampleRate);
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

							/*********************************
                                                        ******** PLUGIN PROGRAMS *********
                                                        *********************************/

/*
 * Plain list of available programs as atoms
 */

PREDICATE(vmpl_pluginPrograms, 2)
{
	//+ plugin
	//- list of the available programs

	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);
	try{
		Vamp::PluginBase::ProgramList progList = plugin -> getPrograms();
		//cerr << (int)progList.size() << " programs: " << endl;
		
		PlTerm prologProgList;
		PlTail tail(prologProgList);	

		for(unsigned int i=0;i<progList.size();i++){

			tail.append(vmpl_string_to_atom(progList[i]));
		}
		tail.close();
		return A2 = prologProgList;
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}
							/*********************************
                                                        ******* PLUGIN PARAMETERS ********
                                                        *********************************/

PREDICATE(vmpl_pluginParameters, 2)
{
	//+ plugin
	//- List of the parameters that need be set for the working plugin
	
	term_t blob = PL_new_term_ref();
	blob = term_t(PlTerm(A1));
	Vamp::Plugin *plugin;
	vmpl_get_plugin(blob, plugin);
	try{
		Vamp::PluginBase::ParameterList parList = plugin -> getParameterDescriptors();
		//cerr << (int)parList.size() << " parameters" << endl;
		
		PlTerm prologParamList;
		PlTail tail(prologParamList);	

		for(unsigned int i=0;i<parList.size();i++){
			
			tail.append(vmpl_string_to_atom(parList[i].identifier));
		}
		tail.close();
		return A2 = prologParamList;
		
	} catch ( PlException &ex )
  	{ cerr << (char *) ex << endl;
	  return FALSE;
 	}
}

