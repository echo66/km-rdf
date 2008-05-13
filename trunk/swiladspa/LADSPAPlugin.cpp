
/**
	LADSPA Plugin Loader. This class is in charge of scanning system for plugins and providing a public interface for a SWI-Prolog host to deal 
	with the plugins.
	Some part of the code is taken from the Sonic Visualiser code (Chris Cannam)
	David Pastor 2008, c4dm, Queen Mary University of London.
*/

#ifndef _LADSPA_PLUGIN_
#define _LADSPA_PLUGIN_


#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <LADSPAPlugin.h>
#include <qdir.h>
#include <qfile.h>

/**
	Cheap init at loading
*/
LADSPAPlugin::LADSPAPlugin(std::string name, const LADSPA_Descriptor *des, size_t sr, size_t ia, size_t oa, size_t ic, size_t oc, size_t b){

	m_name = name;
	m_inAudio = ia;
	m_outAudio = oa;
	m_inControl = ic;
	m_outControl = oc;
	m_sampleRate = sr;
	m_blockSize = b;
	
	init_buffers();
	plugin = desc->instantiate(desc, sr);

}

/***********************************************
************* PLUGIN PROCESS *******************
***********************************************/

/**
	Activate plugin
*/
void
LADSPAPlugin::activate(){

	m_descriptor -> activate(plugin);
}

/**
	run plugin
*/
void
LADSPAPlugin::run(size_t blockSize){

	m_descriptor -> run(plugin, blockSize);
}

/**
	deactivate plugin
*/
void
LADSPAPlugin::deactivate(){

	m_descriptor -> deactivate(plugin);
}

/**
	cleanup plugin
*/
void
LADSPAPlugin::cleanup(){

	m_descriptor -> cleanup(plugin);
	plugin = 0;
}

/*************************************************
***************** PORTS **************************
*************************************************/

/**
	Set ports for the specific instance
*/
void
LADSPAPlugin::init_buffers(){

	if (inCount == 0) {
		inputbuffers = 0;
	} else {
		inputbuffers  = new LADSPA_Data*[m_inAudio];
	}
	if (outCount == 0) {
		outputbuffers = 0;
	} else {
		outputbuffers = new LADSPA_Data*[m_outAudio];
	}

	for (size_t i = 0; i < m_inAudio; ++i) {
		inputbuffers[i] = new LADSPA_Data[blockSize];
	}
	for (size_t i = 0; i < m_outAudio; ++i) {
	         outputbuffers[i] = new LADSPA_Data[blockSize];
	}
}

/**
	Connect audio ports
*/
void
LADPSAPlugin::connect_input_port(int port, int index){

	m_descriptor -> connect_port(plugin, port, inputbuffers[index]);	
}

/**
	Connect just one port. The loader has the information to connect the plugin to its ports.
*/
void
LADPSAPlugin::connect_output_port(int port, int index){

	m_descriptor -> connect_port(plugin, port, outputbuffers[index]);	
}

/**
	Set control port (in or out)
*/
void
LADSPAPlugin::set_control_port(int port, LADSPA_Data value){

	m_descriptor -> connect_port(plugin, port, &value);
}


/******************************************
************ DATA *************************
******************************************/

/**
	Return output selected
*/
LADSPA_Data *
LADSPAPlugin::get_output(int o){

	return outputbuffers[o];
}

/**
	Get audio input
*/
LADSPA_Data **
LADSPAPlugin::get_audio_input(){

	return inputbuffers;
}


/**
	METADATA
*/
std::string
LADSPAPlugin::get_name(){
	return m_name;
}

size_t
LADSPAPlugin::get_blockSize(){
	return m_blockSize;
}


