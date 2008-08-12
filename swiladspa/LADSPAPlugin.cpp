/**
	LADSPA Plugins wrapper.

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
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

	std::cerr << "aqui"<< std::endl;
	m_name = name;
	m_inAudio = ia;
	m_outAudio = oa;
	m_inControl = ic;
	m_outControl = oc;
	m_sampleRate = sr;
	m_blockSize = b;
	m_descriptor = des;
	
	init_buffers();
	std::cerr<<"done"<<std::endl;
	plugin = des->instantiate(des, sr);
	std::cerr<<"done2"<<std::endl;

}

/***********************************************
************* PLUGIN PROCESS *******************
***********************************************/

/**
	Activate plugin
*/
void
LADSPAPlugin::activate(){

	if(!m_descriptor -> activate){
		std::cerr << "Plugin: no activate() routine"<<std::endl;
		return;
	}
	m_descriptor -> activate(plugin);
	std::cerr << "Plugin activated"<<std::endl;
}

/**
	run plugin
*/
void
LADSPAPlugin::run(size_t blockSize){

	if(!m_descriptor -> run){
		std::cerr << "Plugin: no process routine"<<std::endl;
		return;
	}
	m_descriptor -> run(plugin, (unsigned long)blockSize);
	std::cerr << "block processed"<<std::endl;
}

/**
	deactivate plugin
*/
void
LADSPAPlugin::deactivate(){

	if(!m_descriptor -> deactivate){
		std::cerr << "Plugin: no deactivate() routine"<<std::endl;
		return;
	}
	m_descriptor -> deactivate(plugin);
	std::cerr << "Plugin deactivated"<<std::endl;
}

/**
	cleanup plugin
*/
void
LADSPAPlugin::cleanup(){

	if(!m_descriptor -> cleanup){
		std::cerr << "Plugin: no cleanup() routine"<<std::endl;
		return;
	}
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

	std::cerr<<"init buffers"<<std::endl;	
	if (m_inAudio == 0) {
		inputbuffers = 0;
	} else {
		inputbuffers  = new LADSPA_Data*[m_inAudio];
	}
	if (m_outAudio == 0) {
		outputbuffers = 0;
	} else {
		outputbuffers = new LADSPA_Data*[m_outAudio];
	}

	for (size_t i = 0; i < m_inAudio; ++i) {
		inputbuffers[i] = new LADSPA_Data[m_blockSize];
	}
	for (size_t i = 0; i < m_outAudio; ++i) {
	         outputbuffers[i] = new LADSPA_Data[m_blockSize];
	}
}

/**
	Connect audio ports
*/
void
LADSPAPlugin::connect_input_port(int port, int index){

	if(m_descriptor -> connect_port){
		m_descriptor -> connect_port(plugin, port, inputbuffers[index]);	
	}else{
		std::cerr<<"no port connection routine"<<std::endl;
	}
}

/**
	Connect just one port. The loader has the information to connect the plugin to its ports.
*/
void
LADSPAPlugin::connect_output_port(int port, int index){

	if(m_descriptor -> connect_port){
		m_descriptor -> connect_port(plugin, port, outputbuffers[index]);	
	}else{
			std::cerr<<"no port connection routine"<<std::endl;
	}
}

/**
	Set control port (in or out)
*/
void
LADSPAPlugin::set_control_port(int port, LADSPA_Data value){

	if(m_descriptor -> connect_port){
		m_descriptor -> connect_port(plugin, port, &value);	
	}else{
		std::cerr<<"no port connection routine"<<std::endl;
	}
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

#endif
