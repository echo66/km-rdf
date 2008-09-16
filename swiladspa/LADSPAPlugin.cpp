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

	m_name = name;
	m_inAudio = ia;
	m_outAudio = oa;
	m_inControl = ic;
	m_outControl = oc;
	m_sampleRate = sr;
	m_blockSize = b;
	m_descriptor = des;
	
	init_buffers();
	plugin = des->instantiate(des, sr);
	std::cerr<<"plugAdd"<<plugin<<std::endl;
}

/***********************************************
************* PLUGIN PROCESS *******************
***********************************************/

/**
	Activate plugin
*/
int
LADSPAPlugin::activate(){

	if(!m_descriptor -> activate){
		std::cerr << "Plugin: no activate() routine"<<std::endl;
		return -1;
	}
	m_descriptor -> activate(plugin);
	std::cerr << "LADSPAPlugin: activated"<<std::endl;

	return 0;
}

/**
	run plugin
*/
void
LADSPAPlugin::run(size_t blockSize){

	std::cerr<<blockSize<<std::endl;
	std::cerr<<"plugAdd"<<plugin<<std::endl;
	std::cerr<<"port in1 val"<<inputbuffers[0][0]<<std::endl;
	std::cerr<<"port out1 val"<<outputbuffers[0][0]<<std::endl;

	std::cerr<<sizeof(inputbuffers)<<std::endl;
	std::cerr<<sizeof(inputbuffers[0])<<std::endl;
	//std::cerr<<sizeof(inputbuffers*)<<std::endl;

	if(!m_descriptor -> run){
		std::cerr << "Plugin: no process routine"<<std::endl;
		return;
	}
	std::cerr << "se supone q debe funcionar"<< std::endl;
	m_descriptor -> run(plugin, (unsigned long)blockSize);
	std::cerr << "block processed"<<std::endl;

	std::cerr << "printing in first channel "<<std::endl;
	for(int j=0; j<5; j++){
		std::cerr << inputbuffers[0][j] << std::endl;
	}	

	std::cerr << "printing out first channel "<<std::endl;
	for(int j=0; j<5; j++){
		std::cerr << outputbuffers[0][j] << std::endl;
	}	
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
	std::cerr << "LADSPAPlugin: deactivated"<<std::endl;
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
	
	for(int k=0;k<m_inAudio;k++){

		delete[] inputbuffers[k];
	}
	delete[] inputbuffers;
	inputbuffers=0;
	for(int k=0;k<m_outAudio;k++){

		delete[] outputbuffers[k];
	}
	delete[] outputbuffers;
	outputbuffers=0;

	//We should also dlclose the library
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

	std::cerr<<"LADSPAPlugin: initializing buffers"<<std::endl;	

	//std::cerr<<m_inAudio<<std::endl;
	//std::cerr<<m_outAudio<<std::endl;

	if (m_inAudio == 0) {
		inputbuffers = 0;
	} else {
		inputbuffers  = new LADSPA_Data*[m_inAudio];
	}
	for (size_t i = 0; i < m_inAudio; ++i) {
		inputbuffers[i] = new LADSPA_Data[m_blockSize];
	}

	if (m_outAudio == 0) {
		outputbuffers = 0;
	} else {
		outputbuffers  = new LADSPA_Data*[m_outAudio];
	}
	for (size_t i = 0; i < m_outAudio; ++i) {
		outputbuffers[i] = new LADSPA_Data[m_blockSize];
		for(size_t k = 0; k<m_blockSize; k++){
			outputbuffers[i][k]=0;//init value is set to 0 (may work better)
		}
	}
	/**outputbuffers = (LADSPA_Data **)calloc(m_outAudio, sizeof(LADSPA_Data *));
  	for (int s = 0; s<m_outAudio; s++){
	    outputbuffers[s]     = (LADSPA_Data *)calloc(m_blockSize, sizeof(LADSPA_Data));
	}*/
	//Fill with zeros???

	std::cerr<<"port in1 ad"<<inputbuffers[0]<<std::endl;
	std::cerr<<"port in2 ad"<<inputbuffers[1]<<std::endl;

	std::cerr<<"port out1 ad"<<outputbuffers[0]<<std::endl;
	std::cerr<<"port out2 ad"<<outputbuffers[1]<<std::endl;

	std::cerr<<"port in1,1value"<<inputbuffers[0][0]<<std::endl; //Random value!!!!!!
	std::cerr<<"port in2,1 value"<<inputbuffers[1][0]<<std::endl;
	std::cerr<<"port out1,1 value"<<outputbuffers[0][0]<<std::endl; 
	std::cerr<<"port out2,1 value"<<outputbuffers[1][0]<<std::endl; 
}

/**
	Connect audio ports
*/
void
LADSPAPlugin::connect_input_port(int port, int index){

	LADSPA_PortDescriptor pd = m_descriptor->PortDescriptors[port];
      	std::cerr<<"plugAdd"<<plugin<<std::endl;
        if (LADSPA_IS_PORT_AUDIO(pd)) {
	 	if (LADSPA_IS_PORT_INPUT(pd)) {
			if(m_descriptor -> connect_port){
				std::cerr<< "LADSPAPlugin: connecting input port "<<port<<" to the buffer at "<<inputbuffers[index]<<std::endl;
				m_descriptor -> connect_port(plugin, 0, inputbuffers[index]);	
			}else{
				std::cerr<<"no port connection routine"<<std::endl;
			}
		}else{
			std::cerr<<"WARNING: this port is not an input port"<<std::endl;
		}
	}else{
		std::cerr<<"WARNING: this port is not an audio port"<<std::endl;
	}
}

/**
	Connect just one port. The loader has the information to connect the plugin to its ports.
*/
void
LADSPAPlugin::connect_output_port(int port, int index){

	LADSPA_PortDescriptor pd = m_descriptor->PortDescriptors[port];
      
        if (LADSPA_IS_PORT_AUDIO(pd)) {
	 	if (LADSPA_IS_PORT_OUTPUT(pd)) {
	
			std::cerr<< "LADSPAPlugin: connecting output port "<<port<<" to the buffer at "<<outputbuffers[index]<<std::endl;
			if(m_descriptor -> connect_port){
				m_descriptor -> connect_port(plugin, port, outputbuffers[index]);	
			}else{
				std::cerr<<"no port connection routine"<<std::endl;
			}
		}else{
			std::cerr<<"WARNING: this port is not an output port"<<std::endl;
		}
	}else{
		std::cerr<<"WARNING: this port is not an audio port"<<std::endl;
	}
}

/**
	Set control port (in (parameter) or out (outputcontrol))
*/
void
LADSPAPlugin::set_control_port(int port, LADSPA_Data *value){

	LADSPA_PortDescriptor pd = m_descriptor->PortDescriptors[port];
	if (LADSPA_IS_PORT_CONTROL(pd)) {
	 	if(m_descriptor -> connect_port){

			std::cerr<<"LADSPAPlugin: setting control port "<<port<<" to "<<*value<<std::endl;
			m_descriptor -> connect_port(plugin, 2, value);	
		}else{
			std::cerr<<"LADSPAPlugin: no port connection routine"<<std::endl;
		}
	}else{
		std::cerr<<"WARNING: this port is not a control port"<<std::endl;
	}	
}


/******************************************
************ DATA *************************
******************************************/

/**
	Return output selected
*/
LADSPA_Data **
LADSPAPlugin::get_output(){

	std::cerr<<"port out1 ad"<<outputbuffers[0]<<std::endl;
	std::cerr<<"port out2 ad"<<outputbuffers[1]<<std::endl;

	std::cerr<<"port in1 ad"<<inputbuffers[0]<<std::endl;
	std::cerr<<"port in2 ad"<<inputbuffers[1]<<std::endl;

	std::cerr<<"port in1,1 value"<<inputbuffers[0][0]<<std::endl; //Random value!!!!!!
	std::cerr<<"port in2,1 value"<<inputbuffers[1][0]<<std::endl;
	std::cerr<<"port out1,1 value"<<outputbuffers[0][0]<<std::endl; 
	std::cerr<<"port out2,1 value"<<outputbuffers[1][0]<<std::endl; 

	return outputbuffers;
}

/**
	Get audio input
*/
LADSPA_Data **
LADSPAPlugin::get_audio_input(){

	std::cerr<<"port in1 ad"<<inputbuffers[0]<<std::endl;
	std::cerr<<"port in2 ad"<<inputbuffers[1]<<std::endl;
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
