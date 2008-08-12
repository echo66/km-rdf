/**
	LADSPA Plugins wrapper.

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
	*/


#ifndef _LADSPA_PLUGIN_H_
#define _LADSPA_PLUGIN_H_

#include <dlfcn.h> 

#include <ladspa.h>
#include <vector>
#include <map>
#include <set>
#include <qstring.h>

class LADSPAPlugin
{
public:
	~LADSPAPlugin();//Implement this, clean buffers.
	LADSPAPlugin(std::string, const LADSPA_Descriptor *, size_t , size_t , size_t ,size_t , size_t , size_t);

	//plugin data
	std::string get_name();
	size_t get_blockSize();

	//dynamic ports management
	void set_control_port(int, LADSPA_Data);

	//static ports management
	void init_buffers();
	void connect_input_port(int, int);//this can not be done only be the plugin instance. Needs loader's help (may be weird)
	void connect_output_port(int, int);

	//plugin	
	void activate();
	void run(size_t);
	void deactivate();
	void cleanup();

	//get data
	LADSPA_Data* get_output(int);
	LADSPA_Data **get_audio_input();

protected:   

	std::string m_name; //identifier used in this host ladspa-plugin::label

	//The wrapped plugin
	LADSPA_Handle plugin;
	
	//Buffers
	LADSPA_Data **inputbuffers;
	LADSPA_Data **outputbuffers;

	//Parameters
	size_t m_inAudio;
	size_t m_outAudio;
	size_t m_inControl;
	size_t m_outControl;
	size_t m_blockSize;
	size_t m_sampleRate;

	const LADSPA_Descriptor *m_descriptor;

};

#endif
