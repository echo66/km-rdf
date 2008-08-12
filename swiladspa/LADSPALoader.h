/**
	LADSPA plugins Loader
        This code is mainly inspired in the Sonic Visualiser host (Chris Cannam)

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
	*/

#ifndef _LADSPA_LOADER_H_
#define _LADSPA_LOADER_H_

#include <dlfcn.h> 

//JUST FOR LINUX!!!

#define PLUGIN_GLOB  "*.so"
#define PATH_SEPARATOR ':'
#define DEFAULT_LADSPA_PATH "$HOME/ladspa:$HOME/.ladspa:/usr/local/lib/ladspa:/usr/lib/ladspa"

#define MUNLOCKALL() ::munlockall()
#define DLOPEN(a,b)  dlopen((a).toStdString().c_str(),(b))
#define DLSYM(a,b)   dlsym((a),(b))
#define DLCLOSE(a)   dlclose((a))
#define DLERROR()    dlerror()

 
#include <swiladspa.h>
#include <vector>
#include <map>
#include <set>
#include <qstring.h>

class LADSPALoader
{
public:
	~LADSPALoader();
	 LADSPALoader();

	//Quering plugin descriptor
	std::vector<std::string> ladspa_plugins();
	int plugin_parameter_count(std::string);
	std::string plugin_maker(std::string);
	std::string plugin_soname(std::string);
	std::string plugin_name(std::string);
	std::vector<int> inputAudio_ports(std::string);
	std::vector<int> outputAudio_ports(std::string);
	std::vector<int> inputControl_ports(std::string);
	std::vector<int> outputControl_ports(std::string);
	unsigned long plugin_ports_count(std::string);
	std::string port_name(std::string, int);	

	//Loading a plugin
	LADSPAPlugin::LADSPAPlugin *instantiate(std::string, size_t, size_t);
	void connect_audio_ports(LADSPAPlugin::LADSPAPlugin *);
	void set_default_controls(LADSPAPlugin::LADSPAPlugin *);

	//passing data
	void set_parameter(LADSPAPlugin::LADSPAPlugin *, int, LADSPA_Data);
	void set_output_control(LADSPAPlugin::LADSPAPlugin *, int, LADSPA_Data);

protected:   

     std::vector<QString> getPluginPath();
     void discoverPlugins();
     void discoverPlugins(QString soName);
     //void releasePlugin(LADSPA_Handle *, QString);
     const LADSPA_Descriptor *getLADSPADescriptor(std::string identifier); 
     void loadLibrary(QString soName);
     void unloadLibrary(QString soName);
     //void unloadUnusedLibraries();

     void read_ports(const LADSPA_Descriptor *, int);
     std::string ports_names(const LADSPA_Descriptor *, int);	

     typedef std::map<QString, void *> LibraryHandleMap;
     LibraryHandleMap m_libmap;

};

#endif

