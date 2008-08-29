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


#ifndef _LADSPA_LOADER_
#define _LADSPA_LOADER_


#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <LADSPALoader.h>
#include <qdir.h>
#include <qfile.h>

/**
	Database of LADSPA plugins. They are discovered and can be queried afterwards. Active plugins are stored in blobs in another database
*/
static struct ladspa_descriptor{

	std::string name;
	std::string label;
	std::string maker;
	std::string copyright;
	std::string category;
	std::string soname; //need to keep track of the library of the plugin
	std::string key; 
	bool isSynth;
	unsigned long portCount;

	//Keep track of the ports of each plugin type (just the position within the PortDescriptor)
	//I may need to keep the name as well
	std::vector<int> inAudio;
	std::vector<int> outAudio;
	
	std::vector<int> inControl;
	std::vector<int> outControl;

	std::vector<std::pair<std::string, int> > portNames;//position of the port and its name (no type)
}

/*
 * Database of LADSPA type plugins in system (max of 100). It is filled in each session
 */
ladspa_plugins_db[100];
int plugins_sys = 0;


/****************************************
*** Querying the descriptor *************
****************************************/

std::vector<std::string>
LADSPALoader::ladspa_plugins(){

	std::vector<std::string> plugins;
	std::cerr << plugins_sys << std::endl;
	for(int j = 0; j < plugins_sys; j++){

		//std::cerr<<ladspa_plugins_db[j].label<<std::endl;
		//std::cerr<<ladspa_plugins_db[j].soname<<std::endl;
		
		plugins.push_back(ladspa_plugins_db[j].key);
	}
	return plugins;
}

std::string
LADSPALoader::plugin_maker(std::string key){

	for(int j = 0; j < plugins_sys; j++){

		if(key.compare(ladspa_plugins_db[j].key) == 0){
			return ladspa_plugins_db[j].maker;
		}
	}	
	return "";
}

std::string
LADSPALoader::plugin_soname(std::string name){

	for(int j = 0; j < plugins_sys; j++){

		if(name.compare(ladspa_plugins_db[j].key) == 0){
			return ladspa_plugins_db[j].soname;
		}
	}	
	return "";
}

std::string
LADSPALoader::get_name(std::string pkey){

	for(int j = 0; j < plugins_sys; j++){

		std::string m_label;
		if(pkey.compare(ladspa_plugins_db[j].key) == 0){
			m_label = ladspa_plugins_db[j].label;
			return m_label;
		}
	}	
	return "";
}

unsigned long
LADSPALoader::plugin_ports_count(std::string name){

	int count = -1;
	for(int j = 0; j < plugins_sys; j++){

		if(name.compare(ladspa_plugins_db[j].key) == 0){
			count = ladspa_plugins_db[j].portCount;
		}
	}
	return count;
	
}

/*****************************************
********* Querying ports data ************
*****************************************/

std::vector<int>
LADSPALoader::inputAudio_ports(std::string name){

	std::vector<int> input;
	for(int j = 0; j < plugins_sys; j++){

		if(name.compare(ladspa_plugins_db[j].key) == 0){
			input = ladspa_plugins_db[j].inAudio;
		}
	}
	return input;

}

std::vector<int>
LADSPALoader::inputControl_ports(std::string name){

	std::vector<int> input;
	for(int j = 0; j < plugins_sys; j++){

		if(name.compare(ladspa_plugins_db[j].key) == 0){
			input = ladspa_plugins_db[j].inControl;
		}
	}
	return input;

}

std::vector<int>
LADSPALoader::outputAudio_ports(std::string name){

	std::vector<int> input;
	for(int j = 0; j < plugins_sys; j++){

		if(name.compare(ladspa_plugins_db[j].key) == 0){
			input = ladspa_plugins_db[j].outAudio;
		}
	}
	return input;

}

std::vector<int>
LADSPALoader::outputControl_ports(std::string name){

	std::vector<int> input;
	for(int j = 0; j < plugins_sys; j++){

		if(name.compare(ladspa_plugins_db[j].key) == 0){
			input = ladspa_plugins_db[j].outControl;
		}
	}
	return input;

}

std::string
LADSPALoader::port_name(std::string name, int index){

	std::string n;
	for(int j = 0; j < plugins_sys; j++){

		if(name.compare(ladspa_plugins_db[j].key) == 0){
			std::pair<std::string, int> p = ladspa_plugins_db[j].portNames[index];// i don't need pairs for this
			if(p.second == index){
				n = p.first;
			}
		}
	}
	return n;
}

/*****************************************
**** Dealing with plugin library *********
*****************************************/

/**
	Constructor. Scans for plugins in the system at start-up
*/
LADSPALoader::LADSPALoader(){

	discoverPlugins();
}

/**
	Returns a vector with the possible paths for LADSPA libraries depending on the platform (so far, just Linux)
*/
std::vector<QString>
LADSPALoader::getPluginPath()
{
    std::vector<QString> pathList;
    std::string path;

    //Check for a environment variable with LADSPA plugins
    char *cpath = getenv("LADSPA_PATH");
    if (cpath) path = cpath;

    //Go for default path
    if (path == "") {

        path = DEFAULT_LADSPA_PATH;

	char *home = getenv("HOME");
	if (home) {
            std::string::size_type f;
            while ((f = path.find("$HOME")) != std::string::npos &&
                   f < path.length()) {
                path.replace(f, 5, home);
            }
        }

#ifdef _WIN32
        char *pfiles = getenv("ProgramFiles");
        if (!pfiles) pfiles = "C:\\Program Files";
        {
        std::string::size_type f;
        while ((f = path.find("%ProgramFiles%")) != std::string::npos &&
               f < path.length()) {
            path.replace(f, 14, pfiles);
        }
        }
#endif
    }

    std::string::size_type index = 0, newindex = 0;

    while ((newindex = path.find(PATH_SEPARATOR, index)) < path.size()) {
	pathList.push_back(path.substr(index, newindex - index).c_str());
	index = newindex + 1;
    }
    
    pathList.push_back(path.substr(index).c_str());

    return pathList;
}

/**
	Discover all the plugins in the system (plugin paths). We don't load info about I/0 ports yet.
*/
void
LADSPALoader::discoverPlugins(){

	std::vector<QString> pathList = getPluginPath();

	//Not including the LRDF

        for (std::vector<QString>::iterator i = pathList.begin();
		i != pathList.end(); ++i) {

		//esto no se lo q es.
		QDir pluginDir(*i, PLUGIN_GLOB);

		//open and check every library
		for (unsigned int j = 0; j < pluginDir.count(); ++j) {
		    discoverPlugins(QString("%1/%2").arg(*i).arg(pluginDir[j]));
		}
   	}
}

/**
	Analyses each library for plugins
*/
void
LADSPALoader::discoverPlugins(QString soname)
{
    void *libraryHandle = DLOPEN(soname, RTLD_LAZY);
	
    std::cerr << soname.toStdString() <<std::endl;

    if (!libraryHandle) {
        std::cerr << "WARNING: LADSPALoader::discoverPlugins: couldn't load plugin library "
                  << soname.toStdString() << std::endl;
        return;
    }

    LADSPA_Descriptor_Function fn = (LADSPA_Descriptor_Function)DLSYM(libraryHandle, "ladspa_descriptor");

    if (!fn) {
	std::cerr << "WARNING: LADSPALoader:discoverPlugins: No descriptor function in " << soname.toStdString() << std::endl;
	return;
    }

    const LADSPA_Descriptor *descriptor = 0;
    
    int index = 0;
    while ((descriptor = fn(index))) {

        ladspa_plugins_db[plugins_sys].name = descriptor->Name;
	ladspa_plugins_db[plugins_sys].label = descriptor->Label;
	//public identifier
	ladspa_plugins_db[plugins_sys].key = ldpl_plugin_key(descriptor->Label);
        ladspa_plugins_db[plugins_sys].maker = descriptor->Maker;
        ladspa_plugins_db[plugins_sys].copyright = descriptor->Copyright;
	ladspa_plugins_db[plugins_sys].portCount = descriptor->PortCount;
	ladspa_plugins_db[plugins_sys].soname = soname.toStdString();
        ladspa_plugins_db[plugins_sys].category = "";
        ladspa_plugins_db[plugins_sys].isSynth = false;

	//read the ports descriptor	
	read_ports(descriptor, plugins_sys);

	++index;
	plugins_sys++;
    }

    if (DLCLOSE(libraryHandle) != 0) {
        std::cerr << "WARNING: LADSPALoader::discoverPlugins - can't unload " << libraryHandle << std::endl;
        return;
    }
}

/**
	Retrieves a LADSPA plugin descriptor and fills the database of type plugins, so we don't need to access libraries each time
*/
const LADSPA_Descriptor *
LADSPALoader::getLADSPADescriptor(std::string name)
{
 
    const char *son = plugin_soname(name).data();
    std::cerr << son << std::endl;
    QString soname(son);

    if (m_libmap.find(soname) == m_libmap.end()) {
	loadLibrary(soname);
	if (m_libmap.find(soname) == m_libmap.end()) {
	    std::cerr << "WARNING: LADSPALoader::getLADSPADescriptor: loadLibrary failed for " << soname.toStdString() << std::endl;
	    return 0;
	}
    }

    void *libraryHandle = m_libmap[soname];

    LADSPA_Descriptor_Function fn = (LADSPA_Descriptor_Function)
	DLSYM(libraryHandle, "ladspa_descriptor");

    if (!fn) {
	std::cerr << "WARNING: LADSPALoader: No descriptor function in library " << soname.toStdString() << std::endl;
	return 0;
    }

    const LADSPA_Descriptor *descriptor = 0;
    int index = 0;
    std::string label = get_name(name);

    while ((descriptor = fn(index))) {
	
	std:string test(descriptor->Label);
	if (test == label){
		//std::cerr<<test<<std::endl;
		//std::cerr<<label<<std::endl;
		return descriptor;
	}
	++index;
    }
    std::cerr << "WARNING: LADSPALoader: No such plugin as " << label << " in library " << son << std::endl;

    return 0;
}

/**
	Loading a library (just linux so far)
*/
void
LADSPALoader::loadLibrary(QString soName)
{
    void *libraryHandle = DLOPEN(soName, RTLD_NOW);
    if (libraryHandle) {
        m_libmap[soName] = libraryHandle;
        std::cerr << "LADSPALoader::loadLibrary: Loaded library \"" << soName.toStdString() << "\"" << std::endl;
        return;
    }

    if (QFileInfo(soName).exists()) {
        DLERROR();
        std::cerr << "LADSPALoader::loadLibrary: Library \"" << soName.toStdString() << "\" exists, but failed to load it" << std::endl;
        return;
    }

    std::vector<QString> pathList = getPluginPath();

    QString fileName = QFile(soName).fileName();
    QString base = QFileInfo(soName).baseName();

    for (std::vector<QString>::iterator i = pathList.begin();
	 i != pathList.end(); ++i) {
        
#ifdef DEBUG_LADSPA_PLUGIN_FACTORY
        std::cerr << "Looking at: " << (*i).toStdString() << std::endl;
#endif

        QDir dir(*i, PLUGIN_GLOB,
                 QDir::Name | QDir::IgnoreCase,
                 QDir::Files | QDir::Readable);

        if (QFileInfo(dir.filePath(fileName)).exists()) {
#ifdef DEBUG_LADSPA_PLUGIN_FACTORY
            std::cerr << "Loading: " << fileName.toStdString() << std::endl;
#endif
            libraryHandle = DLOPEN(dir.filePath(fileName), RTLD_NOW);
            if (libraryHandle) {
                m_libmap[soName] = libraryHandle;
                return;
            }
        }

	for (unsigned int j = 0; j < dir.count(); ++j) {
            QString file = dir.filePath(dir[j]);
            if (QFileInfo(file).baseName() == base) {
#ifdef DEBUG_LADSPA_PLUGIN_FACTORY
                std::cerr << "Loading: " << file.toStdString() << std::endl;
#endif
                libraryHandle = DLOPEN(file, RTLD_NOW);
                if (libraryHandle) {
                    m_libmap[soName] = libraryHandle;
                    return;
                }
            }
        }
    }

    std::cerr << "LADSPALoader::loadLibrary: Failed to locate plugin library \"" << soName.toStdString() << "\"" << std::endl;
}


/**
  * Unload a library when no need to use it. Should close them all at shutting?
  */
void
LADSPALoader::unloadLibrary(QString soName)
{
    LibraryHandleMap::iterator li = m_libmap.find(soName);
    if (li != m_libmap.end()) {
	std::cerr << "unloading " << soName.toStdString() << std::endl;
	DLCLOSE(m_libmap[soName]);
	m_libmap.erase(li);
    }
}

/***********************************************************
******************* plugin ports ***************************
***********************************************************/

/**
	Read ports. Stores info in the ladspa descriptor database. (names and position within the descriptor)
*/
void
LADSPALoader::read_ports(const LADSPA_Descriptor *m_descriptor, int index){

	int count = (int)m_descriptor->PortCount;
	std::cerr << count << std::endl;

	//analyse all the ports
	for (unsigned long i = 0; i < m_descriptor->PortCount; ++i) {
         
	if (LADSPA_IS_PORT_AUDIO(m_descriptor->PortDescriptors[i])) {
         	
		if (LADSPA_IS_PORT_INPUT(m_descriptor->PortDescriptors[i])) {
			 ladspa_plugins_db[index].inAudio.push_back(i);
	                 std::cerr << "LADSPAPluginInstance::init: port " << i << " is audio in" << std::endl;
			 ladspa_plugins_db[index].portNames.push_back(std::pair<std::string, int>(ports_names(m_descriptor, i), i));
		} else {
			 ladspa_plugins_db[index].outAudio.push_back(i);
              	 	 std::cerr << "LADSPAPluginInstance::init: port " << i << " is audio out" << std::endl;
			 ladspa_plugins_db[index].portNames.push_back(std::pair<std::string, int>(ports_names(m_descriptor, i), i));
		}

	} else if (LADSPA_IS_PORT_CONTROL(m_descriptor->PortDescriptors[i])) {

	        if (LADSPA_IS_PORT_INPUT(m_descriptor->PortDescriptors[i])) {
			ladspa_plugins_db[index].inControl.push_back(i);
	                std::cerr << "LADSPAPluginInstance::init: port " << i << " is control in" << std::endl;
			ladspa_plugins_db[index].portNames.push_back(std::pair<std::string, int>(ports_names(m_descriptor, i), i));

	        } else {
		        ladspa_plugins_db[index].outControl.push_back(i);		       
	                std::cerr << "LADSPAPluginInstance::init: port " << i << " is control out" << std::endl;
		        ladspa_plugins_db[index].portNames.push_back(std::pair<std::string, int>(ports_names(m_descriptor, i), i));

	                if (!strcmp(m_descriptor->PortNames[i], "latency") ||!strcmp(m_descriptor->PortNames[i], "_latency")) {
	                    std::cerr << "Wooo! We have a latency port!" << std::endl;
			}
		}
	 } else
            std::cerr << "LADSPAPluginInstance::init - "
                       << "unrecognised port type" << std::endl;

	}
}

/**
	Returns the name of the port given its position
*/
std::string
LADSPALoader::ports_names(const LADSPA_Descriptor *desc, int index){

	return std::string(desc->PortNames[index]);	
}	

/***********************************************************
***************** plugin management ************************
***********************************************************/

/**
	blockSize
*/
LADSPAPlugin::LADSPAPlugin *
LADSPALoader::instantiate(std::string name, size_t sr, size_t blockSize)
{

	const LADSPA_Descriptor *desc = getLADSPADescriptor(name);
	if(!desc){
		std::cerr << "LADSPALoader::instantiate_plugin: failed to instantiate plugin" << std::endl;
	}	
	
	LADSPAPlugin *plugin = new LADSPAPlugin(name, desc, sr, inputAudio_ports(name).size(),
						outputAudio_ports(name).size(),
						inputControl_ports(name).size(),
						outputControl_ports(name).size(), blockSize); 
	return plugin;
	//need to unload lib?
}

/**
	Connects the plugin ports. Plugin can not do this by its own.
*/
void
LADSPALoader::connect_audio_ports(LADSPAPlugin::LADSPAPlugin *plugin){

	int in = 0;
	int out = 0;
	
	std::string name = plugin->LADSPAPlugin::get_name();
	//connecting input audio ports

	std::vector<int> input = inputAudio_ports(name);
	std::vector<int> output = outputAudio_ports(name);

	std::cerr <<"LOADER::connectin ports"<<std::endl;

	for(int j=0; j<input.size(); j++){
		
		plugin->LADSPAPlugin::connect_input_port(input[j], in);
		in++;
	}
	//connecting output audio ports
	for(int r=0; r<output.size(); r++){
		
		//std::cerr<<"output port connectedd"<<std::endl;	
		plugin->LADSPAPlugin::connect_output_port(output[r], out);
		out++;
	}
	std::cerr<<"LADSPALoader: ports connected"<<std::endl;
}

/**
	Sets every control to 0. This may not be valid!
*/
void
LADSPALoader::set_default_controls(LADSPAPlugin::LADSPAPlugin *plugin){

	std::string name = plugin->LADSPAPlugin::get_name();
	
	//connecting input controls
	for(int j=0; j<inputControl_ports(name).size(); j++){
		
		plugin->LADSPAPlugin::set_control_port(inputControl_ports(name)[j], 0);
	}

	//connecting output controls
	for(int j=0; j<outputControl_ports(name).size(); j++){
		
		plugin->LADSPAPlugin::set_control_port(outputControl_ports(name)[j], 0);
	}
}

/*****************************************
*********** PASSING DATA *****************
*****************************************/

/**
	parameters: input control
*/
void
LADSPALoader::set_parameter(LADSPAPlugin::LADSPAPlugin *plugin, int parameter, LADSPA_Data value){

	plugin->LADSPAPlugin::set_control_port(parameter, value);
}

/**
	output control
*/
void
LADSPALoader::set_output_control(LADSPAPlugin::LADSPAPlugin *plugin, int c, LADSPA_Data value){

	plugin->LADSPAPlugin::set_control_port(c, value);
}

#endif
