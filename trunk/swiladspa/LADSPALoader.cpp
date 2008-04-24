/**
	Plugins loader
	David Pastor 2008
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
	bool isSynth;
	unsigned int parameterCount;
	unsigned int audioInputPortCount;
	unsigned int audioOutputPortCount;
	unsigned int controlOutputPortCount;
	std::vector<std::string> controlOutputPortNames;
}

/*
 * Database of LADSPA type plugins in system (max of 200). It is filled in each session
 */
ladspa_plugins_db[200];

int plugins_sys = 0;

/****************************************
*** Querying the descriptor *************
****************************************/

std::vector<std::string>
LADSPALoader::ladspa_plugins(){

	std::vector<std::string> plugins;
	std::cerr << plugins_sys << std::endl;
	for(int j = 0; j < plugins_sys; j++){

		std::cerr<<ladspa_plugins_db[j].label<<std::endl;
		std::cerr<<ladspa_plugins_db[j].soname<<std::endl;

		//create a better identifier. Should use label, sonmae and ladspa maybe.
		plugins.push_back(ladspa_plugins_db[j].name);
	}
	return plugins;
}

std::string
LADSPALoader::plugin_maker(std::string name){

	for(int j = 0; j < plugins_sys; j++){

		if(name.compare(ladspa_plugins_db[j].name) == 0){
			return ladspa_plugins_db[j].maker;
		}
	}	
	return "";
}

std::string
LADSPALoader::plugin_soname(std::string name){

	for(int j = 0; j < plugins_sys; j++){

		if(name.compare(ladspa_plugins_db[j].name) == 0){
			return ladspa_plugins_db[j].soname;
		}
	}	
	return "";
}

int
LADSPALoader::plugin_parameter_count(std::string name){

	int count;
	for(int j = 0; j < plugins_sys; j++){

		if(name.compare(ladspa_plugins_db[j].name) == 0){
			count = ladspa_plugins_db[j].parameterCount;
		}
		else count = -1;//error no negative counts
	}
	return count;
	
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
	Discover all the plugins in the system (plugin paths)
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
        ladspa_plugins_db[plugins_sys].maker = descriptor->Maker;
        ladspa_plugins_db[plugins_sys].copyright = descriptor->Copyright;
        ladspa_plugins_db[plugins_sys].category = "";
        ladspa_plugins_db[plugins_sys].isSynth = false;
        ladspa_plugins_db[plugins_sys].parameterCount = 0;
        ladspa_plugins_db[plugins_sys].audioInputPortCount = 0;
        ladspa_plugins_db[plugins_sys].audioOutputPortCount = 0;
        ladspa_plugins_db[plugins_sys].controlOutputPortCount = 0;
	ladspa_plugins_db[plugins_sys].soname = soname.toStdString();

	//no hago nada con los nombres de los puertos

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
    while ((descriptor = fn(index))) {
	if (descriptor->Name == name) return descriptor;
	++index;
    }

    std::cerr << "WARNING: LADSPALoader: No such plugin as " << name << " in library " << soname.toStdString() << std::endl;

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
//	std::cerr << "unloading " << soname.toStdString() << std::endl;
	DLCLOSE(m_libmap[soName]);
	m_libmap.erase(li);
    }
}




#endif
