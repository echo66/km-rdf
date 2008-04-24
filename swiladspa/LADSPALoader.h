/**
	Loader for ladspa plugins
	David Pastor 2008
	code taken from SV
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

	void discoverPlugins();

	//virtual const std::vector<QString> &getPluginIdentifiers() const;
	//virtual void enumeratePlugins(std::vector<QString> &list);
	//virtual const RealTimePluginDescriptor *getPluginDescriptor(QString identifier) const;
  	LADSPA_Handle *instantiatePlugin(QString identifier,
                                                   int clientId,
                                                   int position,
                                                   unsigned int sampleRate,
                                                   unsigned int blockSize,
                                                   unsigned int channels);
	std::vector<std::string> ladspa_plugins();
	int plugin_parameter_count(std::string);
	std::string plugin_maker(std::string);
	std::string plugin_soname(std::string);
	LADSPA_Handle instantiate_plugin(std::string, unsigned long);
     //float getPortMinimum(const LADSPA_Descriptor *, int port);
     //float getPortMaximum(const LADSPA_Descriptor *, int port);
     //float getPortDefault(const LADSPA_Descriptor *, int port);
     //float getPortQuantization(const LADSPA_Descriptor *, int port);
     //int getPortDisplayHint(const LADSPA_Descriptor *, int port);

protected:   

     std::vector<QString> getPluginPath();
     //virtual std::vector<QString> getLRDFPath(QString &baseUri);
     void discoverPlugins(QString soName);
     //void releasePlugin(LADSPA_Handle *, QString);
     const LADSPA_Descriptor *getLADSPADescriptor(std::string identifier); 
     void loadLibrary(QString soName);
     void unloadLibrary(QString soName);
     //void unloadUnusedLibraries();

     typedef std::map<QString, void *> LibraryHandleMap;
     LibraryHandleMap m_libmap;
};

#endif

