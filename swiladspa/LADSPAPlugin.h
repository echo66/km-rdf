/**
	Wrapping class for the working instances of LADSPA plugin types.
	The information of this class is specific for each working plugin. The LADSPALoader class contains plugin type information.
	David Pastor Escuredo 2008, c4dm, Queen Mary.
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

	std::string m_name; //identifier used in this host

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
