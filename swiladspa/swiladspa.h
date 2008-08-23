/**
	LADSPA/SWI-Prolog foreign interface. 

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
	*/

#ifndef _SWI_LADSPA_H_
#define _SWI_LADSPA_H_

#include <swiaudioblob.h>
#include <qstring.h>
#include <blobid.h>
#include <LADSPAPlugin.h>

#define MAX_LADSPA_PLUGIN 1000 //1000 active plugins in one session

using namespace std;

			/***************************************************************************************************
			***** Prototypes of the C functions used in this interface to enhance the simpler C++ interface ****
			***************************************************************************************************/

/** Working plugins database **/

int
ldpl_register_plugin(LADSPAPlugin::LADSPAPlugin *, string, term_t); //registers the plugin assigning an id. We also keep the plugin type

string
ldpl_id_for_ladspa();//creates the id for the plugin: __plugin::ladspa_id

int
ldpl_get_plugin(term_t, LADSPAPlugin::LADSPAPlugin * &, string &);//retrieves plugin and the type

/** Type conversion and utils **/
int
ldpl_set_input_buffers(term_t, LADSPA_Data **, int, size_t);//fills the buffer with the input data (data id)

int
ldpl_set_input_buffers(term_t, LADSPA_Data **, int, size_t, size_t);//fills the buffer with the input data (data id) framing

PlAtom
ldpl_string_to_atom(string );//should be in a more general library

string
ldpl_plugin_key(string);//creates a plugin key as swivamp

string
ldpl_plugin_key(string, string);//creates a plugin key as swivamp

vector<float> *
ldpl_select_frame(size_t, size_t , vector<float> *);
#endif
