/**
 * SWI-Prolog external interface to LADSPA Plugins. This source provides working plugins management and type-conversion utilities.
 * David Pastor Escuredo 2008, c4dm, Queen Mary University of London
 *
 */

#ifndef _SWI_LADSPA_H_
#define _SWI_LADSPA_H_

#include <swiaudioblob.h>
#include <swimo.h>
#include <blobid.h>
#include <qstring.h>
#include <LADSPAPlugin.h>

#define MAX_LADSPA_PLUGIN 1000 //1000 active plugins in one session

using namespace std;

			/***************************************************************************************************
			***** Prototypes of the C functions used in this interface to enhance the simpler C++ interface ****
			***************************************************************************************************/

/** Working plugins database **/

int
ldpl_register_plugin(LADSPAPlugin::LADSPAPlugin *, string, term_t); //registers the plugin assigning an id. We also keep the plugin type

QString
ldpl_id_for_ladspa();//creates the id for the plugin: __plugin::ladspa_id

int
ldpl_get_plugin(term_t, LADSPAPlugin::LADSPAPlugin * &, string &);//retrieves plugin and the type

/** Type conversion and utils **/
int
ldpl_set_input_buffers(term_t, LADSPA_Data **, int, size_t);//fills the buffer with the input data (data id)

PlAtom
ldpl_string_to_atom(string );//should be in a more general library

#endif
