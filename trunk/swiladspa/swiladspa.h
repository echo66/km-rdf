/**
 * SWI-Prolog external interface to LADSPA Plugins
 * David Pastor Escuredo 2008, c4dm, Queen Mary University of London
 *
 */

#ifndef _SWI_LADSPA_H_
#define _SWI_LADSPA_H

#include <swiaudioblob.h>
#include <swimo.h>
#include <blobid.h>
#include <qstring.h>
#include <ladspa.h>

#define MAX_LADSPA_PLUGIN 1000 //1000 active plugins in one session

using namespace std;

			/***************************************************************************************************
			***** Prototypes of the C functions used in this interface to enhance the simpler C++ interface ****
			***************************************************************************************************/

int
ldpl_register_plugin(LADSPA_Handle, term_t); //registers the plugin assigning an id. We sue

QString
ldpl_id_for_ladspa();//creates the id for the plugin

int
ldpl_get_plugin(term_t, LADSPA_Handle &);//retrieves plugin

//something to deal with the input

//output type-conversion

PlAtom
ldpl_string_to_atom(string );//should be in a more general library

#endif
