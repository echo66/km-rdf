/**
 * SWI-Prolog external interface to Vamp Plugins
 * David Pastor Escuredo 2007, c4dm, Queen Mary University of London
 *
 */

#ifndef _SWI_VAMP_H_
#define _SWI_VAMP_H

#include <swiaudioblob.h>
#include <swimo.h>
#include <blobid.h>

#include <PluginHostAdapter.h>

using namespace std;

			/***************************************************************************************************
			***** Prototypes of the C functions used in this interface to enhance the simpler C++ interface ****
			***************************************************************************************************/

int
vmpl_plugin_to_blob(Vamp::Plugin *, term_t);//Vamp::Plugin* to blob so we can refer it later on

Vamp::Plugin *
vmpl_blob_to_plugin(term_t);//We get the pointer to plugin from the blob

const float* const*
vmpl_frame_to_input(term_t);//Constructs a valid block of data as vamp plugin input from a MO::frame

term_t
vmpl_frame_features_to_prolog(Vamp::Plugin::FeatureSet , int, term_t , Vamp::Plugin::OutputDescriptor);//Converts the FutureSet object extracted from the plugins into a complex Prolog term

float
vmpl_timestamp_float(Vamp::RealTime );//put the timestamp as a float

PlAtom
vmpl_string_to_atom(string );//should be in a more general library


#endif

