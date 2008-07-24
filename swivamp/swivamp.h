/**
	SWI-Prolog external interface to Vamp Plugins
 
	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2007 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
 	*/

#ifndef _SWI_VAMP_H_
#define _SWI_VAMP_H

#include <swiaudioblob.h>
#include <blobid.h>
#include <qstring.h>
#include <PluginHostAdapter.h>

#define MAX_VAMP_PLUGIN 1000 //1000 active plugins

using namespace std;

			/***************************************************************************************************
			***** Prototypes of the C functions used in this interface to enhance the simpler C++ interface ****
			***************************************************************************************************/

int
vmpl_register_plugin(Vamp::Plugin *, term_t); //registers the plugin assigning an id

QString
vmpl_id_for_vamp();//creates the id for the plugin

int
vmpl_get_plugin(term_t, Vamp::Plugin * &);

const float* const*
vmpl_frame_to_input(int, size_t, term_t);//Constructs a valid block of data as vamp plugin input from a prolog frame

term_t
vmpl_frame_features_to_prolog(Vamp::Plugin::FeatureSet , int, float, float, Vamp::Plugin::OutputDescriptor);//Converts the FutureSet object extracted from the plugins into a complex Prolog term

void
timestamp_functor(term_t, term_t, term_t);//creates a timestamp term. Taken from the old swimo

void
feature_functor(term_t, term_t, term_t, term_t);

float
vmpl_timestamp_float(Vamp::RealTime );//put the timestamp as a float

PlAtom
vmpl_string_to_atom(string );//should be in a more general library


#endif

