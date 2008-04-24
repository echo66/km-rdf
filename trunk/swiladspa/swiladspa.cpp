/**
 * SWI-Prolog external interface to LADSPA Plugins
 * This C/C++ source defines foreign functions a library for swiladspa. Mainly type-conversion stuff and DB of active plugins
 * David Pastor Escuredo 2007
 */
#include <swiladspa.h>

static struct SwiLADSPAPlugin{

	/* id: __plugin_id */
	QString id;
	/* pointer a LADSPA Plugin */
	LADSPA_Handle plugin;
	/* name of the plugin */
	char *type;
}
					
/*
 * Database with all the entries id/plugin. This is a database of working plugins.
 */
LADSPA_plugins_db[MAX_LADSPA_PLUGIN];

/* Variables */
size_t active_plugins = 0;

				/************************************************************************
 				******************* C Interface functions implementation ****************
				************************************************************************/

/** Some functions to deal with the database **/

/*
 * Stores the instance of the plugin when vamp_plugin_load and returns an id that will be the handle for the plugin onwards. The plugin
 * is defined by the name and the sample rate. We don't reuse plugins (we could) and we don't save them through sessions.
 */

int
ldpl_register_plugin(LADSPA_Handle plugin, term_t id){

	LADSPA_plugins_db[active_plugins].plugin = plugin;
	LADSPA_plugins_db[active_plugins].id = ldpl_id_for_ladspa();
	
	PL_unify(id, term_t(PlTerm(PlAtom((LADSPA_plugins_db[active_plugins].id).toLocal8Bit().data()))));
	active_plugins++;
	return 0; //success
}

/*
 * Creates a simple id incrementally for the plugins
 */

QString
ldpl_id_for_ladspa()
{
	QString head("__ladspa::plugin_");

	//incremental id for blobs	
	QString var;
	var = QString("%1")
		.arg((long)active_plugins);
	head.append(var);
	return head;

}

/*
 * Gets the plugin for the id given
 */

int
ldpl_get_plugin(term_t id_t, LADSPA_Handle &plugin){

	char *id;
	PL_get_atom_chars(id_t,&id);
	QString qid((const char*)id);
	
	for(size_t r=0; r<MAX_LADSPA_PLUGIN; r++){
		
		if(qid.compare(LADSPA_plugins_db[r].id)==0){
			plugin = LADSPA_plugins_db[r].plugin;
			return 0;
		}
	}
	return -1;	
}

/*
 * util
 */
PlAtom
ldpl_string_to_atom(string x){

	int length = x.size();
	char y[length+1];
	for(int j=0;j<length;j++){
		y[j]=x[j];
	}	
	y[length] = '\000';

	return PlAtom(y);
}










