/**
	PRolog interface LADSPA Plugins
	David PAstor 2008
*/

#include <LADSPALoader.h>

LADSPALoader::LADSPALoader *l_loader;

/**
	Get a LADSPA plugins loader
*/
PREDICATE(ldpl_loader, 0){

	l_loader = new LADSPALoader::LADSPALoader();
	return true;	
}

/**
	This predicate lists the plugins in the systems returning a list of identifiers (better to use labels?)
*/
PREDICATE(ldpl_plugins, 1)
{
	//-listOfLADSPAPlugins
	std::vector<std::string> ladspaList = l_loader->ladspa_plugins();

	PlTerm list;
	PlTail tail(list);
	for(int j= 0; j<ladspaList.size(); j++){

	tail.append(PlTerm((const char*)ladspaList[j].data()));
	}
	tail.close();
	return A1 = list;

}

/*********************************************
*** QUERYING PLUGIN DESCRIPTION **************
*********************************************/

/**
	Plugin maker. We just need the identifier of the plugin.
*/
PREDICATE(ldpl_plugin_maker,  2)
{
	//+plugin identifier
	//-maker

	std::string ident((char *)A1);


	string author = l_loader->LADSPALoader::plugin_maker(ident);
	char maker[author.size()+1];
	for(unsigned int j=0;j<author.size();j++){
		maker[j]=author[j];
	}
	maker[author.size()] = '\000';
	return A2 = PlAtom(maker);
}

/**
	Returns library owning the plugin
*/
PREDICATE(ldpl_plugin_library,  2)
{
	//+plugin identifier
	//-library

	std::string ident((char *)A1);


	string author = l_loader->LADSPALoader::plugin_soname(ident);
	char maker[author.size()+1];
	for(unsigned int j=0;j<author.size();j++){
		maker[j]=author[j];
	}
	maker[author.size()] = '\000';
	return A2 = PlAtom(maker);
}



