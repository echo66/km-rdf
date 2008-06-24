/**
	
	SWI-Prolog/Weka module. This module provides a quite rough set of predicates to interact with the WEKA API and deal with ARFF files.
	This module is loaded by classification.pl which provides a framework to use it properly.

	David Pastor Escuredo 2008,
	Centre for Digital Music, Queen Mary	
	*/

:-module(weka, [	wkpl_fastVector/1
		,	wkpl_fastVector/2,
		, 	wkpl_fastVector_add/2
		,	wkpl_fastVector_addList/2

		
	
		,	wkpl_read_arff/2
		,	wkpl_write_arff/2
		,	wkpl_create_arff/6
		]).

/**loading interface written in JPL**/
:-[jpl_weka].
:-[classification].
:-[dataSet].
:-[arffFiles].

/** FAST VECTORS **/

/** ARFF FILES **/

/** read:
		wkpl_read_arff(+ArffFilePath, -Instances)
**/

/** write:
		wkpl_write_arff(+ArffFilePath, +Instances)

		wkpl_create_arff(+FilePath, +Name, +NumAttributes, +NomAttributes, +Capacity, +ListOfInstances)
**/
