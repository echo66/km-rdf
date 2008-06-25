/**
	
	SWI-Prolog/Weka module. This module provides a quite rough set of predicates to interact with the WEKA API and deal with ARFF files.
	This module is loaded by classification.pl which provides a framework to use it properly.

	David Pastor Escuredo 2008,
	Centre for Digital Music, Queen Mary	
	*/

:-module(weka, [	wkpl_fastVector/1
		,	wkpl_fastVector/2
		, 	wkpl_fastVector_add/2
		,	wkpl_fastVector_addList/2

		,	wkpl_create_dataSet/4
		,	wkpl_create_empty_dataSet/2
		,	wkpl_add_attribute/3
		,	wkpl_set_class/2
		,	wkpl_set_classIndex/2
		,	wkpl_fill_dataSet/2

		,	wkpl_attribute_nominal/3
		,	wkpl_attribute_numerical/2
		,	wkpl_attribute_string/2
		, 	wkpl_attribute_date/3
		, 	wkpl_attribute_relation/3
		
		,	wkpl_read_arff/2
		,	wkpl_write_arff/2
		,	wkpl_create_arff/6

		,	wkpl_classifier/3
		,	wkpl_classifier_description/2
		,	wkpl_set_options/2
		,	wkpl_get_options/2
		,	wkpl_list_options/2
		,
		]).

:- style_check(-discontiguous).

/**loading interface written in JPL**/
:-[runclassification].
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
