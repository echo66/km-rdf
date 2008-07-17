/**
	This pl analyses the WEKA API allowing querying and management of weka resources

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
*/

:-[jpl_weka].

/***************************************** GENERAL RESOURCES PART ************************************/

wkpl_resource('classifier').
wkpl_resource('filter').
wkpl_resource('clusterer').
%wkpl_resource('datagenerator').

/**
	wkpl_packages(-Packages)
*/
wkpl_packages(PList):-
	wkpl_getObject('weka.core.ClassDiscovery', ClassDiscovery),
	jpl_call(ClassDiscovery, findPackages, [], Packages),
	jpl_call(Packages, toArray, [], PackArray),
	jpl_array_to_list(PackArray, PList).

/**
	wkpl_package(?Package)
*/
wkpl_package(Package):-
	wkpl_packages(P),
	member(Package, P).

/**
	wkpl_resource_packages(?Resource, ?Package, ?Type)
*/
wkpl_resource_packages(Resource, Package, Type):-
	wkpl_resource(Resource),
	wkpl_package_resource(Resource, Package, Type).

wkpl_package_resource(classifier, Package, Type):-
	wkpl_package_classifiers(Package, Type).

wkpl_package_resource(filter, Package, Type):-
	wkpl_package_filters(Package, Type).

wkpl_package_resource(clusterer, Package, Type):-
	wkpl_package_clusterers(Package, Type).
	
/**
	wkpl_packages_classifiers(?Package, ?Type)
*/
wkpl_package_classifiers(Package, Type):-
	wkpl_package(Package),
	atom_concat('weka.classifiers.', Type, Package). 

/**
	wkpl_packages_clusters(?Package, ?Type)
*/
wkpl_package_clusterers(Package, Type):-
	wkpl_package(Package),
	atom_concat('weka.clusterers.', Type, Package). 

/**
	wkpl_packages_filters(?Package, ?Type)
*/
wkpl_package_filters(Package, Type):-
	wkpl_package(Package),
	atom_concat('weka.filters.', Type, Package).

/*********************************************** CLASSIFIERS *************************************/

/**
	wkpl_classifier(?Classifier). Extracts all the classes which are subclass of Classifier!
*/
wkpl_classifier(Classifier):-
	wkpl_getObject('weka.core.ClassDiscovery', ClassDiscovery),
	wkpl_resource_packages(classifier, Package, _),
	jpl_call(ClassDiscovery, find, ['weka.classifiers.Classifier', Package], Classifiers),
	jpl_call(Classifiers, toArray, [],ClasifArray),
	jpl_array_to_list(ClasifArray, List),
	member(Classifier, List).

wkpl_filter(Filter):-
	wkpl_getObject('weka.core.ClassDiscovery', ClassDiscovery),
	wkpl_resource_packages(filter, Package, _),
	jpl_call(ClassDiscovery, find, ['weka.filters.Filter', Package], Filters),
	jpl_call(Filters, toArray, [], FArray),
	jpl_array_to_list(FArray, List),
	member(Filter, List).

wkpl_clusterer(Clusterer):-
	wkpl_getObject('weka.core.ClassDiscovery', ClassDiscovery),
	wkpl_resource_packages(clusterer, Package, _),
	jpl_call(ClassDiscovery, find, ['weka.filters.Filter', Package], Cls),
	jpl_call(Cls, toArray, [], CArray),
	jpl_array_to_list(CArray, List),
	member(Clusterer, List).




