/**
	<module> Classification 'scenario'. This module offers an understandable logic framework to manage machine learning algorithms from SWI-Prolog.
	This module relies on just one external API called WEKA. For a deeper control of WEKA possibilities, please check out the weka module.
	
	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.

	@author David Pastor Escuredo	
	@license GPLv3
	@version 1.0
	@tbd specific functions for particular classifiers. Add filters and clusterers
**/

:-module(classification, [	classifSets_db/2
			,	dataSets_db/2

			,	create_classifSet/3
			,	save_classifSet/2
			,	load_classifSet/2
		
			,	insert_records/3
			, 	save_dataSet/2
			,	save_dataSet/2

			,	classifier/1
			,	classifier_taxonomy/4
			,	set_classifier/4

			,	classify/3
			]).

:-use_module('weka').

/*************************************** IDENTIFIERS *****************************/
/**
	Keeping a database of classification sets which can be modified. We use a java.util.HashTable.
	This sets only comprise weka headers (attributes) ‪like an empty dataset
*/
:- dynamic classifSets_db/2.

/**
	The same for full datasets, testSets and trainSets
*/
:-dynamic dataSets_db/2.
:-dynamic testSets_db/2.
:-dynamic trainSets_db/2.

/************************************* DATE SET MANAGEMENT ****************************/

%% create_classifSet(+SetID, +ListOfAttributes, +Class) is semidet
%
%	Create Classification set (this sort of set does not contain data itself, just the classification set up) with:
%
%		-Name (particular sets with data will have some other name). This ID is a handle for the JPL reference for the Instances object. 
%		This ID is also the dataset name within the WEKA class.
%		-List of Attributes
%		-Class (optional). Need to pass the index of the attribute within the dataset
%
% @param numeAt('name').
% @param nomAt('name', ListOfValues).
% @param stringAt('name').
% @param relationAt('name', Instances).
% @param dateAt('name', Date).

create_classifSet(Id, ListOfAttributes, Class):-
	wkpl_fastVector(FastVector),
	set_attributes(ListOfAttributes, FastVector),
	wkpl_create_dataSet(Id, FastVector, 0, Instances),
	((nonvar(Class), !, set_class(Class, Instances));(var(Class))),	
	assert(classifSets_db(Id, Instances)).

set_class(_, _).
set_class(Index, Instances):-
	wkpl_set_classIndex(Instances, Index).		

set_attributes([],_).
set_attributes([H|T], F):-
	set_attribute(H, F),
	set_attributes(T, F).

set_attribute(numAt(Name), F):-
	wkpl_attribute_numerical(Name, A),
	wkpl_fastVector_add(F,A).
set_attribute(nomAt(Name, Values), F):-
	wkpl_attribute_nominal(Name, Values, A),
	wkpl_fastVector_add(F, A).
set_attribute(stringAt(Name), F):-
	wkpl_attribute_string(Name, A),
	wkpl_fastVector_add(F, A).
set_attribute(dateAt(Date), F):-
	wkpl_attribute_date(Date, A),
	wkpl_fastVector_add(F, A).
%so far fails to put a relation attribute

%%save_classifSet(+ID, +ArffFilePath) is semidet
%
% Save the classification set into an arr file. 
	
save_classifSet(ID, ArffFilePath):-
	classifSets_db(ID, Instances),
	wkpl_write_arff(ArffFilePath, Instances).

%% load_classifSet(+ArffFilePath, -ID) is semidet
% Loads the ARFF file returning the name of dataset as handle

load_classifSet(ArffFilePath, ID):-
	wkpl_read_arff(ArffFilePath, Instances),
	wkpl_dataSet_name(Instances, ID),
	assert(classifSets_db(ID, Instances)).

%% insert_records(+ClassifID, +DataSetID, +Data) is semidet

% Filling the classification set with data records. The Data is not checked out, so the user is resonsible of using it properly.
%
%	Data = [[valueAt1, valueAt2, valueAt3, ...,valueAtN]|...].
%
%	two ways:
%
%	1.
%		Giving a ClassifSet id which retrieves the original classification environment without data.
%		Passing DataSetID to store the new filled dataset (different from the classifsetID
%
%	2.	Retrieving your own dataset with your own id so classifID is not passed

insert_records(ClassifID, DataSetID, Data):-
	nonvar(ClassifID), nonvar(DataSetID),
	classifSets_db(ClassifID, Set),
	test_data_records(Set, Data),
	copy_term(Set, MySet),
	wkpl_fill_dataSet(MySet, Data),
	assert(dataSets_db(DataSetID, MySet)).	

insert_records(ClassifID, DataSetId, Data):-
	var(ClassifID), nonvar(DataSetId),
	dataSets_db(DataSetId, Set),
	wkpl_fill_dataSet(Set, Data).

%crappy test. Checks the structure of the input and the length of the record. Does not check types of attributes.
check_data_records(Instances, [Record1|_T]):-
	is_list(Record1),
	wkpl_numberAttributes(Instances, L),
	length(Record1, L).

%% save_dataSet(+ID, +ArffFilePath) is semidet
%
% Dumps the dataset into an ARFF file. The ID handle is not the name of the dataset but a private name we give to the dataset to identify it

save_dataSet(ID, ArffFilePath):-
	dataSets_db(ID, Instances),
	wkpl_write_arff(ArffFilePath, Instances).

%% load_dataSet(+ArffPath, +ID) is semidet
%
% Loads the dataset from an ARFF file and unifies it with an ID we use as handle

load_dataSet(ArffFilePath, ID):-
	wkpl_read_arff(ArffFilePath, Instances),
	assert(dataSets_db(ID, Instances)).

%% test_set(+DataSetID, +Folds, +Fold, +TestID) is semidet
%
% Creates a test set for the given parameters and unifies with the ID (i may not need to use other db)

test_set(DataSetID, Folds, Fold, TestID):-
	dataSets_db(DataSetID, Instances),
	wkpl_create_testSet(Instances, Folds, Fold, Test),
	assert(testSets_db(TestID, Test)).

%% train_set(+DataSetID, +Folds, +Fold, +TrainID) is semidet
%
% Creates a train set for the given parameters and unifies with the ID (i may not need to use other db)

train_set(DataSetID, Folds, Fold, TrainID):-
	dataSets_db(DataSetID, Instances),
	wkpl_create_trainSet(Instances, Folds, Fold, Train),
	assert(trainSets_db(TrainID, Train)).

%% stratify_dataSet(+DataSetID, +Folds) is semidet
%
% Stratifies a data in the given folds for cross-validation

stratify_dataSet(ID, Folds):-
	dataSets_db(ID, Set),
	wkpl_stratify_dataSet(Set, Folds).

/******************************************** CLASSIFIERS ***************************************/	

%% classifier(?Classifier) is nondet
%
% Queries/check available classifiers

classifier(Classifier):-
	wkpl_classifier(Classifier).

%% classifier_taxonomy(?Name, ?Type, ?Subtype, ?ClassifierClassName) is semidet
%
% Hierarchy of classifiers. It allows the user to find the desired classifier.

classifier_taxonomy(Name, Type, Subtype, Classifier):-
	wkpl_classifier(Classifier),
	atom_concat('weka.classifiers.', TypeName, Classifier),
	concat_atom(List, '.', TypeName),
	nth0(0, List, Type),
	rest_name(List, Name, Subtype).
rest_name(List, N, _):-
	length(List, A),
	A = 2,
	nth0(1, List, N).
rest_name(List, N, S):-
	length(List, A),
	A = 3,
	nth0(1, List, S),
	nth0(2, List, N).
	
%% set_classifier(+Classifier, +DataSetID, +ListOfOptions, -Object)
%
%	Classifier: class name 'weka.classifiers.x.y'
%	DataSetID: member of dataSets_db/1
%	ListOfOptions: ['-D', '-O'] Options must be checked in WEKA DOC or through the swiweka interface predicates ([classifiers.pl]). Can be empty
%	Object: a JPL reference to the classifier set

set_classifier(Classifier, DataSetID, Options, Object):-
	classifier(Classifier),
	create_classifier(Classifier, Options, Object),
	dataSets_db(DataSetID, DataSet),
	wkpl_build_classifier(DataSet, Object).

create_classifier(Classifier, Options, Object):-
	var(Options), !,
	wkpl_classifier(Classifier, Object).
create_classifier(Classifier, Options, Object):-
	nonvar(Options),
	Options = [], !, 
	wkpl_classifier(Classifier, Object).
create_classifier(Classifier, Options, Object):-
	is_list(Options),
	length(Options, L),
	L>0, !, 
	wkpl_classifier(Classifier, Options, Object).

%%	classify(+SetJPLRef, +ClassifierJPLref, -Result) is nondet
%
%	Run classification. The result can be one literal (the value of the class attribute) or a list with the distribution for the class attribute.
% 	I should try to give a better look to the result

classify(RealSet, Classifier, Result1):-
	jpl_call(RealSet, enumerateInstances, [], Entries),
	classify_record(Entries, Classifier, Result1).
	%jpl_call(RealSet, classAttribute, [], _Class).
	%constrain_result(Result1, Result, Class).

%Retrieve the distribution as it is been retrieved
constrain_result(R1, R, Class):-
	is_list(R1), !, 
	length(R1, L),
	jpl_call(Class, numValues, [], L2),
	L2 = L,
	R = R1.

%not numeric
constrain_result(R1, R, Class):-
	\+is_list(R1),!,
	jpl_call(Class, value,  [R1], R).	

classify_record(Entries, Classifier, Result):-
	jpl_call(Entries, nextElement, [], Record),
	classification(Classifier, Record, Result).

%Depending on the classification method implemented by the classifier (classifies categories or not). The classifier will fail if u try to classify a 
%nominal class if it is not implemented and vice versa

classification(Classifier, Record, Result):-
	wkpl_classification_method(Classifier, 'classifyInstance'), !, wkpl_classify_instance(Record, Classifier, Result).

classification(Classifier, Record, Result):-
	wkpl_classification_method(Classifier, 'distributionForInstance'), !, wkpl_distributionFor_instance(Record, Classifier, Result).
	

