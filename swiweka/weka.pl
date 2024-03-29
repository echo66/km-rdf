/**	
	<module> SWI-Prolog/Weka 1.0. This module provides a quite rough set of predicates to interact with the WEKA API and deal with ARFF files.
	
	This module is loaded by the classification front end which provides a framework to use it properly.

	This module defines the general and more important predicates. There are more useful predicates spread in the specific sources which are named 
	according to its functionality. 

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.

	@author David Pastor Escuredo	
	@license GPLv3
	@version 1.0
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

		,	wkpl_classifier/1
		,	wkpl_filter/1
		,	wkpl_clusterer/1
		,	wkpl_resource/1
		,	wkpl_package/1
		,	wkpl_resource_packages/3

		,	wkpl_classifier/2
		,	wkpl_classifier/3
		,	wkpl_classifier_description/2
		,	wkpl_set_options/2
		,	wkpl_get_options/2
		,	wkpl_list_options/2

		,	wkpl_stratify_dataSet/2
		,	wkpl_create_testSet/4
		,	wkpl_create_trainSet/4

		, 	wkpl_classification_method/2
		,	wkpl_classify_instance/3
		,	wkpl_distributionFor_instance/3

		,	wkpl_run_classifier/3
		,	wkpl_train_classifier/3
		,	wkpl_train_classifier/4
		,	wkpl_predictions/3
		,	wkpl_prediction_for/3
		]).

:- style_check(-discontiguous).

%loading interface written in JPL
%:-use_module(library(pldoc)). 
:-[evaluation].
:-[dataSet].
:-[arffFiles].

%% wkpl_fastVector(-FastVector) is det
% Constructs an empty WEKA::FastVector

%% wkpl_fastVector(-FastVector, +Capacity) is det
% Constructs a fast vector with the given capacity
 
%% wkpl_fastVector_add(+FastVector, +Element) is semidet
% Adds a valid element at the end of the fast vector

%% wkpl_fastVector_addList(+FastVector, +ListOfElements) is semidet
% Adds a list of elements (JPL references or datums, check JPL doc).

%% wkpl_create_empty_dataSet(+Name, -Dataset) is det
% Creates an Instances object completely empty. Only the dataset name.

%% wkpl_add_attribute(+Dataset, +Attribute, +Pos) is semidet
% Sets an attribute (JPL referece to an attribute object) in the given position

%% wkpl_set_class(+Dataset, +Attribute) is semidet
% Sets the attribute (JPL reference) as class for classification of the dataset

%% wkpl_set_classIndex(+Dataset, +Index) is semidet
% The same but passing the index of the attribute

%% wkpl_fill_dataSet(+Dataset, +ListOfRecords) is semidet
% Fills the dataset with the list of records (Instance objects) passed as a list: [Record1|...] where Record1 = [ValueAt1, ValueAt2, ...]

%% wkpl_attribute_nominal(+Name, +ListOfValuesRange, -Attribute) is semidet
% Returns a JPL ref to a nominal attribute given the name and the range of values. e.g. x_x_x(weather, [sunny, cloudy, warm], At).

%% wkpl_attribute_numerical(+Name, -Attribute) is semidet
% Returns an object wrapping a numerical attribute given its name

%% wkpl_attribute_string(+Name, -Attibute) is semidet
% A string attribute is returned given its name

%% wkpl_attribute_date(+Name, +Date, -Attribute) is semidet
% Creates a date attribute given the name an a date in a correct format

%% wkpl_attribute_relation(+Name, +Instances, -Attribute) is semidet
% Creates an attribute given a dataset and the name

%% wkpl_read_arff(+ArffFilePath, -Instances) is semidet
% Gets the dataset written in the ARFF file and wraps into a JPL reference (an Instances object) 

%% wkpl_write_arff(+ArffFilePath, +Instances) is semidet
% Writes an Instances object as JPL reference into an ARFF file

%% wkpl_create_arff(+FilePath, +Name, +NumAttributes, +NomAttributes, +Capacity, +ListOfInstances) is semidet
%% Writes and ARFF file given the elements of the data set (This is deprecated) 

%% wkpl_classifier(?ClassifierName) is nondet
% Checks for classifiers in the WEKA API. The classifier name is its Java class name.

%% wkpl_filter(?FilterName) is nondet
% Checks for filters in the WEKA API.

%% wkpl_clusterer(?Clusterer) is nondet
% Checks for clusterers in the WEKA API.

%% wkpl_resource(?Resource) is nondet
% Checks for any WEKA API resource.

%% wkpl_resource_packages(?Resource, ?Package, ?Type) is nondet
% Checks the relationships between resources, packages and also the family of the resource in the package hierarchy

%% wkpl_stratify_dataSet(+Instances, +Folds) is semidet
% Stratifies a dataset given the number of folds for cross-validation

%% wkpl_create_testSet(+Instances, +NumberFolds, +Fold, -TestSet) is semidet
% Creates a test set for the given parameters

%% wkpl_create_trainSet(+Instances, +NumberFolds, +Fold, -TrainSet) is semidet
% Creates a training set for the given parameters

%% wkpl_classification_method(+Classifier, -Method) is semidet
% Returns if the classifier is able to classify nominal classes (distributionFor) or numerical classes (classify)

%% wkpl_classify_instance(+Instance, +Classifier, -Result) is semidet
% Returns the value of the classified instance (a numerical value)

%% wkpl_distributionFor_instance(+Instance, +Classifier, -Distribution) is semidet
% Returns the distribution (list of values) of the classified instance for the nominal class

%% wkpl_run_classifier(+ClassifierName, +WekaOptions, -Evaluation) is semidet
% Wrapping predicate of the command line call of weka. Check the Doc to see available options. The evaluation is by default a large string but output 
% files can be set as options

%% wkpl_train_classifier(+ClassifierName, +Train, -Classifier) is semidet
% Trains the classifier given the train. The trainning is performed without cross-validation. May need to provide an alternative trainning.

wkpl_train_classifier(ClassifierName, Options, Train, Classifier) is semidet
% With options in the inti classifier

%% wkpl_predictions(+DataSet, +Classifier, -Predictions) is semidet
% Returns the predictions for the dataset given the trainned classifier. For nominal class datasets the prediction will be one of the values in the distribution

%% wkpl_prediction_for(+Instance, +Classifier, -Prediction) is semidet
% Returns the prediction for one single instance




