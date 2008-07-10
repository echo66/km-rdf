/**	
	SWI-Prolog/Weka module. This module provides a quite rough set of predicates to interact with the WEKA API and deal with ARFF files.
	
	This module is loaded by the classification front end which provides a framework to use it properly.

	This module defines the general and more important predicates. There are more useful predicates spread in the specific sources which are named according to its functionality. 

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (c) 2008 David Pastor Escuredo and QMUL.
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
		]).

:- style_check(-discontiguous).

%%loading interface written in JPL* 
:-[runclassification].
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
