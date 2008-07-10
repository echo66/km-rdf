/**
	Classifiers' common stuff

	ToDO:
		-Try to get a better format for names instead of the classnames.
		-What to do with the non common stuff?

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (c) 2008 David Pastor Escuredo and QMUL.
*/

:-[jpl_weka].
:-[resources].

/***** CONSTRUCTORS *******/

/**
	wkpl_classifier/1 defined in resources.pl lists all the WEKA classifiers
*/

/**
	wkpl_classifier(?Name, ?Classifier). Constructor for WEKA classifier without options setting seen as declarative relationship between the name an the instance
*/
wkpl_classifier(Name, Classifier):-
	var(Classifier),
	wkpl_classifier(Name),
	wkpl_getObject(Name, Classifier).
wkpl_classifier(Name, Classifier):-
	nonvar(Classifier),
	jpl_object_to_class(Classifier, Class),
	jpl_class_to_classname(Class, CName),
	Name = CName.

/**
	wkpl_classifier(?Name, ?Options, ?Classifier). Constructor for WEKA classifier with options
*/
wkpl_classifier(Name, Options, Classifier):-
	jpl_datums_to_array(Options, Args),
	jpl_call('weka.classifiers.Classifier', forName, [Name, Args], Classifier).

/**
	String description of the classifier. The important thing of the predicate that it gives information of the classifier status at different 
	times so we get dynamic information of the classifier and the classification set.
*/
wkpl_classifier_description(Classifier, Description):-
	jpl_call(Classifier, toString, [], Description).


/*************************** CLASSIFIER SETTING ***************************/

/**
	Set options with a prolog list ['-D', '-Q' ...]. This must be checked out for each classifier by the user. Each element of the list is an option 	 synopsis
*/
wkpl_set_options(Classifier, Options):-
	jpl_datums_to_array(Options, Args),
	jpl_call(Classifier, setOptions, [Args], _).


/**
	gets how the options are configure at a certain point
*/
wkpl_get_options(Classifier, Options):-
	jpl_call(Classifier, getOptions, [], Array),
	jpl_array_to_list(Array, Options).

/**
	Lists the possible options for a classifier
*/
wkpl_list_options(Classifier, Options):-
	jpl_call(Classifier, listOptions, [], Enum),
	jpl_enumeration_to_list(Enum, Options).

/**
	Description, synopsis for the option
*/
wkpl_option_synopsis(Option, Desc):-
	jpl_call(Option, synopsis, [], Desc).

wkpl_option_description(Option, Desc):-
	jpl_call(Option, description, [], Desc).

wkpl_option_numArguments(Option, N):-
	jpl_call(Option, numArguments, [], N).


/**
	Loads a dataset in the classifier passing a weka.Instances object that can be read from an arff file or somehow else.
	Each classifier implements this in a different way.
*/
wkpl_build_classifier(Dataset, Classifier):-
	jpl_call(Classifier, buildClassifier, [Dataset], _).


/********************************** CLASSIFICATION TOOLS  ************************/
/**
	Classifies an instance once the classifier has been built
	wkpl_classify_instance(+Instance, +Classifier):-
	This and the next one are the basic an minimal predicates for classification. 
	EVERY CLASSIFIER SHOULD IMPLEMENT ON OF THOSE METHODS, so calling them over all instances of the dataset we get the classification
	as a prolog format (at least that's the idea).
	*/

wkpl_classify_instance(Instance, Classifier, Result):-
	jpl_call(Classifier, classifyInstance, [Instance], Result).

wkpl_classify_instance_of(ListOfInstances, Classifier, Result):-
	member(Instance, ListOfInstances),
	wkpl_classify_instance(Instance, Classifier, Result).
	
/**
	Distribution fro instance. The result is a distribution (array of doubles).
	This is a method of the classes that are sublclasses of DistributionClassifier
	*/

wkpl_distributionFor_instance(Instance, Classifier, ResultList):-
	jpl_call(Classifier, distributionForInstance, [Instance], Result),
	jpl_array_to_list(Result, ResultList).

wkpl_distributionFor_instance_of(ListOfInstances, Classifier, Result):-
	member(Instance, ListOfInstances),
	wkpl_distributionFor_instance(Instance, Classifier, Result).


