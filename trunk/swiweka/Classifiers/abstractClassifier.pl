/**
	Represents the common predicates por any classifier that comes from weka.classifiers.Classifier

	David Pastor Escuredo 2008, c4dm Queen Mary
	*/

:- style_check(-discontiguous).
:- use_module(library(jpl)).

/**
	Loads a dataset in the classifier passing a weka.Instances object that can be read from an arff file or somehow else.
	Each classifier implements this in a different way.
	*/

wkpl_build_classifier(Dataset, Classifier):-
	jpl_call(Classifier, buildClassifier, [Dataset], _).

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


