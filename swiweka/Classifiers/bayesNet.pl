/**
	Wrapping Specific functionality of the weka.classifier.bayes.BayesNet. This program will be loaded by classifiers.
	David Pastor 2008
	*/

:-[abstractClassifier].

/* This classifier returns a distribution so a lis of doubles */

wkpl_is_bayesNet(Classifier):-
	jpl_ref_to_type(Classifier, Type),
	jpl_type_to_classname(Type, Name),
	Name = 'weka.classifiers.bayes.BayesNet'.

wkpl_classify_dataSet(Classifier, Dataset, Classification):-
	wkpl_is_bayesNet(Classifier),
	wkpl_build_classifier(Dataset, Classifier),
	jpl_call(Dataset, enumerateInstances, [], Enum),
	jpl_enumeration_to_list(Enum, Instances),
	findall(Result, wkpl_distributionFor_instance_of(Instances, Classifier, Result), Classification).
	
wkpl_get_graph(Classifier, Graph):-
	wkpl_is_bayesNet(Classifier),
	jpl_call(Classifier, graph, [], Graph).


	
