/**

	Prolog wrapping WEKA Classification
	David Pastor 2008
	*/

:-[dataSet].
:-[classifiers].

			/******************************************
			******* SETTING CLASSIFIER ****************
			******************************************/

/**
	wkpl_classifier(Name, Classifier) from classifiers.pl hides the specific creation of each classifier type.

	The name can be better seen with one example. 

	For weka.classifiers.bayes.BayesNet

	the name is 'bayes.BayesNet'

	We don't really need it as we have wkpl_classifier/3
	*/

/**
	Creates a classifier giving the Class name of the classifier (weka.classifieres.bayes.BayesNet) and a list with the options for the classifier
	*/

wkpl_classifier(Name, Options, Classifier):-
	jpl_datums_to_array(Options, Args),
	jpl_call('weka.classifiers.Classifier', forName, [Name, Args], Classifier).
			
/**
	Set options with a prolog list ['-D', '-Q' ...]. This must be checked out for each classifier by the user.
	*/

wkpl_set_options(Classifier, Options):-
	jpl_datums_to_array(Options, Args),
	jpl_call(Classifier, setOptions, [Args], _).

/**
	Loads a dataset in the classifier passing a weka.Instances object that can be read from an arff file or somehow else.
	Each classifier implements this in a different way.
	*/

wkpl_build_classifier(Dataset, Classifier):-
	jpl_call(Classifier, buildClassifier, [Dataset], _).

/**
	Classifies an instance once the classifier has been built
	wkpl_classify_instance(+Instance, +Classifier):-
	*/

wkpl_classify_instance(Instance, Classifier, Result):-
	jpl_call(Classifier, classifyInstance, [Instance], Result).

/**
	Distribution fro instance. The result is a distribution
	This is a method of the classes that are sublclasses of DistributionClassifier
	*/

wkpl_distributionFor_instance(Instance, Classifier, Result):-
	jpl_call(Classifier, distributionForInstance, [Instance], Result).

/**
	One single predicate to make classification.
	Should run over every instance of a dataset.
	*/

wkpl_classify_dataset(Dataset(Instance, Classfier, Result):-
	wkpl_classify_instance(Instance, Classifier, Result).

wkpl_classify(Instance, Classifier, Result):-
	wkpl_distributionFor_instance(Instance, Classifier, Result).


	
