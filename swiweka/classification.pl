/**
	Prolog wrapping WEKA Classification. It just contains the common calls to any classifier that inherit it from weka.classifier.Classifier. These 
	methods can be called separately and manually if the user knows how to do it properly. There is a wkpl_run_classifier that makes sort of the 
	same that what the calling from the terminal does in the WEKA does.

	Issues:
		-how to manage the output
		-specific calls individually
		-wrap the main method of each one

	David Pastor Escuredo, c4dm, Queen Mary, 2008.
	*/

:-[dataSet].
:-[classifiers].

			/******************************************
			******* GENERAL CLASSIFIER ****************
			******************************************/

/**
	wkpl_classifier(Name, Classifier) from classifiers.pl hides the specific creation of each classifier type.

	The name can be better seen with one example. 

	For weka.classifiers.bayes.BayesNet

	the name is 'bayes.BayesNet'

	We don't really need it as we have wkpl_classifier/3 but we may want to create with t
	*/

/**
	Creates a classifier giving the Class name of the classifier (weka.classifieres.bayes.BayesNet) and a list with the options for the classifier
	*/

wkpl_classifier(Name, Options, Classifier):-
	jpl_datums_to_array(Options, Args),
	jpl_call('weka.classifiers.Classifier', forName, [Name, Args], Classifier).
			
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
	Should run over every instance of a dataset
	*/

/**
	String description of the classifier. The important thing of the predicate that it gives information of the classifier status at different 
	times so we get dynamic information of the classifier and the classification set.
	*/

wkpl_classifier_description(Classifier, Description):-
	jpl_call(Classifier, toString, [], Description).


					/**********************************
					********* EVALUATION **************
					**********************************/

/**
	This predicate is equivalent to call the classifier from the terminal with the given options
		+Name like weka.classifier.bayes.BayesNet
		+Options: list of options as atoms ['-o /....', '-i /....', ...]
		+A long string having the evaluation of the process

	Look at the documentation about classification to see the options that are allowed generally.
	For specific options of each classifier, would be necessary to go the WEKA API
	*/

wkpl_run_classifier(ClassifierName, Options, Evaluation):-
		jpl_datums_to_array(Options, Args),
		jpl_call('weka.core.Evaluation
	

					/***********************************
					*********** OPTIONS ****************
					***********************************/

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

/*
	Lists the possible options for a classifier
	*/

wkpl_list_options(Classifier, Options):-
	jpl_call(Classifier, listOptions, [], Enum),
	jpl_enumeration_to_list(Enum, Options).

/*
	Description, synopsis for the option
	*/

wkpl_option_synopsis(Option, Desc):-
	jpl_call(Option, synopsis, [], Desc).

wkpl_option_description(Option, Desc):-
	jpl_call(Option, description, [], Desc).

wkpl_option_numArguments(Option, N):-
	jpl_call(Option, numArguments, [], N).

