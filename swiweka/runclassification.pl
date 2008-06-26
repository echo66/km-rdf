/**
	Prolog wrapping WEKA Classification. It just contains the common calls to any classifier that inherit it from weka.classifier.Classifier. These 
	methods can be called separately and manually if the user knows how to do it properly. There is a wkpl_run_classifier that makes sort of the 
	same that what the calling from the terminal does in the WEKA does.

	This program imports classifiers which is a program to define specific-classifier predicates that will be hide within this program using the 
	same name for any of them.

	David Pastor Escuredo, c4dm, Queen Mary, 2008.
	*/

:-[dataSet].
:-[classifiers].


wkpl_classifier_description(Classifier, Description):-
	jpl_call(Classifier, toString, [], Description).


					/*****************************************************
					********* EVALUATION AND CLASSIFICATION **************
					*****************************************************/

/**
	This predicate is equivalent to call the classifier from the terminal with the given options
		+Name like weka.classifier.bayes.BayesNet. It's not an actual instance!!
		+Options: list of options as atoms ['-o /....', '-i /....', ...]
		+A long string having the evaluation of the process

	Look at the documentation about classification to see the options that are allowed generally.
	For specific options of each classifier, would be necessary to go the WEKA API
	
	This is not the best way to run a classifier within the KM as we need to handle the acutal statistics of each instance for further 
	processing. This may require a lot of effor to set a proper, sensible and handy way to deal with WEKA. Working on it...
	*/

wkpl_run_classifier(ClassifierName, Options, Evaluation):-
		jpl_datums_to_array(Options, Args),
		jpl_call('weka.core.Evaluation', evaluateModel, [ClassifierName, Args], Evaluation).
	

/**
	This a lower level alternative predicate to classify and run a classifier. We don't get as much information as with the previous one but we 
	can handle the results in prolog for further management.
		+Classifier
		-Classification
	
	This is a very simple call which needs important considerations:
		1. The classifier must be already an instance with the set options but not built with the dataset.
		2. The classification can be done with:
			2.1. wkpl_classify_instance/2 and we get just one value for each instance, so at the end we get a simple list of values
			with the classification of the instances (value within the class attribute I guess).

			2.2. wkpl_distributionFor_instance/2 and we get a distribution of values for each instance as a prolog list and thefore a list
			of sublists for the whole dataset.

	The specific way is hidden by using classifiers.pl. Every interfaced classifier needs a specific prolog program for it implementing this
	predicate and will be loaded by classifiers.
	*/

wkpl_classify(Classifier, Dataset, Classification):-
	wkpl_classify_dataSet(Classifier, Dataset, Classification).



