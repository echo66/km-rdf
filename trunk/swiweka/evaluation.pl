/**
	Prolog wrapping WEKA Classification (evaluation of classifiers). This program wraps functios of the Evaluation class.

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
*/

:-[dataSet].
:-[classifiers].


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
	processing. 
	*/

wkpl_run_classifier(ClassifierName, WekaOptions, Evaluation):-
		jpl_datums_to_array(WekaOptions, Args),
		jpl_call('weka.core.Evaluation', evaluateModel, [ClassifierName, Args], Evaluation).
	

% Train classifier. Just pass a training set to the classifier and build it. We could make a more complex training stratifying the training set
% I'm not sure this is alright...

wkpl_train_classifier(ClassifierName, Train, Classifier):-
	wkpl_classifier(ClassifierName, Classifier),
	wkpl_build_classifier(Train, Classifier).

% Train classifier with options

wkpl_train_classifier(ClassifierName, Options, Train, Classifier):-
	wkpl_classifier(ClassifierName, Options, Classifier),
	wkpl_build_classifier(Train, Classifier).

% Predictions. Get the predictions for a passed set once the Classifier is trainned. It will return a single value for methods with a numeric class and 
% a prediction within the distribution for nominal class datasets. This can be also done by retrieving the classification of each instance (are almost
% equivalent procedures).

wkpl_predictions(DataSet, Classifier, Predictions):-
	jpl_call('weka.core.Evaluation', evaluateModel, [Classifier, DataSet, _], ArrayPred),
	jpl_array_to_list(ArrayPred, Predictions).

% The same predictions but for individual instances. It varies from wkpl_classification_for/3 that the latter returns list of distribution for a nominal class

wkpl_prediction_for(Instance, Classifier, Prediction):-
	jpl_call('weka.core.Evaluation', evaluateModelOnce, [Classifier, Instance, _], Prediction).


