/**
	Hiding the classifiers creation using the very specific last name in the class formal name
	Just make the whoooole list. 
	David Pastor Escuredo 2008, c4dm, Queen Mary
	*/

:-[bayesNet].

/** LOADING EACH INTERFACED CLASSIFIER **/

/**
	The number should be the rest of the class name deleting the commong weka.classifiers. Good idea¿?
	*/

wkpl_classifier(Name, Classifier):-
	Name = 'bayes.BayesNet',
	wkpl_getObject('weka.classifiers.bayes.BayesNet', Classifier).

wkpl_classifier(Name, Classifier):-
	Name = 'bayes.NaiveBayes',
	wkpl_getObject('weka.classifiers.bayes.NaiveBayes', Classifier).	
