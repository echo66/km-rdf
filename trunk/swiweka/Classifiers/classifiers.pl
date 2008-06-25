/**
	Hiding the classifiers creation using the very specific last name in the class formal name
	Just make the whoooole list. Should be a better way!!!!! 
	David Pastor Escuredo 2008, c4dm, Queen Mary
	*/

:-[bayesNet].


/**
	jpl_call(ClassDiscovery, find, ['weka.classifiers.Classifier', PName], Classifiers),
	jpl_call(Classifiers, capacity, [], L2),
	Limit2 is L2-1,
	numlist(0, Limit2, Ls),
	member(I, Ls),
	jpl_new('java.lang.Integer', [I], In2),
	jpl_call(Classifiers, get, [In2], C),
	jpl_class_to_classname(C, Classifier).



 LOADING EACH INTERFACED CLASSIFIER **/

/**
	The number should be the rest of the class name deleting the commong weka.classifiers. Good idea¿?
	*/

wkpl_classifier(Name, Classifier):-
	Name = 'bayes.BayesNet',
	wkpl_getObject('weka.classifiers.bayes.BayesNet', Classifier).

wkpl_classifier(Name, Classifier):-
	Name = 'bayes.NaiveBayes',
	wkpl_getObject('weka.classifiers.bayes.NaiveBayes', Classifier).	
