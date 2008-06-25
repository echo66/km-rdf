/**
	Hiding the classifiers creation using the very specific last name in the class formal name
	Just make the whoooole list. Should be a better way!!!!! 
	David Pastor Escuredo 2008, c4dm, Queen Mary
	*/

:-[bayesNet].


/**
	Enumerate Classifiers
**/
weka_classifiers(_C, T):-
	wkpl_getObject('weka.core.ClassDiscovery', ClassDiscovery),
	jpl_call(ClassDiscovery, findPackages, [], Packages),
	jpl_call(Packages, capacity, [], L),
	Limit is L-1,
	numlist(0, Limit, List),
	member(Index, List),
	jpl_datum_to_type(Index, T).

/**
	jpl_call(Packages, get, [Index], Package),
	jpl_class_to_classname(Package, PName),
	jpl_call(ClassDiscovery, find, ['weka.classifiers.Classifier', PName], Classifiers),
	jpl_call(Classifiers, capacity, [], L2),
	Limit2 is L2-1,
	numlist(0, Limit2, Ls),
	member(I, Ls).

	jpl_call(Classifiers, get, [I], C),
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
