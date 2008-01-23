
/**
	Sets a class, builds a BayesNet classifier with it
	*/

bayesNet_example3:-
	wkpl_read_arff('/home/david/km-rdf/swiweka/Examples/example3.arff', X), 
	wkpl_set_classIndex(X, 2), 
	wkpl_classifier('bayes.BayesNet', Y), 
	wkpl_build_classifier(X, Y),
	wkpl_write_arff('/home/david/km-rdf/swiweka/Examples/example3.arff', X).


:-bayesNet_example3.
