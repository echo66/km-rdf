
wkpl_create_testSet:-		

	wkpl_read_arff('/home/david/km-rdf/swiweka/Examples/example3.arff', X),
	wkpl_create_testSet(X, 2, 0, Y), 
	wkpl_write_arff('/home/david/km-rdf/swiweka/Examples/example4.arff', Y).

:-wkpl_create_testSet.
