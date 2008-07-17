/**

	Example of how to create a dataSet giving attributes, fill it with data and output in a arff file
	Note that any value is a double within the weka internal representation
	*/

create_music_arff:-
	 wkpl_create_dataSet(music, [length, year], [nominal(genre, [rock, folk])], 10, X), 
	 wkpl_fill_dataSet(X, [[3.84, 1997.0, 0.0], [5.98, 2000.0, 1.0]]),
	 wkpl_write_arff('../km-rdf/swiweka/Examples/example1.arff', X),
	 wkpl_numberInstances(X, L).

:-[dataSet],create_music_arff.
