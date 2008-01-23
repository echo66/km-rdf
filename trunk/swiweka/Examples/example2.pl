create_music2_arff:-
	 wkpl_create_dataSet(music, [length, year], [nominal(name, []), nominal(genre, [rock, folk])], 10, X), 
	 wkpl_add_dateAttribute(X, modified, '/24/09/2000', 4), 
	 wkpl_fill_dataSet(X, [[3.45, 2000.0, mytag, 1.0, 0.0]]),
	 wkpl_write_arff('/home/david/km-rdf/swiweka/Examples/example2.arff', X).

:-create_music2_arff.
