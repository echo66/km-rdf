/**
	Interface to deal with ARFF files
	David Pastor 2008
*/

:- [dataSet].

	/***************************************************************************************************
	****************************** DEALING WITH ARFF FORMAT FILES **************************************
	***************************************************************************************************/

/* READING */

/**
	Reads an ARFF file given the path to it. 
	*/

wkpl_open_arff(ArffPath, ArffLoader):-
	wkpl_getObject('weka.core.converters.ArffLoader', ArffLoader), 
	wkpl_new_argsType_array(1, Args),
	wkpl_add_type_to_args('java.lang.String', Args, 0),
	jpl_datums_to_array([ArffPath], Values),
	wkpl_getObject('java.io.File', [Args], [Values], ArffFile),
	jpl_call(ArffLoader, setSource, [ArffFile], _).

/**
	Creates a data set by reading an ARFF file. (Automatic performance)  
	*/ 

wkpl_read_arff(ArffPath, Instances):-
	wkpl_open_arff(ArffPath, ArffLoader),
	jpl_call(ArffLoader, getDataSet, [], Instances).

/* WRITING */

/**
	Creates an ARFF file give the given Instances object (previously created). We can use it with read_arff to modify the file.
	*/

wkpl_write_arff(FilePath, Instances):-
	jpl_new('java.io.File', [FilePath], File),
	jpl_call(File, createNewFile, [], _),
	jpl_new('weka.core.converters.ArffSaver', [], S),
	jpl_call(S, setFile, [File], _),
   	jpl_call(S, setInstances, [Instances], _),
	jpl_call(S, writeBatch, [], _).

/**
	Creates an ARFF file with the full dataset. We first do the the dataset, we fill it with instance samples and then we write it to arff
	Check dataset to see how we create a dataset.
	*/ 

wkpl_create_arff(FilePath, Name, NumAttributes, NomAttributes, Capacity, ListOfInstances):-
	wkpl_create_dataSet(Name, NumAttributes, NomAttributes, Capacity, Instances),
	wkpl_fill_dataSet(Instances, ListOfInstances),
	wkpl_write_arff(FilePath, Instances).


