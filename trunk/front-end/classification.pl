/**
	Classification 'scenario'. This module offers an understandable logic framework to manage machine learning algorithms from prolog. This module
	relies on just one external API called WEKA. For a deeper control of WEKA possibilities, please check out swiweka.
	David Pastor Escuredo.
	June 2008, c4dm, Queen Mary University of London
**/

:-module(classification, []).

:-use_module('../swiweka/weka').

/*************************************** IDENTIFIERS *****************************/
/**
	Keeping a database of classification sets which can be modified. We use a java.util.HashTable.
	This sets only comprise weka headers (attributes) ‪like an empty dataset
*/
:- dynamic classifSets_db/2.

/**
	The same for full datasets
*/
:-dynamic dataSets_db/1.

/************************************* DATE SET MANAGMENT ****************************/

/**
	Create Classification set

		Name (particular sets with data will have some other name)
		
		-numeAt('name').
		-nomAt('name', ListOfValues).
		-stringAt('name').
		-relationAt('name', Instances).
		-dateAt('name', Date).

	Class may be a nominal attribute to set for classification, is really optional. It is necessary to pass th eindex of the attribute

	create_classifSet(+Id, +ListOfAttributes, +Class)
*/
create_classifSet(Id, ListOfAttributes, Class):-
	wkpl_fastVector(FastVector),
	set_attributes(ListOfAttributes, FastVector),
	wkpl_create_dataSet(Id, FastVector, 0, Instances),
	set_class(Class, Instances),	
	assert(classifSets_db(Id, Instances)).

set_class(_, _).
set_class(Index, Instances):-
	wkpl_set_classIndex(Instances, Index).		

set_attributes([],_).
set_attributes([H|T], F):-
	set_attribute(H, F),
	set_attributes(T, F).

set_attribute(numAt(Name), F):-
	wkpl_attribute_numerical(Name, A),
	wkpl_fastVector_add(F,A).
set_attribute(nomAt(Name, Values), F):-
	wkpl_attribute_nominal(Name, Values, A),
	wkpl_fastVector_add(F, A).
set_attribute(stringAt(Name), F):-
	wkpl_attribute_string(Name, A),
	wkpl_fastVector_add(F, A).
set_attribute(dateAt(Date), F):-
	wkpl_attribute_date(Date, A),
	wkpl_fastVector_add(F, A).
%so far fails to put a relation attribute

/**
	They can be saved i/o to arff files.
		Giving the ID and a path, we create an ARFF to store the set up that can be retrieved by passing the file path (the id is read from the file and returned to prolog)

	save_classifSet(-ID, +ArffFilePath)

	load_classifSet(+ArffFilePath, -ID)
*/
save_classifSet(ID, ArffFilePath):-
	classifSets_db(ID, Instances),
	wkpl_write_arff(ArffFilePath, Instances).

load_classifSet(ArffFilePath, ID):-
	wkpl_read_arff(ArffFilePath, Instances),
	wkpl_dataSet_name(Instances, ID),
	assert(classifSets_db(ID, Instances)).

/**
	FILLING DATA

	[[valueAt1, valueAt2, valueAt3, ...,valueAtN]|...].

	two ways:

	1.
		Giving a ClassifSet id which retrieves the original classification environment without data.
		Passing DataSetID to store the new filled dataset (different from the classifsetID

	2.	Retrieving your own dataset with your own id so classifID is not passed
*/
insert_records(ClassifID, DataSetID, Data):-
	nonvar(ClassifID), nonvar(DataSetID),
	classifSets_db(ClassifID, Set),
	copy_term(Set, MySet),
	wkpl_fill_dataSet(MySet, Data),
	assert(dataSets_db(DataSetID, MySet)).	

insert_records(ClassifID, DataSetId, Data):-
	var(ClassifID), nonvar(DataSetId),
	dataSets_db(DataSetId, Set),
	wkpl_fill_dataSet(Set, Data).


/**
	They can be saved i/o to arff files.
		Giving the ID (of a dataset not a classif set) and a path, we create an ARFF to store the set up that can be retrieved by passing the file path. Now, we need to give the id as the id of the weka class is not our id to handle our particular data set

	save_dataSet(-ID, +ArffFilePath)

	load_dataSet(+ArffFilePath, +ID)
*/
save_dataSet(ID, ArffFilePath):-
	dataSets_db(ID, Instances),
	wkpl_write_arff(ArffFilePath, Instances).

load_dataSet(ArffFilePath, ID):-
	wkpl_read_arff(ArffFilePath, Instances),
	assert(dataSets_db(ID, Instances)).

/******************************************** CLASSIFIERS ***************************************/	

/**
	Queries/check available classifiers
*/
classifier(Classifier):-
	wkpl_classifier(Classifier).

/**
	Hierarchy of classifiers. It allows the user to find the desired classifier

	classifier_type(?Name, ?Type, ?Subtype, ?ClassifierClassName)
*/
classifier_type(Name, Type, Subtype, Classifier):-
	wkpl_classifier(Classifier),
	atom_concat('weka.classifiers.', TypeName, Classifier),
	concat_atom(List, '.', TypeName),
	nth0(0, List, Type),
	rest_name(List, Name, Subtype).
rest_name(List, N, _):-
	length(List, A),
	A = 2,
	nth0(1, List, N).
rest_name(List, N, S):-
	length(List, A),
	A = 3,
	nth0(1, List, S),
	nth0(2, List, N).
	
/**
	set_classifier(+Classifier, +DataSetID, +ListOfOptions, -Object)

	Classifier: class name 'weka.classifiers.x.y'
	DataSetID: member of dataSets_db/1
	ListOfOptions: ['-D', '-O'] Options must be checked in WEKA DOC or through the swiweka interface predicates ([classifiers.pl]). Can be empty
	Object: a JPL reference to the classifier set
*/
set_classifier(Classifier, DataSetID, Options, Object):-
	classifier(Classifier),
	create_classifier(Classifier, Options, Object),
	dataSets_db(DataSetID, DataSet),
	wkpl_build_classifier(DataSet, Object).

create_classifier(Classifier, Options, Object):-
	var(Options),
	wkpl_classifier(Classifier, Object).
create_classifier(Classifier, Options, Object):-
	nonvar(Options),
	Options = [],
	wkpl_classifier(Classifier, Object).
create_classifier(Classifier, Options, Object):-
	is_list(Options),
	length(Options, L),
	L>0,
	wkpl_classifier(Classifier, Options, Object).

/**
	TODO:
		Training sets
		Retrieve results
**/

