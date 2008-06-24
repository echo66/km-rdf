/**
	Prolog program to deal with dataSet for classification as weka::Instances objects
	This program manages the dataset representation of WEKA and its basic elements:
		-weka.Instances
		-weka.Instance
		-weka.Attribute

	It would be useful to extend this, but should be done as we need it.
	
	David Pastor 2008, c4dm, Queen Mary, Universtiy of London
*/

:-[jpl_weka].	
:-[fastVector].
:- style_check(-discontiguous).
:- load_foreign_library(swiweka).

	/*******************************************************************************
	****************************** CREATING DATASETS *******************************
	*******************************************************************************/

/**
	PREDEFINED WAY TO CREATE A DATASET (this is more as example, we are not using this on classification front-end)

	Creates an empty set (no values) for the given attributes and capacity. These predicate is designed to set numerical and nominal predicates
	as many classifiers don't support more attribute types that those ones. It's also possible on user's responsability to create a string 
	attribute like a nominal with an empty list

		+Name of the Dataset
		+List of numerical Attributes (just a list with the names)
		+List of nominal/String attributes 
			[nominal(brand, [mercedes, bmw]), nominal(name,[]), ...]

   		NOTE: Other sort of attributes may and need to be added later (just to not make this predicate so long)

		+Capacitys
		+Dataset: Weka::Instances. empty

	Note: This predicate establishes an order numerical / nominal-string attributes. We can create it empty and fill it as we want.
	Note: Other attributes can be added later on with the wkpl_add_xxx predicates, but always before filling the dataset with the data.
	*/

wkpl_create_dataSet(Name, AtVector, Capacity, Instances):-
	wkpl_add_type_to_args('java.lang.String', Args, 0),
	wkpl_add_wekaType_to_args('weka.core.FastVector', Args, 1),
	wkpl_add_primitiveType_to_args(int, Args, 2),
	wkpl_new_array('java.lang.Object', 3, Values),
	jpl_new('java.lang.Integer', [Capacity], C),
	jpl_call('java.lang.reflect.Array', set, [Values, 0, Name], _),
	jpl_call('java.lang.reflect.Array', set, [Values, 1, AtVector], _),
	jpl_call('java.lang.reflect.Array', set, [Values, 2, C], _),
	wkpl_getObject('weka.core.Instances', [Args], [Values], Instances).

/**
	This version allows to create a Instances with no attributes, so we can fill them later on
	*/

wkpl_create_empty_dataSet(Name, Instances):-
	wkpl_fastVector(FastVector),
	wkpl_new_argsType_array(3, Args),
	wkpl_add_type_to_args('java.lang.String', Args, 0),
	wkpl_add_wekaType_to_args('weka.core.FastVector', Args, 1),
	wkpl_add_primitiveType_to_args(int, Args, 2),
	wkpl_new_array('java.lang.Object', 3, Values),
	jpl_new('java.lang.Integer', [0], C),
	jpl_call('java.lang.reflect.Array', set, [Values, 0, Name], _),
	jpl_call('java.lang.reflect.Array', set, [Values, 1, FastVector], _),
	jpl_call('java.lang.reflect.Array', set, [Values, 2, C], _),
	wkpl_getObject('weka.core.Instances', [Args], [Values], Instances).

/**
	Prolog representation of an attribute to be added
*/
wkpl_add_attribute(Instances, numAt(Name), Position):-
	wkpl_add_numAttribute(Instances, Name, Position).

wkpl_add_attribute(Instances, nomAt(Name, Values), Position):-
	wkpl_add_nomAttribute(Instances, Name, Values, Position).

wkpl_add_attribute(Instances, stringAt(Name), Position):-
	wkpl_add_stringAttribute(Instances, Name, Position).

wkpl_add_attribute(Instances, dateAt(Name, Date), Position):-
	wkpl_add_dateAttribute(Instances, Name, Date, Position).

/**
	Adds a numerical attribute once the data has been already created. We only pass the name. 
	*/

wkpl_add_numAttribute(Instances, Name, Position):-
	wkpl_attribute_numerical(Name, Attribute),
	jpl_call(Instances, insertAttributeAt, [Attribute, Position], _).

/**
	Adds a nominal attribute once the data has been already created.
	*/

wkpl_add_nomAttribute(Instances, Name, Values, Position):-
	wkpl_attribute_nominal(Name, Values, Attribute),
	jpl_call(Instances, insertAttributeAt, [Attribute, Position], _).

/**
	Adds a string attribute
	*/

wkpl_add_stringAttribute(Instances, Name, Position):-
	wkpl_attribute_string(Name, Attribute),
	jpl_call(Instances, insertAttributeAt, [Attribute, Position], _).

/**
	Adds a date attribute
	*/	

wkpl_add_dateAttribute(Instances, Name, Date, Position):-
	wkpl_attribute_date(Name, Date, Attribute),
	jpl_call(Instances, insertAttributeAt, [Attribute, Position], _).

/**
	Sets the class. I AM NOT SURE HOW IMPORTANT THIS IS IN ORDER TO DO THE CLASSIFICATION AND IF IT SHOULD BE SET IN ANY CASE
	*/

wkpl_set_class(Instances, Attribute):-
	jpl_call(Instances, setClass, [Attribute], _).

wkpl_set_classIndex(Instances, Index):-
	jpl_call(Instances, setClassIndex, [Index], _). 


	/*******************************************************************************
	****************************** FILLING A DATASETS ******************************
	*******************************************************************************/

/**
	After creating the dataset (relation and attributes) we can set the values of each Isntance of the dataset
	*/

/**
	Sets the dataset with values with a list of Instance objects (records). Each record is itself a prolog list with a value for each attribute.
		+Instances,
		+[[Values1], [Values2], ...]
	*/
wkpl_fill_dataSet(_, []).
wkpl_fill_dataSet(Instances, [H|T]):-
	wkpl_add_instance(Instances, H),	
	wkpl_fill_dataSet(Instances, T).

/*
	Sets a specific instance into a the dataSet.
		+Dataset
		+[28, john, waiter, 70]
	It's user's responsability to put the values in the correct order according to the attributes distribution
	*/
wkpl_add_instance(Instances, InstanceList):-
	wkpl_numberAttributes(Instances, N),
	wkpl_instance(Instance, N),
	jpl_call(Instance, setDataset, [Instances], _),
	wkpl_set_instance(Instance, 0, InstanceList),
	jpl_call(Instances, add, [Instance], _).


				/**************************************************
				************* QUERYING A DATASET ******************
				**************************************************/
	
/**
	Gets the name of the dataset
	*/
wkpl_dataSet_name(Instances, Name):-
	jpl_call(Instances, relationName, [], Name).
	
/**
	Get a weka::Attribute from the dataset (Instances)
	*/
wkpl_get_attribute(Instances, Index, Attribute):-
	jpl_call(Instances, attribute, [Index], Attribute).

/**
	Non deterministic way to obtain all the attributes of a data set
	*/
wkpl_get_attributes(Instances, Attribute):-
	jpl_call(Instances, numAttributes, [], Num),
	Limit is Num-1,
	between(0, Limit, Index),
	wkpl_get_attribute(Instances, Index, Attribute).

/**
	Number of attributes
	*/
wkpl_numberAttributes(Instances, Number):-
	jpl_call(Instances, numAttributes, [], Number).
	
/**
	Number of instance elements
	*/
wkpl_numberInstances(Instances, Number):-
	jpl_call(Instances, numInstances, [], Number).

/**
	Get instance at
	*/
wkpl_get_instance(Instances, Index, I):-
	jpl_call(Instances, instance, [Index], I).

/**
	Non deterministic way to obtain all the attributes of a data set
	*/
wkpl_get_instances(Instances, I):-
	jpl_call(Instances, numInstances, [], Num),
	Limit is Num-1,
	between(0, Limit, Index),
	wkpl_get_instance(Instances, Index, I).

/**
	get the class attribute
	*/
wkpl_get_class(Instances, AttributeName):-
	jpl_call(Instances, classAttribute, [], At),
	wkpl_attribute_name(At, AttributeName).

/**
	Variance of one attribute
	*/
wkpl_attribute_variance(Instances, Attribute, Var):-
	jpl_call(Instances, variance, [Attribute], Var).

wkpl_attributeIndex_variance(Instances, Index, Var):-
	jpl_call(Instances, variance, [Index], Var).

/**
	Mean (numerical) or mode (nominal) of one attribute. Always like a double.
	*/ 
wkpl_attribute_meanOrMode(Instances, Attribute, Mean):-
	jpl_call(Instances, meanOrMode, [Attribute], Mean).

wkpl_attributeIndex_meanOrMode(Instances, Index, Mean):-
	jpl_call(Instances, meanOrMode, [Index], Mean).

/**
	Sort a dataset using one attribute (the index to it) as reference
	*/
wkpl_sort_for(Instances, Index):-
	jpl_call(Instances, sort, [Index], _).


				/*************************************************
				********* TESTING AND TRAINING SET ***************
				*************************************************/

/**
	Stratify if the class is nominal for a stratified cross-validation
	*/
wkpl_stratify_dataSet(Instances, NumFolds):-
	jpl_call(Instances, stratify, [NumFolds], _).

/**
	Create a testing set
	*/
wkpl_create_testSet(Instances, NumFolds, Fold, Set):-
	jpl_call(Instances, testCV, [NumFolds, Fold], Set).

/**
	Create training set
	*/
wkpl_create_trainSet(Instances, NumFolds, Fold, Set):-
	jpl_call(Instances, trainCV, [NumFolds, Fold], Set).


				/**************************************************
				*********** DATA INSTANCE MANAGEMENT **************
				**************************************************/

/**
	Set an instance. List of values for each attribute	
	wkpl_instance(-Instance, +NumberAtt).
	*/
wkpl_instance(Instance, Number):-
	wkpl_new_argsType_array(1, Args), 
	wkpl_add_primitiveType_to_args(int, Args, 0),
	wkpl_new_array('java.lang.Integer', 1, Values),
	jpl_new('java.lang.Integer', [Number], Value),
	jpl_call('java.lang.reflect.Array', set, [Values, 0, Value], _),
	wkpl_getObject('weka.core.Instance', [Args], [Values], Instance).

/**
	Sets the value for the att on the index
	*/
wkpl_set_instanceValue(Instance, Index, Value):-
	jpl_call(Instance, setValue, [Index, Value], _).

/**
	Sets the whole instance with the values passed as prolog list
		+Instance
		+Number of attributes to get the index of the record 
		+List of values for the weka:Instance
	*/
wkpl_set_instance(Instance, Index, List):-
	length(List, Length),	
	Index=Length, !, true;	
	nth0(Index, List, Value),
	length(List, Length),
	NewIndex is Index+1,
	wkpl_set_instanceValue(Instance, Index, Value),
	wkpl_set_instance(Instance, NewIndex, List).

/**
	Attaches the Instance to a dataset, we need to do this to get aware of the attributes.
	IMPORTANT: It is not enough with attaching, we have to add the INSTANCE TO THE DATASET!!
	*/
wkpl_attach_to_dataSet(Instance, Instances):-
	jpl_call(Instance, setDataset, [Instances], _).

/**
	Get the value which is always a double (Weka internal represneation) for the index given
	*/
wkpl_instance_value(Instance, Index, Value):-
	jpl_call(Instance, value, [Index], Value).


				/***************************************************
				************** DEALING WITH ATTRIBUTES *************
				***************************************************/

/**
	Creates a numerical attribute given the name. 
	*/
wkpl_attribute_numerical(Name, Attribute):-
	wkpl_new_argsType_array(1, Args),
	wkpl_add_type_to_args('java.lang.String', Args, 0),	
	jpl_datums_to_array([Name], Param),
	wkpl_getObject('weka.core.Attribute', [Args], [Param], Attribute).

/**	
	Creates a nominal attribute given the name and the range of values that it may take as prolog list
		+Name
		+Values: List of values of the range
		-Attribute
	*/
wkpl_attribute_nominal(Name, Values, Attribute):-
	wkpl_fastVector(FastVector),
	wkpl_fastVector_addList(FastVector, Values),
	wkpl_new_argsType_array(2, Args),
	wkpl_add_type_to_args('java.lang.String', Args, 0),
	wkpl_add_wekaType_to_args('weka.core.FastVector', Args, 1),
	jpl_datums_to_array([Name, FastVector], Parameters),
	wkpl_getObject('weka.core.Attribute', [Args], [Parameters], Attribute).

/**
	Creates a string attribute given the name.
	*/
wkpl_attribute_string(Name, Attribute):-
	jpl_null(Vector),
	wkpl_new_array('java.lang.String', 2, Args),
	jpl_call('java.lang.reflect.Array', set, [Args, 0, Name], _),
	jpl_call('java.lang.reflect.Array', set, [Args, 1, Vector], _),
	wkpl_getAt_constructor('weka.core.Attribute', 4, C),
	jpl_call(C, newInstance, [Args], Attribute).

/**
	Create a date attribute with a specific format for the date!
	*/
wkpl_attribute_date(Name, Date, Attribute):-
	wkpl_new_argsType_array(2, Args),
	wkpl_add_type_to_args('java.lang.String', Args, 0),
	wkpl_add_type_to_args('java.lang.String', Args, 1),
	wkpl_new_array('java.lang.Object', 2, Param),
	jpl_call('java.lang.reflect.Array', set, [Param, 0, Name], _),
	jpl_call('java.lang.reflect.Array', set, [Param, 1, Date], _),
	wkpl_getObject('weka.core.Attribute', [Args], [Param], Attribute).

/**
	Create a relational attribute
		+Name
		+Header of the relation (i'm not sure about the meaning of this)
	Check if works...
	*/
wkpl_attribute_relation(Name, Instances, Attribute):-
	jpl_datums_to_array([Name, Instances], Args),
	jpl_new('weka.core.Attribute', [Args], Attribute).

/**
	Gets the name of the attribute
	*/
wkpl_attribute_name(Attribute, Name):-
	jpl_call(Attribute, name, [], Name).

/**
	Gets the type of the attribute
	*/
wkpl_attribute_type(Attribute, Type):-
	jpl_call(Attribute, type, [], TypeNum),
	wkpl_type(TypeNum, Type).
wkpl_type(TypeNum, Type):-
	TypeNum is 0,
	Type = 'numerical'.
wkpl_type(TypeNum, Type):-
	TypeNum is 1,
	Type = 'nominal'.
wkpl_type(TypeNum, Type):-
	TypeNum is 2,
	Type = 'string'.
wkpl_type(TypeNum, Type):-
	TypeNum is 3,
	Type = 'relation-valued'.
wkpl_type(TypeNum, Type):-
	TypeNum is 4,
	Type = 'date'.



