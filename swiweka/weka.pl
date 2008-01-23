/**
	
	Prolog program to deal with dataSet for classification as weka::Instance objects
	David Pastor 2008	
	*/


:- style_check(-discontiguous).
:- use_module(library(jpl)).
:- [jpl_extension].


	/***************************************************************************************************
	****************************** SETTING WEKA FOR CLASSIFICATION *************************************
	***************************************************************************************************/

/**
	Creates an empty set for the given attributes and capacity. 
	*/

wkpl_create_dataSet_empty(Name, Attributes, Capacity, Instances):-
	wkpl_create_FastVector_empty(FastVector),
	wkpl_set_dataSet_attributes(Attributes, FastVector),
	wkpl_new_argsType_array(3, Args),
	wkpl_add_type_to_args('java.lang.String', Args, 0),
	wkpl_add_wekaType_to_args('weka.core.FastVector', Args, 1),
	wkpl_add_primitiveType_to_args(int, Args, 2),
	wkpl_new_array('java.lang.Object', 3, Values),
	jpl_new('java.lang.Integer', [Capacity], C),
	jpl_call('java.lang.reflect.Array', set, [Values, 0, Name], _),
	jpl_call('java.lang.reflect.Array', set, [Values, 1, FastVector], _),
	jpl_call('java.lang.reflect.Array', set, [Values, 2, C], _),
	wkpl_getObject_arg('weka.core.Instances', [Args], [Values], Instances).

/**
	Sets the attributes (given as a prolog list) of an Instances object by the associated FastVector of Attributes 
	*/

wkpl_set_dataSet_attributes(Attributes):-
	wkpl_create_FastVector_empty(FastVector),
	wkpl_insert_attributes(Attributes, FastVector).	

wkpl_insert_attributes(Attributes, FastVector):-
	wkpl_insert_first_attribute(Attributes, FastVector, Rest),
	append([],Rest,Tail),
	wkpl_insert_attributes(Tail, FastVector).
	
wkpl_insert_first_attribute(Attributes, FastVector, Rest):-	
	select(Attribute, Attributes, Rest), 
	wkpl_add_attribute(FastVector, Attribute).

/**
	Sets the Instance objects of a dataSet 
	*/






