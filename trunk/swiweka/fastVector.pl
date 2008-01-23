/**
	Module to manage Weka::FastVector
	David Pastor 2008
*/

:-[jpl_weka].

	/***************************************************************************************************
	************************************** WEKA FAST VECTORS *******************************************
	***************************************************************************************************/

/**
	Creating instances. Emptyt or with a specific capacity
	*/

wkpl_fastVector(FastVector):-
	wkpl_getObject('weka.core.FastVector', FastVector).

wkpl_fastVector(FastVector, Capacity):-
	wkpl_new_argsType_array(1, Args), 
	wkpl_add_primitiveType_to_args(int, Args, 0),
	wkpl_new_array('java.lang.Integer', 1, Values),
	jpl_new('java.lang.Integer', [Capacity], Value),
	jpl_call('java.lang.reflect.Array', set, [Values, 0, Value], _),
	wkpl_getObject('weka.core.FastVector', [Args], [Values], FastVector).

/**
	Add attribute to the vector. Check if this is this simple and correct. It is ok for strings. 
	MAY NOT WORK FOR EVERY TYPE!!!
	*/

wkpl_fastVector_add(FastVector, Attribute):-
	jpl_call(FastVector, addElement, [Attribute], _).

/**
	Add list of attributes which to the vector.
	*/

wkpl_fastVector_addList(_, []).
wkpl_fastVector_addList(FastVector, [H|T]):-
	wkpl_fastVector_add(FastVector, H),
	wkpl_fastVector_addList(FastVector, T).






