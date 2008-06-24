/**
	This is a prolog file defining more jpl utils that make easier dealing with the interface. It is mainly oriented to create instances. 
	Need to set correctly paths in wkpl_weka_loader
	David Pastor 2008.
*/

:- style_check(-discontiguous).
:- use_module(library(jpl)).


	/***************************************************************************************************
	****************************** SETTING WEKA ON A PROLOG ENGINE *************************************
	***************************************************************************************************/

/**
	Predicate to load the weka jar to the classpath (seems to be useless)
	*/ 

wkpl_add_weka_to_classpath:-
	 jpl_call('java.lang.System', setProperty, ['java.class.path','/usr/local/lib/pl-5.6.34/lib/jpl.jar:/home/david/weka-3-5-6/weka.jar:/home/david/weka-3-5-6/weka-src.jar:.'], _).  

/**
	This predicate establishes a connection with the weka jars and returns a loader which loads any class from the weka package
	IMPORTANT: This should be asserted in memory to avoid the execution of the subqueries each time we call the loader to load a class!!!!
	ToDo: Set the Path in the host!
	*/ 

jar_weka('jar:file:/home/david/weka-3-5-7/weka.jar!/').
jar_srcweka('jar:file:/home/david/weka-3-5-7/weka_src.jar!/').

wkpl_set_weka_path(Path):-
	flag('path', _, Path).

wkpl_weka_loader(Loader):-
	jar_weka(JAR),
	jar_srcweka(JARSRC),
	jpl_new('java.net.URL', [JAR], Url1),
	jpl_new('java.net.URL', [JARSRC], Url2),
	wkpl_new_array('java.net.URL', 2, Array),
	jpl_call('java.lang.reflect.Array', set, [Array, 0, Url1], _),
	jpl_call('java.lang.reflect.Array', set, [Array, 1, Url2], _),
	jpl_new('java.net.URLClassLoader', [Array], Loader).


	/***************************************************************************************************
	****************************** GENERAL PURPOSE PREDICATES ******************************************
	***************************************************************************************************/

/**
	wkpl_call_method(+Object or +Class, +Method, +Parameters, -Return). This is just a wrapping function to not JPL users so they can 
	call a method of any java API from swiweka
	P is a list of arguments.
	*/

wkpl_call_method(O, M, P, R):-
	jpl_call(O, M, P, R).

/**
	Create an array of the type given as Name (encapsulated in a Class object) with the length specified. This is useful to pass
	attributes in the jpl interface
	*/ 

wkpl_new_array(Name, Length, Array):-	
	jpl_call('java.lang.Class', forName, [Name], Class),
	jpl_call('java.lang.reflect.Array', newInstance, [Class, Length], Array).

/**
	Convenience predicates to call the constructors of the WEKA objects to pass the arguments types and values
	*/ 

wkpl_new_argsType_array(Length, Args):-
	wkpl_new_array('java.lang.Class', Length, Args).

wkpl_add_type_to_args(Type, Args, Position):-
	jpl_call('java.lang.Class', forName, [Type], TC),
	jpl_call('java.lang.reflect.Array', set, [Args, Position, TC], _).

wkpl_add_wekaType_to_args(Name, Args, Position):-
	wkpl_weka_loader(Loader),
	jpl_call(Loader, loadClass, [Name], Class),	
	jpl_call('java.lang.reflect.Array', set, [Args, Position, Class], _).

wkpl_add_primitiveType_to_args(Type, Args, Position):-
	jpl_type_to_class(Type, Class),
	jpl_call('java.lang.reflect.Array', set, [Args, Position, Class], _).

/**
	Several predicates to query about constructors and their attributes given the name of the class
	*/ 

wkpl_get_numberConstructors(Name, Number):-
	wkpl_get_constructors(Name, Constructors),
	jpl_call('java.lang.reflect.Array', getLength, [Constructors], Number).

wkpl_get_constructors(Name, Constructors):-
	wkpl_weka_loader(Loader),
	jpl_call(Loader, loadClass, [Name], Class),
	jpl_call(Class, getConstructors, [], Constructors).

wkpl_getAt_constructor(Name, Index, Constructor):-
	wkpl_get_constructors(Name, Constructors),	
	jpl_call('java.lang.reflect.Array', get, [Constructors, Index], Constructor).

wkpl_get_numberOfParameters_constructor_at(Name, Index, Number):-
	wkpl_get_paramTypes_of_constructor_at(Name, Index, Parameters),
	jpl_call('java.lang.reflect.Array', getLength, [Parameters], Number).

wkpl_get_paramTypes_of_constructor_at(Name, IndexCons, Parameters):-
	wkpl_getAt_constructor(Name, IndexCons, Constructor),
	jpl_call(Constructor, getParameterTypes, [], Parameters).

wkpl_getAt_paramType_of_constructor_at(Name, IndexCons, IndexParam, ParamType):-
	wkpl_get_paramTypes_of_constructor_at(Name, IndexCons, ParamTypes),
	jpl_call('java.lang.reflect.Array', get, [ParamTypes, IndexParam], P),
	jpl_call(P, getName, [], ParamType).

/**
	Generic predicate to retrieve an object from the WEKA API built by the default constructor. The errors of the constructor are propagated. Use 		the predicate for arguments if the default constructor is not correct.		
	*/

wkpl_getObject(Name, Object):-
	wkpl_weka_loader(Loader),
	jpl_call(Loader, loadClass, [Name], Class),
	jpl_call(Class, newInstance, [], Object).

/**
	Generic predicate to retrieve an object from the WEKA API given  
		+Canonical name of the class
		+List with an array containing the parameters types
		+List of with an array containing the values of the parameters
		
		THIS DOESN'T WORK FOR EVERY ARG TYPE ARRAY!!!!!!!!!!!!!!!!!!
	*/

wkpl_getObject(Name, ParamTypes, ParamValues, Object):-
	wkpl_weka_loader(Loader),
	jpl_call(Loader, loadClass, [Name], Class),
	jpl_call(Class, getConstructor, ParamTypes, Constructor),
	jpl_call(Constructor, newInstance, ParamValues, Object).




