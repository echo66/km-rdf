/**
	This foreign module defines a database to work with several instances simultaneously. It follows the same paradigm of ids used 
	in some other bits in the KM.
	David Pastor 2008

	NO FUNCIONAAAAAA
*/

#include <iostream>
#include <cstdlib>
#include <qstring.h>
#include <SWI-cpp.h>

/**
	Definition of the database. 
	We basically keep track of duple containing the term pointing to the java Weka.Instances oject and an ID to refer to it
	like __dataset_1.
	Note that it is not interesting to store in disk this data as we have the arff files to do so and can be easily input and output.
*/

static struct dataset{

	QString id; //ID
	term_t set; //The Instances term
}

instances_db[10000];//maximum of datasets handled at the same time by the km.

/**
	Incremental count of datasets for this session
*/
size_t dataset_count = 0;

/** Protoype **/
long
existing_id(const char*);

		/**************************************************
		********************* PREDICATES ******************
		**************************************************/

/**
	This predicate returns an id for the dataset given
	*/
PREDICATE(wkpl_id_for_dataset, 2){

	//+dataset
	//-id

	term_t dataset = PL_new_term_ref();
	dataset = term_t(PlTerm(A1));
	
	//generating id
	QString head("__dataset_");
	QString var;
	var = QString("%1")
		.arg((long)dataset_count);
	head.append(var);
	
	//storing
	instances_db[dataset_count].id = head;
	instances_db[dataset_count].set = PL_new_term_ref();
	instances_db[dataset_count].set = dataset;

	if(dataset_count>10000) return false;
	dataset_count++;
	return A2 = PlAtom(head.toLocal8Bit().data());
}

/**
	This one gives us the term given the id
 	*/

PREDICATE(wkpl_dataset_from_id, 2){

	//+id
	//-dataset

	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = term_t(PlTerm(A1));
	PL_get_atom_chars(id_t,&id);

	std::cerr<<(const char*)id<<std::endl;
	
	long flag = existing_id((const char*)id);
	if(flag<0) return false;
	
	std::cerr<<flag<<std::endl;
	return A2 = PlTerm(instances_db[flag].set);
}


/********************************************
********** Foreign functions ****************
********************************************/	

/**
	Gets the position of the id
	*/
long
existing_id(const char* ident)
{
	int flag = -1;
	size_t pos = 0;
	QString input(ident);
	for(size_t i=0; i<dataset_count; i++){
		
		if(input.compare(instances_db[i].id)==0) {//this check should be correctly defined
			flag = 0;
			pos = i;
		}	
	}
	if(flag == 0){
		return pos;
	}
	return flag;
}



