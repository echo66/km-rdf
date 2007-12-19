/**
 * SWI-Prolog external interface to c4dml
 * -- MFCC computation
 * NOTE: must add better memory management...
 * copyright (c) Yves Raimond 2006
 */

#include <SWI-Prolog.h>
#include <stdio.h>


#include "lib_feature.h"
#include "utils.h"

#include "lib_files.h"
#include "lib_buffer.h"



install_t install();
foreign_t c4dml_read(term_t af, term_t start, term_t end, term_t samplerate, term_t array);
foreign_t c4dml_mfcc(term_t to_analise, term_t length, term_t samplerate, term_t featdim, term_t mfcc_term);
double * extract_mfcc(double * toanalise,int samplerate, long length, int featDim);
term_t to_prolog_list(double * in, long length);
double * to_c_list(term_t in, long length);
	


install_t install(){
	PL_register_foreign("c4dml_mfcc",5, (void *) c4dml_mfcc, 0);
	PL_register_foreign("c4dml_read",5, (void *) c4dml_read, 0);
}

/**
 * An external predicate allowing to read audiofiles -- `seek' access
 */
foreign_t c4dml_read(term_t af, term_t start, term_t end, term_t samplerate, term_t array){
	au_file_t * au_file;
	char * filepath;
	int sr;
	long s, e;
	double * read;
	term_t sr_temp = PL_new_term_ref();
	term_t array_temp;
	int r1;
	int r2;
	
	PL_get_atom_chars(af,&filepath);
	if ((au_file=au_open(filepath, AU_MONO))==NULL)
	{
		fprintf(stderr, "Error loading audio file...\n");
		return FALSE;
	}
	
	PL_get_long(start,&s);
	PL_get_long(end,&e);
	
	au_file->au_seek(s,SEEK_SET);
	double** buf = init_buffer(e - s, e - s, au_file->au_read_double, NULL);
	long samples = fill_buffer();
	if(samples<0) return FALSE;
	read = buf[0];
	
	sr = au_file->samplerate;
	PL_put_integer(sr_temp,sr);

	au_close(au_file);

	array_temp = to_prolog_list(read, e - s);
	
	r1 = PL_unify(samplerate,sr_temp);
	r2 =  PL_unify(array,array_temp);
	
	//fprintf(stderr, "READING OK...\n");
	
	if(r1==0||r2==0){
		return FALSE;
	}
	else{
		return TRUE;
	}
}


/**
 * An external prolog predicate, allowing to compute MFCC from a prolog array of doubles
 */
foreign_t c4dml_mfcc(term_t to_analise, term_t length, term_t samplerate, term_t featdim, term_t mfcc_term){
	long le;
	int sr,fd;
	double * data;
	double * mfcc;
	term_t feat;

	fprintf(stderr, "Entering MFCC computation...\n");
	
	PL_get_long(length,&le);//type checks?
	PL_get_integer(samplerate,&sr);
	PL_get_integer(featdim,&fd);
	
	//fprintf(stderr, "converting prolog list to c list...\n");
	data = to_c_list(to_analise,le);
	//fprintf(stderr, "...done\n");
	
	mfcc = extract_mfcc(data,sr,le,fd);
	
	//free(data);
	
	feat = to_prolog_list(mfcc,fd);
	
	//free(data);
	fprintf(stderr, "MFCC Computation OK...\n");
	return PL_unify(mfcc_term,feat);
}

/**
 * Extract MFCC on the given array of doubles (the signal has already been framed)
 */
double * extract_mfcc(double * toanalise,int samplerate, long length, int featDim){
	long windowsize = length;
	feature_params_t *feature_params = (feature_params_t*) calloc(1,sizeof(feature_params_t));
	
	
	feature_params->feature_names = (char**)calloc(1,sizeof(char*));
	feature_params->feature_names[0] = "MFCC";
	feature_params->feature_nbr = 1;
	feature_params->frame_length = windowsize;
	feature_params->sampling_rate = samplerate;
	feature_params->feature_dimension = (int*)calloc(1,sizeof(int*));
	feature_params->feature_dimension[0] = featDim;

	
	double ** features = init_feature(feature_params);
	double * mfcc = features[0];	
	
	fprintf(stderr, "Feature extraction...\n");
	//enhance error management here:-)
	extract_feature(toanalise,windowsize);
	fprintf(stderr,"End of feature extraction...\n");
	if(valid(toanalise,windowsize)==0)
		printf("No feature...\n");
	
	free(feature_params);
	
	//fprintf(stderr,"Returning MFCC...\n");
	return mfcc;
}

/**
 * Converts a C list (double *) to a prolog list of prolog floats
 */
term_t to_prolog_list(double * in, long length){
	long i;
	term_t list = PL_new_term_ref();
	term_t num;
	
	PL_put_nil(list);
	for(i=0;i<length;i++){
		num = PL_new_term_ref();
		//printf("%f\n",in[i]);
		PL_put_float(num,in[i]);
		PL_cons_list(list,num,list);
	}

	return list;
}

/**
 * Converts a prolog list of prolog floats to a C list (double *)
 */
double * to_c_list(term_t in, long length){
	term_t head = PL_new_term_ref();
	term_t tail = PL_new_term_ref();
	term_t templist = in;
	
	double * list = (double *) calloc(length,sizeof(double));
	long k = 0;
	
	
	while(PL_get_nil(templist)==0){
		PL_get_list(templist,head,tail);
		PL_get_float(head,&list[k]);
		k++;
		templist = tail;
	}

	return list;		
}

