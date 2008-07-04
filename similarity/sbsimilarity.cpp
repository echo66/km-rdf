/**
	This is the SoundBite similarity mechanism applied for the KM equivalent to SBFeatureVector. This module returns an statistic distance which 
	allow the creation of playlists.
	We use the mean and variance of a the MFCC's to extract the distance using (also from chromagram).
	David Pastor Escuredo, Jan 2008, c4dm, Queen Mary.

	ToDo: add beat and chromatic similarities.
	*/

/**
	Let's remember how a swivamp feature looks like:
	
		'Feature'(type, MO::timestamp, '__data_id').

	*/

#ifndef _FLI_H_INCLUDED
#include <SWI-cpp.h>
#endif

#include <iostream>
#include <vector>
#include <blobid.h>
#include <cmath>

using namespace std;

int
smpl_get_data(term_t, vector<double> *);

double 
distanceDistribution(vector<float> *, vector<float> *, bool);

						/*******************************************
						******** FOREIGN PREDICATES ****************
						*******************************************/

//--------------------------------------------------------------------------
// Gets the distance using mfcckldiv (taken from libsoundbite). This is only valid for MFCC statistics (mean and variances that model a gaussian statistically)
//	
//			Input is the data id for each feature (mean and variance of both tracks)
//			Returns a distance between the audiofiles with the features belong to
//			This features come from the similarity vamp plugin. The means and variances can be also extracted from the chromagram.
//
//--------------------------------------------------------------------------

PREDICATE(smpl_mfcc_kldiv, 5){

	//+Mfcc means for the first track
	//+Mfcc variances for the first track
	//+Mfcc means for the second one
	//+Mfcc variances for the second one
	//-Distance (double)

	//Audiofile 1
	cerr<<"distance..."<<endl;
	vector<double> *mfccmean_1;
	vector<double> *mfccvar_1;
	mfccmean_1 = new vector<double>();
	mfccvar_1 = new vector<double>();

	if(smpl_get_data(term_t(PlTerm(A1)), mfccmean_1)<0) return false;
	if(smpl_get_data(term_t(PlTerm(A2)), mfccvar_1)<0) return false;

	//Audiofile 2
	vector<double> *mfccmean_2;
	vector<double> *mfccvar_2;
	mfccmean_2 = new vector<double>();
	mfccvar_2 = new vector<double>();

	if(smpl_get_data(term_t(PlTerm(A3)), mfccmean_2)<0) return false;
	if(smpl_get_data(term_t(PlTerm(A4)), mfccvar_2)<0) return false;

	
	//may need some checkings if there are vectors or not

	int K = mfccmean_1 -> size();		// dimensionality i.e. number of MFCCs

	if(K==0) return false;
	
	double d = -2.0 * K;
	
	for (int i = 0; i < K; i++)
	{
		d += mfccvar_1->at(i) / mfccvar_2->at(i) + mfccvar_2->at(i) / mfccvar_1->at(i);
		d += (mfccmean_1->at(i) - mfccmean_2->at(i)) * (1.0 / mfccvar_1->at(i) + 1.0 / mfccvar_2->at(i)) * (mfccmean_1->at(i) - mfccmean_2->at(i));
	}

	//isNan for windows???	

	//freeing dynamic memory
	delete mfccmean_1;
	delete mfccvar_1;
	delete mfccmean_2;
	delete mfccvar_2;

	return A5 = PlTerm(d);
}

//------------------------------------------------------
//	Now the input is the WholeFeature. We get statistics that
//	model the mfcc as just one Gaussian wichi is still meaningful
//	for timbral similarity (Mark Levy) 
//------------------------------------------------------
PREDICATE(smpl_mfcc_gaussian_parameters, 2){

	//+MFCC coefficients
	//+List with the gaussian parameters

	//-MFCC means (__data_id) Note that these values are just the means and variances and 
	//not a 'Feature' functor (they can be also retrived like that using the similarity vamp plugin
	//-MFCC variances

	cerr<<"gaussian"<<endl;
	PlTail mfccList(A1);
	PlTail mfccList2(A1);
	PlTerm mfccFrame;

	vector<float> *means;
	vector<float> *vars;
	
	size_t frames = 0; //numer of frames;
	//Means
	while(mfccList.next(mfccFrame)){
		
		frames++;
		vector<float> *coeff;
	
	        char *id;
		term_t id_t = PL_new_term_ref();
		id_t = term_t(mfccFrame[3]);//getting the data id
		PL_get_atom_chars(id_t,&id);		
		BLOBID::get_data_for_id((const char*)id, coeff);

		//Init vectors
		if(frames==1){
			cerr<<coeff->size()<<endl;
			means = new vector<float>(coeff->size());
			vars = new vector<float>(coeff->size());
		}


		//calculating the mean 
		for(size_t j=0; j<coeff->size(); j++){
			
			if(frames==1){
				means->at(j)=0; //init just in case
			}			

			means -> at(j) += coeff -> at(j);//adding coefficients
		}		
	}
	for(size_t r = 0; r<means->size(); r++){
		
		means->at(r) /= frames;
	}
	
	cerr<<frames<<endl;

	//Variances
	while(mfccList2.next(mfccFrame)){
		
		vector<float> *coeff;
	        char *id;
		term_t id_t = PL_new_term_ref();
		id_t = term_t(mfccFrame[3]);//getting the data id
		PL_get_atom_chars(id_t,&id);
		
		BLOBID::get_data_for_id((const char*)id, coeff);
		//cerr<<coeff->size()<<endl;

		//calculating the mean and the frame
		for(size_t j=0; j<coeff->size(); j++){
			
			if(frames==1){
				vars->at(j)=0; //init just in case
			}

			vars -> at(j) += (coeff -> at(j)-means -> at(j))*(coeff -> at(j)-means -> at(j));//adding coefficients
		}		
	}
	for(size_t r = 0; r<vars->size(); r++){
		
		vars->at(r) /= frames;
	}

	PlTerm means_t(PlAtom(BLOBID::assign_data_id(means)));
	PlTerm vars_t(PlAtom(BLOBID::assign_data_id(vars)));
	PlTerm gaussian;
	PlTail tail(gaussian);
	tail.append(means_t);
	tail.append(vars_t);
	tail.close();

	return A2 = gaussian;
}


//--------------------------------------------------------------------
//   Calculate a Kullback-Leibler divergence of two probability
//   distributions.  Input vectors must be of equal size.  
//   We assume in not symmetrised
    
//
//   This just needs the chroma means extracted from the chromagram plugin
//--------------------------------------------------------------------
 
PREDICATE(smpl_chroma_kldiv, 3){

	//+ probability distribution track1
	//+ probability distribution track2
	//+ symmetrised flag. Active
	//- Distance

	vector<float> *hist1;
	vector<float> *hist2;

	char *id1;
	term_t id_t1 = PL_new_term_ref();
	id_t1 = term_t(PlTerm(A1));//getting the data id
	PL_get_atom_chars(id_t1,&id1);

	char *id2;
	term_t id_t2 = PL_new_term_ref();
	id_t2 = term_t(PlTerm(A2));//getting the data id
	PL_get_atom_chars(id_t2,&id2);

	BLOBID::get_data_for_id((const char*)id1, hist1);
	BLOBID::get_data_for_id((const char*)id2, hist2);

	return A4 = PlTerm((double)distanceDistribution(hist1, hist2, true));
}

//---------------------------------------------------------------
//---------- Cosine distance for beat Spectra. Kurt Jacobson
//---------------------------------------------------------------	

PREDICATE(smpl_cosine_distance, 3){

	cerr<<"cosine"<<endl;

	vector<double> *beatspec1;
	vector<double> *beatspec2;
	
	double dist, dDenTot, dDen1, dDen2, dSum1;

	beatspec1 = new vector<double>();
	beatspec2 = new vector<double>();

	if(smpl_get_data(term_t(PlTerm(A1)), beatspec1)<0) return false;
	if(smpl_get_data(term_t(PlTerm(A2)), beatspec2)<0) return false;


    	dist = 1.0; dDenTot = 0; dDen1 = 0; dDen2 = 0; dSum1 =0;

    	//check if v1 v2 same size
    	if (beatspec1->size() != beatspec2->size())
    	{
      	  cerr << "CosineDistance::distance: ERROR: vectors not the same size\n";
      	  return false;
    	}
    	else
    	{
        for(int i=0; i<beatspec1->size(); i++)
        {
            dSum1 += beatspec1->at(i)*beatspec2->at(i);
            dDen1 += beatspec1->at(i)*beatspec1->at(i);
            dDen2 += beatspec2->at(i)*beatspec2->at(i);
        }
        dDenTot = sqrt(fabs(dDen1*dDen2));
        if(dDenTot == 0)
        {
            cerr << "CosineDistance::distance: WARNING: dividing by zero in cosine dist\n";
            return false;
        }
	
        dist = 1-((dSum1)/dDenTot);
        return A3 = PlTerm(dist);
    }
}


//---------------------------------------------------------------
//---------- Taken from the qm-dsp library
//---------------------------------------------------------------

double distanceDistribution(vector<float> *d1, vector<float> *d2, bool symmetrised)
{
    int sz = d1->size();

    double d = 0;
    float small = 1e-20;
    
    for (int i = 0; i < sz; ++i) {
        d += (double)(d1->at(i) * log10((d1->at(i) + small) / (d2->at(i) + small)));
    }

    if (symmetrised) {
        d += distanceDistribution(d2, d1, false);
    }

    return d;
}

//------
//reads the data id and returns a double vector (soundbite type).
//------

int
smpl_get_data(term_t data, vector<double> *vec)
{
	char *id;//A1
	term_t id_t = PL_new_term_ref();
	id_t = data;
	PL_get_atom_chars(id_t,&id);

	//means
	vector<float> *tmp;
	BLOBID::get_data_for_id((const char*)id, tmp);
	for(size_t j=0; j<tmp->size() ; j++){
		vec->push_back((double)tmp->at(j));
	}
	return 0;	
}









