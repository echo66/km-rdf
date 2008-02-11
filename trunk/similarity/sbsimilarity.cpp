/**
	This is the SoundBite similarity mechanism applied for the KM equivalent to SBFeatureVector. This module returns an statistic distance which 
	allow the creation of playlists.
	We use the mean and variance of a the MFCC's to extract the distance using (also from chromagram).
	David Pastor Escuredo, Jan 2008, c4dm, Queen Mary.
	*/

/**
	Let's remember how a swivamp feature looks like:
	
		'Feature'(type, MO::timestamp, '__data_id').

	*/

#ifndef _FLI_H_INCLUDED
#include <SWI-cpp.h>
#endif

#include <swimo.h>
#include <vector>
#include <blobid.h>

using namespace std;

int
smpl_get_data(term_t, vector<double> *);

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

PREDICATE(smpl_mfcckldiv, 5){

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
	DataID::get_data_for_id((const char*)id, tmp);
	for(size_t j=0; j<tmp->size() ; j++){
		vec->push_back((double)tmp->at(j));
	}
	return 0;	
}









