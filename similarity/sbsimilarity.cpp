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

//------------------------
//	Prototypes       -
//------------------------

int
smpl_parse_SBFeatures(PlTerm, vector<double> *, vector<double> *);//no se si los tengo q psar por valor o por referencia


						/*******************************************
						******** FOREIGN PREDICATES ****************
						*******************************************/

//--------------------------------------------------------------------------
// Gets the distance using mfcckldiv (taken from libsoundbite)
//	
//			Input is a list of features ['MFFCCMEAN', 'MFCCVAR'] which each of them has the syntax of any swivamp feature.
//			Returns a distance between the audiofiles with the features belong to
//			This features come from the similarity vamp plugin. The means and variances can be also extracted from the chromagram.
//
//--------------------------------------------------------------------------

PREDICATE(smpl_mfcckldiv, 3){

	//+Feature List of one file
	//+Feature List of the other file
	//-Distance (double)

	//We get the feature List which contains the MFCC mean and the MFFCC variance for each file and we convert them into vector<double>

	//Audiofile 1
	vector<double> *mfccmean_1;
	vector<double> *mfccvar_1;
	mfccmean_1 = new vector<double>();
	mfccvar_1 = new vector<double>();

	if(smpl_parse_SBFeatures(PlTerm(A1), mfccmean_1, mfccvar_1)<0){
		return false;
	}

	//Audiofile 2
	vector<double> *mfccmean_2;
	vector<double> *mfccvar_2;
	mfccmean_2 = new vector<double>();
	mfccvar_2 = new vector<double>();

	if(smpl_parse_SBFeatures(PlTerm(A2), mfccmean_2, mfccvar_2)<0){
		return false;
	}
	
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

	return A3 = PlTerm(d);
}


						/**********************
						**** FUNCTIONS ********
						**********************/

//--------------------------------------------------------------------------------------------------------------------------------
//	
//    fv is a list of features. The first one must be the mean feature elements list and the second another 
//    list with the variance elements
//
//    swi km features should look like:
//					'Feature'(type, MO::timestamp, '__data_id').
//
//	Returns: 
//		-1 if the terms passed are not features
//--------------------------------------------------------------------------------------------------------------------------------

//TO DO: Extend this if we wont just to pass blobs instead of the whole feature compound!!!!!!

int
smpl_parse_SBFeatures(PlTerm fv, vector<double> *mean, vector<double> *var){

	//fv is a list of vamp features. The first one must be the mean feature elements list and the second another list with the variance elements.

	//Retrieving features list and parse them

	PlTail tail(fv);
  	PlTerm list;
  	while(tail.next(list)){
		//supposed to have just two lists
	
		//Treat like mean
		PlTail subtail(list);
		PlTerm feature;
	
		while(subtail.next(feature)){
			//Checking the type and storing the value

			cerr<<feature.name()<<endl;

			//Analyze the Compound
			if(feature.name()=="Feature"){
				return -1;
			}
			PlAtom type(feature[0]);

			//need to check this from the plugin.
			if(type=="means"){
			
				vector<float> *data;
				DataID::get_data_for_id((const char*)(char *)feature[2], data);
				cerr<<data->size()<<endl;
				mean->push_back((double)data->at(0));//should be just one value in this feature type.
				return 0;
			}
			else if(type=="variances"){
				
				vector<float> *data;
				DataID::get_data_for_id((const char*)(char *)feature[2], data);
				cerr<<data->size()<<endl;
				mean->push_back((double)data->at(0));//should be just one value in this feature type.
				return 0;
			}
			else{
				cerr<<"not valid feature type"<<endl;
				return -2;
			}
		}
	}
	return 0;
}









