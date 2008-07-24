/**
	Support version machine-based genre classifier using vamp features

	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2008 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
*/

:-use_module('../swiweka/weka').
:-use_module('../swiweka/classification').
:-use_module('../swivamp/vamp_transform').
:-use_module('../swiaudio/audio').
:-use_module('../swidata/data').

%	Input is a list of records [song, [mffcmeans], [mfccvars], tag]	

svm_model(TrainData, TrainSet):-
	classification:create_classifier('weka.classifiers.functions.SVMreg', [], SVM),
	Options = ['-C 1.0', '-N 0', '-I weka.classifiers.functions.supportVector.RegSMOImproved', '-L 0.0010', '-W 1', '-P 1.0E-12', '-T 0.0010', '-N 0', '-K weka.classifiers.functions.supportVector.PolyKernel', '-C 250007', '-E 1.0', '-d /home/david/my.model'], 
	wkpl_set_options(SVM, Options),
	At = [numAt('C0mean'), numAt('C1mean'), numAt('C2mean'), numAt('C3mean'), numAt('C4mean'), numAt('C5mean'), numAt('C6mean'), numAt('C7mean'), numAt('C8mean'), numAt('C9mean'), numAt('C10mean'), numAt('C11mean'), numAt('C12mean'), numAt('C13mean'), numAt('C14mean'), numAt('C15mean'), numAt('C16mean'), numAt('C17mean'), numAt('C18mean'), numAt('C19mean'), numAt('C0var'), numAt('C1var'), numAt('C2var'), numAt('C3var'), numAt('C4var'), numAt('C5var'), numAt('C6var'), numAt('C7var'), numAt('C8var'), numAt('C9var'), numAt('C10var'), numAt('C11var'), numAt('C12var'), numAt('C13var'), numAt('C14var'), numAt('C15var'),numAt('C16var'), numAt('C17var'), numAt('C18var'), numAt('C19var'), numAt('tag')],
	SetID = 'genre',	
	%may need some checks about the class we set here
	create_classifSet(SetID, At, 40),
	save_classifSet('genre', '/home/david/my.arff'),
	classifSets_db('genre', TrainSet),
	findall(_,jamendo_train(TrainData, TrainSet),_),
	wkpl_write_arff('/home/david/train.arff', TrainSet),
	weka:wkpl_build_classifier(TrainSet, SVM).
	
	
jamendo_train(TrainData, TrainSet):-
	member(Record, TrainData),
	Record = [_Song, Lmeans, Lvars, _Tag],
	is_list(Lmeans),
	is_list(Lvars),
	length(Lmeans, 20),
	length(Lvars, 20),
	flatten(Record, Instance),	
	weka:wkpl_add_instance(TrainSet, Instance).
	
:-Song = '/home/david/Repository/Test.mp3',
	decode(Song, X), 
	mix_stereo(X, Y),
	transform(Y, 'libqm-vamp-plugins', 'qm-similarity', [means, variances], L), 
	L = [[feature(_, _, D1)], [feature(_, _, D2)]],
	svm_model([[Song, D1, D2, 'britpop']], P).
	
	
