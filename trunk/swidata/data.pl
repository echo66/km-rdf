/**
	
*/

:- module(data, [	reserve_id/1
		,	current_id/1
		,	next_id/1
		, 	blobids/1
		,	ids_status/3
		, 	busy_id/1
		,	is_blobid/1
		,	clean_blob/1
		,	blob_size/2
		,	blobs_mean/3
		,	equal_blobs/2
		,	blob_frame/4
		, 	concat_blobs/3	

/*FIXME*/
		,	plblob_blobid/2
		,	blobid_plblob/2
		,       register_blob/2
		,	register_data_list/2

			
		,	data/2,
			load_data_list/2,
			blob_to_file/2,
			file_to_blob/2,
			clean_data/1,
			data_in/2,
			data_out/2

			]).


:- style_check(-discontiguous).
:- load_foreign_library(swidata).
:- use_module(library(pldoc)).

/************************************** BLOBIDs DATABASE ***************************/

%% reserve_blobid(+BLOBID) is det
% reserves the passed id in the database so it can not be assigned to any outcoming blob unless we pass it for unification

reserve_id(Term):-
	atom(Term),
	reserve_data_id(Term).	

%% is_blobid(+Term) is det
% checks if the passed term is a BLOBID (an atom stored in the ids database). The id may be reserved but not pointing to any block of data

is_blobid(T):-
	atom(T),
	is_blobid(T, _).
	
%% busy_id(+Term) is det
% checks if the BLOBID is actually pointing to any block of data

busy_id(BLOBID):-
	active_id(BLOBID).

%% clean_blob(+BLOBID) is det
% deletes the data pointed by the ID from memory

clean_blob(T):-
	busy_id(T),
	clean_data(T).

%% blobids(+Number) is det
% Number of blobid in the database

blobids(L):-
	ids_in_system(C).

%% current_id(?BLOBID) is det
% Last id assigned by the system

current_id(I):-	
	current_blob_id(Id),
	I = Id.

%% next_id(?BLOBID) is det
% Next id to assign automatically by the system

next_id(I):-
	next_blob_id(ID),
	I = ID.

%% ids_status(-CurrentID, -NextID, -IDs)
% Shows the BLOBIDs status

ids_status(A, L, O):-
	current_id(A),
	next_id(L),
	blobids(O).
	
%% blob_size(+BLOBID, -Size) is det
% Returns the size of the blob

blob_size(T, L):-
	busy_id(T),
	get_blob_size(T, L).

%% blobs_mean(+BLOBID, +BLOBID2, -BLOBIDMEAN) is det
% Returns the mean of two blobs

blobs_mean(B1, B2, Mean):-
	blob_size(B1, L),
	blob_size(B2, L),
	mean_of_blobs(B1, B2, Mean).

%% equal_blobs(+BLOBID1, +BLOBID2) is det
% Tells if the blobs are equal

blobs_equal(B1, B2):-
	blob_size(B1, L),
	blob_size(B2, L),
	are_equal_blobs(B1, B2).

%% data_size(+DataObject, -Size) is det
% Returns the size of the data object (list of BLOBID)

data_size(O, L):-
	is_list(O),
	length(O, L).

data_size(O, L):-
	is_blobid(O),
	blob_size(O, L).

%% blob_frame(+BLOBID1, Start, Size, -BLOBIDFRAME) is det
% Returns a frame of the original blob

blob_frame(B, S, Si, F):-
	busy_id(B),
	get_frame_of_blob(B, S, Si, F).

%% concat_blobs(BLOBID1, BLOBID2, BLOBID3) is det
% Concats two blobs and returns the new one

concat_blobs(B1, B2, B3):-
	busy_id(B1),
	busy_id(B2),
	concat_of_blobs(B1, B2, B3).


