/**
	AAC decoder using the FAAD library based on the sources package and the code of John Edwards.
 	
	Centre for Digital Music, Queen Mary, University of London.
	Copyright (C) 2007 David Pastor Escuredo and QMUL.

	This program is free software: you can redistribute it and/or modify
  	it under the terms of the GNU General Public License as published by
   	the Free Software Foundation, either version 3 of the License, or
    	(at your option) any later version.
	*/

#ifndef __FAADDECODE_H__
#define __FAADDECODE_H__

#include <stdio.h>
#include <mp4ff.h>
#include <vector>

/*
 * Decoding AAC data encoded in MP4 or AAC
 */

int 
fdpl_decode_MP4(const char *, size_t &, size_t &, size_t &, std::vector<float> *, std::vector<float> *);
int 
fdpl_decode_AAC(int, const char *, size_t &, size_t &, size_t &, std::vector<float> *, std::vector<float> *);

/* other functions */

int 
GetAACTrack(mp4ff_t *); 

uint32_t 
read_callback(void *, void *, uint32_t );

uint32_t 
seek_callback(void *, uint64_t );

int 
id3v2_tag(unsigned char *);

#endif 
