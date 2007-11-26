/*
 *
 * This program is distributed under the GNU General Public License, version 2.
 * A copy of this license is included with this source.
 *
 * Copyright (C) 2002 John Edwards
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
