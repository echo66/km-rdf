/**
 *	-get the extension of an audio file
 *	
 */

#include <swiaudioblob.h>
#include <swimo.h>

#include <vector>
#include <qstring.h>
#include <iostream>

using namespace std;

				/****************************************************************************
				 ***************************************************************************/


/**
 */

PREDICATE(aspl_file_extension, 2)
{
	//+ Path to audioFile
	//- extension of the audio file

	QString filePath((char *)A1);
	QString ext = filePath.section(".",-1);
	return A2 = PlTerm(PlAtom(ext.toLocal8Bit().data()));

}



