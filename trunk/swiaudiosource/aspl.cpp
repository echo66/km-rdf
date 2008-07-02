/** * Foreign interface with predicates useful in the audiosource module. Basically this does:
 *	-get the extension of an audio file
 *	-obtains an incremental start for framing given the parameters
 *	David Pastor 2008
 */

#include <swiaudioblob.h>
#include <swimo.h>

#include <vector>
#include <qstring.h>
#include <iostream>

using namespace std;

				/****************************************************************************				 * Prolog predicates used in the swiaudiosource module (C++/Prolog interface)
				 ***************************************************************************/


/** * Extracts the file extension from the file path to the audio file
 */

PREDICATE(aspl_file_extension, 2)
{
	//+ Path to audioFile
	//- extension of the audio file

	QString filePath((char *)A1);
	QString ext = filePath.section(".",-1);
	return A2 = PlTerm(PlAtom(ext.toLocal8Bit().data()));

}

/
/*
 * This predicate calculates the number of frames that will be retrieved in the framing for the specific signal and the StepSize of the framing process
 */

PREDICATE(set_limit_framing, 3)
{
	//+samples per channel
	//+StepSize
	//-Limit
	
	long limit = long((long)A1/(long)A2);
	return A3 = PlTerm(limit);

}


