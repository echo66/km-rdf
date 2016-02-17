# [HENRY](http://km-rdf.googlecode.com/files/henry.tar.gz) (N3 parser and reasoner for SWI-Prolog) #


## Install ##

  * Make sure you have [SWI-Prolog](http://www.swi-prolog.org/) installed.
  * Download [the latest stable version](http://km-rdf.googlecode.com/files/henry-1.01.tar.gz)
  * If you want to query your N3-entailed store using SPARQL, get SeRQL from:
> > http://e-culture.multimedian.nl/software/ClioPatria.shtml
> > And put the SeRQL directory in the Henry directory.

## Use ##

  * The swicwm.pl script behaves (almost) like the following command line, involving [CWM](http://www.w3.org/2000/10/swap/doc/cwm.html):


> `$ cwm $1 --think --rdf --data`

  * From the Prolog command-line, you can load henry.pl:

> `?- [henry].`

> You can then load some n3 files using:

> `?- n3_load('file.n3').`

> Or some RDF (XML or turtle) files using:

> `?- rdf_load('file.rdf').`

> Then, you can compile the N3 rules you have in store using:

> `?- compile_all.`

> Then, the derived statements are accessible through:

> `?- n3_entailment:rdf(Subject,Predicate,Object).`

> Or through the SPARQL end-point at http://localhost:3020/sparql (make sure you specify N3 as the entailment module)