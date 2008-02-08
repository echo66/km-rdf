#!/bin/bash

rm -rdf henry/
mkdir henry
mkdir henry/examples

cp -rd server.pl AUTHORS README n3_entailment.pl builtins.pl ../n3/n3_dcg.pl namespaces.pl swicwm.pl henry/

cp -rd examples/recur.n3 examples/family.n3 examples/list.n3 examples/nose-2.n3 examples/nose.n3 examples/uncle.n3 examples/sameas.n3 examples/list.sparql henry/examples/

tar czvf henry.tar.gz henry/

